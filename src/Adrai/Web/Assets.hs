{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fforce-recomp #-}

-- | Checked bytes embedded into the executable. There is no filesystem fallback.
module Adrai.Web.Assets
  ( indexHtml,
    applicationCss,
    applicationJavaScript,
    assetProvenance,
  )
where

import Control.Monad (forM, forM_, unless, when)
import Crypto.Hash (Digest, SHA256, hash)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Language.Haskell.TH.Syntax as TH
import System.Directory (doesDirectoryExist, doesFileExist, listDirectory, pathIsSymbolicLink)
import System.FilePath ((</>))

indexHtml :: ByteString
applicationCss :: ByteString
applicationJavaScript :: ByteString
assetProvenance :: ByteString
(indexHtml, applicationCss, applicationJavaScript, assetProvenance) =
  $(do
      let required =
            [ "web/elm.json",
              "web/package.json",
              "web/package-lock.json",
              "web/tools/build.mjs",
              "web/tools/verify-assets.mjs",
              "web/static/bridge.js",
              "web/static/index.html",
              "web/static/app.css"
            ]
          safeEntry name =
            name /= "."
              && name /= ".."
              && not (any (`elem` ("/\\" :: String)) name)
          walk directory = do
            names <- sort <$> listDirectory directory
            fmap concat $ forM names $ \name -> do
              unless (safeEntry name) (fail ("Unsafe web source entry: " ++ name))
              let path = directory </> name
              linked <- pathIsSymbolicLink path
              when linked (fail ("Symlink in web source: " ++ path))
              directoryEntry <- doesDirectoryExist path
              if directoryEntry
                then walk path
                else do
                  regular <- doesFileExist path
                  unless regular (fail ("Nonregular web source: " ++ path))
                  pure [map (\c -> if c == '\\' then '/' else c) path]
          failReceipt message = fail ("Web asset provenance is stale or invalid: " ++ message)
          objectAt label value = case value of
            Aeson.Object object -> pure object
            _ -> failReceipt (label ++ " must be an object")
          textAt label object key =
            case KeyMap.lookup (Key.fromString key) object of
              Just (Aeson.String value) -> pure value
              _ -> failReceipt (label ++ "." ++ key ++ " must be a string")
          nestedAt label object key =
            case KeyMap.lookup (Key.fromString key) object of
              Just value -> objectAt (label ++ "." ++ key) value
              Nothing -> failReceipt (label ++ "." ++ key ++ " is missing")
          keys object = sort (map (Text.unpack . Key.toText) (KeyMap.keys object))
          exactKeys label expected object =
            unless (keys object == sort expected) (failReceipt (label ++ " has an unexpected field set"))
          sha bytes = Text.pack (show (hash bytes :: Digest SHA256))
      sourceExists <- TH.runIO (doesDirectoryExist "web/src")
      unless sourceExists (failReceipt "web/src is missing")
      sourcePaths <- TH.runIO (walk "web/src")
      let paths = sort (required ++ sourcePaths)
          receiptKey = drop (length ("web/" :: String))
          receiptPaths = sort (map receiptKey paths)
      unless (length paths == Map.size (Map.fromList (zip paths (repeat ()))))
        (failReceipt "duplicate input path")
      forM_ (paths ++ ["web/dist/app.js", "web/dist/provenance.json"]) TH.addDependentFile
      entries <- forM paths $ \path -> do
        linked <- TH.runIO (pathIsSymbolicLink path)
        when linked (failReceipt ("symlink input: " ++ path))
        regular <- TH.runIO (doesFileExist path)
        unless regular (failReceipt ("missing input: " ++ path))
        bytes <- TH.runIO (BS.readFile path)
        pure (path, bytes)
      let captured = Map.fromList entries
      receiptBytes <- TH.runIO (BS.readFile "web/dist/provenance.json")
      bundleBytes <- TH.runIO (BS.readFile "web/dist/app.js")
      receipt <- either failReceipt pure (Aeson.eitherDecodeStrict' receiptBytes)
      receiptObject <- objectAt "receipt" receipt
      exactKeys "receipt" ["schema", "inputs", "toolchain", "output"] receiptObject
      schema <- textAt "receipt" receiptObject "schema"
      unless (schema == "adrai/assets/v1") (failReceipt "unknown schema")
      inputs <- nestedAt "receipt" receiptObject "inputs"
      unless (keys inputs == receiptPaths) (failReceipt "input path set changed")
      forM_ entries $ \(path, bytes) -> do
        recorded <- textAt "inputs" inputs (receiptKey path)
        unless (recorded == sha bytes) (failReceipt ("input hash changed: " ++ path))
      tools <- nestedAt "receipt" receiptObject "toolchain"
      exactKeys "toolchain" ["node", "npm", "elm"] tools
      nodeVersion <- textAt "toolchain" tools "node"
      npmVersion <- textAt "toolchain" tools "npm"
      elmVersion <- textAt "toolchain" tools "elm"
      unless (nodeVersion == "24.15.0" && npmVersion == "npm@11.12.1" && elmVersion == "0.19.2")
        (failReceipt "toolchain identity changed")
      packageValue <- either failReceipt pure (Aeson.eitherDecodeStrict' (captured Map.! "web/package.json"))
      packageObject <- objectAt "package.json" packageValue
      engineObject <- nestedAt "package.json" packageObject "engines"
      devObject <- nestedAt "package.json" packageObject "devDependencies"
      packageManager <- textAt "package.json" packageObject "packageManager"
      pinnedNode <- textAt "package.json.engines" engineObject "node"
      pinnedElm <- textAt "package.json.devDependencies" devObject "elm"
      unless (packageManager == npmVersion && pinnedNode == nodeVersion && pinnedElm == "0.19.2-0")
        (failReceipt "package toolchain pins changed")
      elmValue <- either failReceipt pure (Aeson.eitherDecodeStrict' (captured Map.! "web/elm.json"))
      elmObject <- objectAt "elm.json" elmValue
      manifestVersion <- textAt "elm.json" elmObject "elm-version"
      unless (manifestVersion == elmVersion) (failReceipt "Elm manifest compiler version changed")
      output <- nestedAt "receipt" receiptObject "output"
      exactKeys "output" ["dist/app.js"] output
      recordedBundle <- textAt "output" output "dist/app.js"
      unless (recordedBundle == sha bundleBytes) (failReceipt "bundle hash changed")
      indexExpression <- TH.lift (captured Map.! "web/static/index.html")
      cssExpression <- TH.lift (captured Map.! "web/static/app.css")
      appExpression <- TH.lift bundleBytes
      receiptExpression <- TH.lift receiptBytes
      pure (TH.TupE (map Just [indexExpression, cssExpression, appExpression, receiptExpression]))
   )
