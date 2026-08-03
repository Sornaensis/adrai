{-# LANGUAGE OverloadedStrings #-}

module Adrai.VectorQualityTest (tests, writeP301Goldens) where

import Adrai.Format (renderDigest)
import Adrai.Provenance (sha256Digest)
import Adrai.Vector
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.IO as TextIO
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

tests :: TestTree
tests =
  testGroup
    "P3-01 Haskell-owned vector quality goldens"
    [ testCase "semantic memoized-compiler float32 bytes are frozen" semanticBinaryGolden,
      testCase "identifier DropwireLease float32 bytes are frozen" identifierBinaryGolden,
      testCase "vector contract manifest is frozen" contractGolden,
      testCase "architecture synonyms rank closer than unrelated text" (assertComparison 0 "memoized compiler query must favor cache reuse"),
      testCase "identifier parts and lease tokens are contextually useful" $ do
        assertComparison 1 "Dropwire environment identifiers must favor runtime configuration"
        assertComparison 2 "lease-token source must favor worker delivery"
    ]

semanticGoldenInput, identifierGoldenInput :: Text
semanticGoldenInput = "memoized compiler"
identifierGoldenInput = "DropwireLease"

semanticGoldenBytes, identifierGoldenBytes :: ByteString
semanticGoldenBytes = packVector (semanticEmbedding semanticGoldenInput)
identifierGoldenBytes = packVector (identifierEmbedding identifierGoldenInput)

semanticBinaryGolden :: IO ()
semanticBinaryGolden = do
  expected <- BS.readFile (goldenRoot <> "/semantic-memoized-compiler.f32le")
  BS.length expected @?= 4096
  semanticGoldenBytes @?= expected

identifierBinaryGolden :: IO ()
identifierBinaryGolden = do
  expected <- BS.readFile (goldenRoot <> "/identifier-dropwire-lease.f32le")
  BS.length expected @?= 3072
  identifierGoldenBytes @?= expected

contractGolden :: IO ()
contractGolden = do
  expected <- BS.readFile (goldenRoot <> "/vector-contract.golden")
  vectorContractBytes @?= expected

assertComparison :: Int -> String -> IO ()
assertComparison index message = case drop index qualityComparisons of
  comparison : _ -> assertBool message (comparisonPass comparison)
  [] -> assertBool "quality-comparison index is in range" False

data QualityComparison = QualityComparison
  { comparisonName :: Text,
    comparisonPass :: Bool
  }

qualityComparisons :: [QualityComparison]
qualityComparisons =
  [ compareSemantic
      "quality_architecture_synonyms"
      "memoized artifact fingerprint"
      "cache identity uses content digest keys"
      "service authorization rotates credentials",
    compareSemantic
      "quality_environment_identifier"
      "DROPWIRE_DATABASE DROPWIRE_WEBHOOK_TOKEN healthcheck readiness"
      "runtime configuration environment variables database and health readiness"
      "fixed interval polling sleeps while idle",
    compareSemantic
      "quality_lease_delivery"
      "lease_token expires; acknowledge remote delivery only after commit"
      "worker claims a row with a lease token and acknowledges after successful delivery"
      "bearer credentials and webhook secrets come from the process environment"
  ]

compareSemantic :: Text -> Text -> Text -> Text -> QualityComparison
compareSemantic name query preferred unrelated =
  QualityComparison
    name
    (similarity query preferred > similarity query unrelated)
  where
    similarity left right = mustRight (dot (semanticEmbedding left) (semanticEmbedding right))

vectorContractBytes :: ByteString
vectorContractBytes = TextEncoding.encodeUtf8 (renderContract <> "\n")
  where
    plan = mustRight (buildLshPlan semanticVectorDimensions (embedderVectorId semanticEmbedder))
    signatures = mustRight (lshSignatures plan (semanticEmbedding semanticGoldenInput))
    firstProbes = case signatures of
      [] -> []
      first : _ -> lshProbeBuckets first
    renderContract =
      "{"
        <> Text.intercalate
          ","
          ( [ field "blake2b128_empty" "cae66941d9efbd404e4d88758ea67670",
              integerField "identifier_bytes" (BS.length identifierGoldenBytes),
              field "identifier_fingerprint" (embedderFingerprint identifierEmbedder),
              field "identifier_input" identifierGoldenInput,
              field "identifier_sha256" (digest identifierGoldenBytes),
              field "identifier_vector_id" (embedderVectorId identifierEmbedder)
            ]
              <> map comparisonField qualityComparisons
              <> [ integerField "semantic_bytes" (BS.length semanticGoldenBytes),
                   field "semantic_fingerprint" (embedderFingerprint semanticEmbedder),
                   field "semantic_input" semanticGoldenInput,
                   bucketsField "semantic_probes_band0" firstProbes,
                   field "semantic_sha256" (digest semanticGoldenBytes),
                   bucketsField "semantic_signatures" signatures,
                   field "semantic_vector_id" (embedderVectorId semanticEmbedder),
                   field "vector_implementation_fingerprint" implementationFingerprint
                 ]
          )
        <> "}"
    field key value = quote key <> ":" <> quote value
    integerField key value = quote key <> ":" <> Text.pack (show value)
    booleanField key value = quote key <> if value then ":true" else ":false"
    comparisonField comparison = booleanField (comparisonName comparison) (comparisonPass comparison)
    bucketsField key values = quote key <> ":[" <> Text.intercalate "," [Text.pack (show bucket) | LshBucket bucket <- values] <> "]"
    quote value = "\"" <> value <> "\""

digest :: ByteString -> Text
digest = renderDigest . sha256Digest

-- | Explicit, Haskell-only acceptance hook. Ordinary test execution never calls
-- this function and therefore cannot silently update expected artifacts.
writeP301Goldens :: IO ()
writeP301Goldens = do
  createDirectoryIfMissing True goldenRoot
  BS.writeFile (goldenRoot <> "/semantic-memoized-compiler.f32le") semanticGoldenBytes
  BS.writeFile (goldenRoot <> "/identifier-dropwire-lease.f32le") identifierGoldenBytes
  BS.writeFile (goldenRoot <> "/vector-contract.golden") vectorContractBytes
  TextIO.putStrLn ("semantic-memoized-compiler.f32le " <> digest semanticGoldenBytes)
  TextIO.putStrLn ("identifier-dropwire-lease.f32le " <> digest identifierGoldenBytes)
  TextIO.putStrLn ("vector-contract.golden " <> digest vectorContractBytes)

goldenRoot :: FilePath
goldenRoot = "test/golden/p3-01"

mustRight :: (Show failure) => Either failure value -> value
mustRight (Right value) = value
mustRight (Left failure) = error (show failure)
