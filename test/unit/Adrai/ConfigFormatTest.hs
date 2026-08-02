{-# LANGUAGE OverloadedStrings #-}

module Adrai.ConfigFormatTest (tests) where

import Adrai.Format.Config
import Adrai.Types
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "strict configuration format"
    [ canonicalDefaultTests,
      roundTripAndDefaultTests,
      strictShapeTests,
      domainValidationTests,
      escapingTests
    ]

canonicalDefaultTests :: TestTree
canonicalDefaultTests =
  testGroup
    "canonical default"
    [ testCase "default text has exact bytes and final LF" $ do
        defaultConfigText @?= expectedDefaultText
        BS.length (TE.encodeUtf8 defaultConfigText) @?= 239
        T.isSuffixOf "\n" defaultConfigText @?= True,
      testCase "default text has the immutable SHA-256" $
        sha256Text (TE.encodeUtf8 defaultConfigText)
          @?= "634db372b5ddf19440951835ed203d310bc5f18edbc696497e02f143254b2eea",
      testCase "default text parses to defaultConfig" $
        parseConfigText defaultConfigText @?= Right defaultConfig
    ]

roundTripAndDefaultTests :: TestTree
roundTripAndDefaultTests =
  testGroup
    "round trips and independent defaults"
    [ testCase "render and parse round trip" $
        parseConfigText (renderConfig defaultConfig) @?= Right defaultConfig,
      testCase "absent paths and lines default" $
        parseConfigText "schema = 1\n" @?= Right defaultConfig,
      testCase "an absent decision path defaults independently" $
        case parseConfigText "schema = 1\n[paths]\nconnections = \"relations\"\n" of
          Left problem -> assertFailure (show problem)
          Right config -> do
            repoPathText (managedDecisionPath (configManagedPaths config))
              @?= "architecture/adrai/decisions"
            repoPathText (managedConnectionPath (configManagedPaths config)) @?= "relations",
      testCase "an absent connection path defaults independently" $
        case parseConfigText "schema = 1\n[paths]\ndecisions = \"adr\"\n" of
          Left problem -> assertFailure (show problem)
          Right config -> do
            repoPathText (managedDecisionPath (configManagedPaths config)) @?= "adr"
            repoPathText (managedConnectionPath (configManagedPaths config))
              @?= "architecture/adrai/connections",
      testCase "an explicit empty line array defaults" $
        parseConfigText "schema = 1\nline = []\n" @?= Right defaultConfig
    ]

strictShapeTests :: TestTree
strictShapeTests =
  testGroup
    "closed tables and strict types"
    [ testCase "schema is required and is exactly integer 1" $ do
        assertRejected ""
        assertRejected "schema = \"1\"\n"
        assertRejected "schema = 1.0\n"
        assertRejected "schema = 2\n",
      testCase "root, path, and line keys are closed" $ do
        assertRejected "schema = 1\nextra = true\n"
        assertRejected "schema = 1\n[paths]\nextra = \"x\"\n"
        assertRejected (oneLine "extra = true"),
      testCase "paths and line fields have exact TOML types" $ do
        assertRejected "schema = 1\npaths = []\n"
        assertRejected "schema = 1\n[paths]\ndecisions = 7\n"
        assertRejected (oneLine "id = 7")
        assertRejected "schema = 1\n\n[[line]]\nid = \"trunk\"\nrefs = [7]\n",
      testCase "TOML duplicate assignments are rejected" $
        assertRejected "schema = 1\nschema = 1\n"
    ]

domainValidationTests :: TestTree
domainValidationTests =
  testGroup
    "domain constructors"
    [ testCase "managed paths must be safe and non-overlapping" $ do
        assertRejected "schema = 1\n[paths]\ndecisions = \"../adr\"\n"
        assertRejected
          "schema = 1\n[paths]\ndecisions = \"architecture\"\nconnections = \"architecture/connections\"\n",
      testCase "line ids and refs use strict existing constructors" $ do
        assertRejected (lineDocument "UPPER" "[\"refs/heads/main\"]")
        assertRejected (lineDocument "trunk" "[]")
        assertRejected (lineDocument "trunk" "[\"main\"]")
        assertRejected
          (lineDocument "trunk" "[\"refs/heads/main\", \"refs/heads/main\"]"),
      testCase "logical line ids are unique" $
        assertRejected
          ( "schema = 1\n\n[[line]]\nid = \"trunk\"\nrefs = [\"refs/heads/main\"]\n\n"
              <> "[[line]]\nid = \"trunk\"\nrefs = [\"refs/heads/other\"]\n"
          )
    ]

escapingTests :: TestTree
escapingTests =
  testGroup
    "escaping"
    [ testCase "rendered Unicode paths round trip canonically" $
        case customConfig "architecture/beslutning-\x00f8" "relations" of
          Left problem -> assertFailure problem
          Right config -> do
            assertBool "Unicode path must be preserved" ("beslutning-\x00f8" `T.isInfixOf` renderConfig config)
            parseConfigText (renderConfig config) @?= Right config,
      testCase "TOML 1.1-only basic-string escapes are rejected" $ do
        assertRejected "schema = 1\n[paths]\ndecisions = \"architecture\\x2Fadrai\"\n"
        assertRejected "schema = 1\n[paths]\ndecisions = \"architecture\\eadrai\"\n",
      testCase "TOML 1.1 multiline inline tables are rejected exactly" $
        parseConfigText
          "schema = 1\npaths = {\n  decisions = \"adr\",\n  connections = \"relations\"\n}\n"
          @?= Left
            (ConfigToml11Syntax "TOML 1.1 multiline inline tables are not supported"),
      testCase "TOML 1.1 trailing inline-table commas are rejected exactly" $ do
        parseConfigText
          "schema = 1\npaths = { decisions = \"adr\", connections = \"relations\", }\n"
          @?= Left
            (ConfigToml11Syntax "TOML 1.1 trailing commas in inline tables are not supported")
        parseConfigText
          "schema = 1\nline = [{ id = \"trunk\", refs = [\"refs/heads/main\"], }]\n"
          @?= Left
            (ConfigToml11Syntax "TOML 1.1 trailing commas in inline tables are not supported"),
      testCase "valid TOML 1.0 inline tables and literal strings parse" $
        case customConfig "adr" "relations" of
          Left problem -> assertFailure problem
          Right expected ->
            parseConfigText
              ( "schema = 1\n"
                  <> "paths = { decisions = 'adr', connections = 'relations' }\n"
                  <> "line = [{ id = 'trunk', refs = ['refs/heads/main'] }]\n"
              )
              @?= Right expected,
      testCase "a closed one-line inline table does not taint following lines" $
        parseConfigText
          ( "schema = 1\n"
              <> "paths = { decisions = 'architecture/adrai/decisions' }\n"
              <> "# the closed frame must not treat this newline as inline-table content\n"
          )
          @?= Right defaultConfig,
      testCase "multiline arrays remain valid inside an inline table value" $
        case customConfig "adr" "relations" of
          Left problem -> assertFailure problem
          Right expected ->
            parseConfigText
              ( "schema = 1\n"
                  <> "paths = { decisions = 'adr', connections = 'relations' }\n"
                  <> "line = [{ id = 'trunk', refs = [\n"
                  <> "  'refs/heads/main',\n"
                  <> "] }]\n"
              )
              @?= Right expected,
      testCase "TOML 1.1 escape text in comments is harmless" $
        parseConfigText "# \\e and \\x2F are provenance text\nschema = 1\n"
          @?= Right defaultConfig
    ]

assertRejected :: Text -> IO ()
assertRejected input =
  assertBool ("expected rejection for:\n" <> T.unpack input) (isLeft (parseConfigText input))

oneLine :: Text -> Text
oneLine replacement =
  "schema = 1\n\n[[line]]\n"
    <> replacement
    <> "\nrefs = [\"refs/heads/main\"]\n"

lineDocument :: Text -> Text -> Text
lineDocument identifier refs =
  "schema = 1\n\n[[line]]\nid = \""
    <> identifier
    <> "\"\nrefs = "
    <> refs
    <> "\n"

customConfig :: Text -> Text -> Either String Config
customConfig decisionsText connectionsText = do
  decisions <- mapShow (mkRepoPath decisionsText)
  connections <- mapShow (mkRepoPath connectionsText)
  paths <- mapShow (mkManagedPaths decisions connections)
  ref <- mapShow (mkGitRef "refs/heads/main")
  logicalLine <- mapShow (mkLogicalLine "trunk" [ref])
  mapShow (mkConfig ConfigSchemaV1 paths [logicalLine])

mapShow :: (Show problem) => Either problem value -> Either String value
mapShow value =
  case value of
    Left problem -> Left (show problem)
    Right result -> Right result

sha256Text :: BS.ByteString -> Text
sha256Text bytes = T.pack (show (Crypto.hash bytes :: Crypto.Digest Crypto.SHA256))

expectedDefaultText :: Text
expectedDefaultText =
  T.unlines
    [ "schema = 1",
      "",
      "[paths]",
      "decisions = \"architecture/adrai/decisions\"",
      "connections = \"architecture/adrai/connections\"",
      "",
      "[[line]]",
      "id = \"trunk\"",
      "refs = [\"refs/heads/main\", \"refs/remotes/origin/main\", \"refs/heads/master\", \"refs/remotes/origin/master\"]"
    ]
