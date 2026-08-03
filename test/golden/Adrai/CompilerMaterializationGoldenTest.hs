{-# LANGUAGE OverloadedStrings #-}

module Adrai.CompilerMaterializationGoldenTest (tests, writeP303Goldens) where

import Adrai.Compiler
import Adrai.Property.Generators
import Adrai.Retrieval
import Adrai.Sqlite
import Adrai.Types (adrIdText, recordIdText, stateTokenText)
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Numeric (showFFloat)
import System.Directory (createDirectoryIfMissing)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)

tests :: TestTree
tests =
  testGroup
    "P3-03 Haskell-owned materialization goldens"
    [ testCase "current logical rows and passage identities are frozen" (assertGolden "materialization.golden" materializationBytes),
      testCase "ordinary and six FTS schemas are frozen" (assertGolden "search-schema.golden" schemaBytes)
    ]

assertGolden :: FilePath -> ByteString -> IO ()
assertGolden name actual = BS.readFile (goldenRoot <> "/" <> name) >>= (@?= actual)

materializationBytes :: ByteString
materializationBytes = TextEncoding.encodeUtf8 (Text.unlines (header <> documentLines <> passageLines <> aliasLines))
  where
    fixture = materializeProjectionDag fixedProjectionSpec
    materialization = mustRight (materializeCurrentSearch (projectionFixtureAfter fixture))
    header = ["fingerprint=" <> materializationImplementationFingerprint]
    documentLines = map renderDocument (searchMaterializationDocuments materialization)
    passageLines = map renderPassage (searchMaterializationPassages materialization)
    aliasLines =
      case searchMaterializationAliases materialization of
        [] -> error "P3-03 golden fixture must exercise deterministic first-wins aliases"
        aliases -> ["alias|" <> escaped alias <> "|" <> escaped expansion | (alias, expansion) <- aliases]

renderDocument :: SearchDocument -> Text
renderDocument document =
  Text.intercalate
    "|"
    [ "document",
      searchDocumentItemId document,
      adrIdText (searchDocumentAdrId document),
      recordIdText (searchDocumentCandidateRecordId document),
      escaped (searchDocumentTitle document),
      escaped (searchDocumentSummary document),
      escaped (searchDocumentContext document),
      escaped (searchDocumentDecision document),
      escaped (searchDocumentConsequences document),
      escaped (Text.intercalate "\n" (searchDocumentDomains document)),
      escaped (searchDocumentRationale document),
      escaped (searchDocumentIdentifierSource document),
      escaped (searchDocumentIdentifiers document),
      escaped (searchDocumentOther document),
      escaped (Text.intercalate "\n" (searchDocumentScope document)),
      boolText (searchDocumentObsolete document),
      boolText (searchDocumentConflicted document),
      stateTokenText (searchDocumentStateToken document),
      escaped (Text.intercalate "\n" (searchDocumentSourcePaths document))
    ]

renderPassage :: SearchPassage -> Text
renderPassage passage =
  Text.intercalate
    "|"
    [ "passage",
      searchPassageId passage,
      searchPassageDocumentItemId passage,
      adrIdText (searchPassageAdrId passage),
      recordIdText (searchPassageCandidateRecordId passage),
      sectionKindName (searchPassageSectionKind passage),
      Text.pack (show (searchPassageOrdinal passage)),
      Text.pack (show (searchPassageLineStart passage)) <> "-" <> Text.pack (show (searchPassageLineEnd passage)),
      Text.pack (showFFloat Nothing (searchPassageWeight passage) ""),
      escaped (searchPassageText passage),
      escaped (Text.intercalate "\n" (searchPassageSourcePaths passage)),
      escaped (searchPassageIdentifiers passage)
    ]

schemaBytes :: ByteString
schemaBytes = TextEncoding.encodeUtf8 (Text.unlines (ordinary <> fts))
  where
    ordinary = ["ordinary|" <> Text.pack (show component) <> "|" <> ddl | (component, ddl) <- searchOrdinarySchemaDdl]
    fts = ["fts|" <> ftsTargetName target <> "|" <> ftsTargetDdl target | target <- allFtsTargets]

fixedProjectionSpec :: ProjectionDagSpec
fixedProjectionSpec = ProjectionDagSpec (IdentifierPool 0) (DomainSpec ["compiler", "cache"]) (ScopeSpec ["src"] True) "Cache Compiler API (CCAPI)"

escaped :: Text -> Text
escaped = Text.replace "\n" "\\n" . Text.replace "|" "\\|"

boolText :: Bool -> Text
boolText value = if value then "true" else "false"

writeP303Goldens :: IO ()
writeP303Goldens = do
  createDirectoryIfMissing True goldenRoot
  BS.writeFile (goldenRoot <> "/materialization.golden") materializationBytes
  BS.writeFile (goldenRoot <> "/search-schema.golden") schemaBytes

goldenRoot :: FilePath
goldenRoot = "test/golden/p3-03"

mustRight :: (Show failure) => Either failure value -> value
mustRight (Right value) = value
mustRight (Left failure) = error (show failure)
