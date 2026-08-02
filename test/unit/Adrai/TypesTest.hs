{-# LANGUAGE OverloadedStrings #-}

module Adrai.TypesTest (tests) where

import Adrai.Types
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Foldable (traverse_)
import qualified Data.Text as T
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "typed foundations"
    [ identifierTests,
      prefixTests,
      repoPathTests,
      digestAndTokenTests,
      exitAndConfigTests,
      sharedRecordTests
    ]

identifierTests :: TestTree
identifierTests =
  testGroup
    "identifiers"
    [ testCase "accepts canonical A/R/C/O identifiers" $ do
        fmap adrIdText (mkAdrId adrText) @?= Right adrText
        fmap recordIdText (mkRecordId recordText) @?= Right recordText
        fmap connectionIdText (mkConnectionId connectionText) @?= Right connectionText
        fmap operationIdText (mkOperationId operationText) @?= Right operationText,
      testCase "rejects lowercase, wrong-kind, short, and forbidden payloads" $ do
        assertBool "lowercase full ID" (isLeft (mkAdrId (T.toLower adrText)))
        mkAdrId recordText @?= Left (IdWrongKind 'R')
        mkAdrId "A0123" @?= Left (IdWrongLength 5)
        mkAdrId "A0123456789ABCDEFGHIKMNPQRS" @?= Left (IdInvalidPayloadCharacter 'I')
    ]

prefixTests :: TestTree
prefixTests =
  testGroup
    "public prefixes"
    [ testCase "trims and case-normalizes the shortest public prefix" $
        fmap (\prefix -> (idPrefixKind prefix, idPrefixText prefix)) (mkIdPrefix "  a0123456  ")
          @?= Right (AdrObject, "A0123456"),
      testCase "rejects too-short and non-public operation prefixes" $ do
        mkIdPrefix "A012345" @?= Left (IdPrefixWrongLength 7)
        mkIdPrefix "O0123456" @?= Left (IdPrefixUnsupportedKind 'O'),
      testCase "returns not found for a valid unmatched prefix" $
        case (mkIdPrefix "A9999999", mkAdrRef adrText) of
          (Right prefix, Right candidate) ->
            resolveIdPrefix prefix [candidate] @?= Left (PrefixNotFound prefix)
          other -> assertFailure ("fixture construction failed: " <> show other),
      testCase "ambiguity candidates are deduplicated and stably sorted" $
        case traverse mkAdrRef [adrTextB, adrText, adrTextB] of
          Left problem -> assertFailure ("fixture construction failed: " <> show problem)
          Right [later, earlier, duplicate] ->
            case mkIdPrefix "A0123456" of
              Left problem -> assertFailure (show problem)
              Right prefix ->
                resolveIdPrefix prefix [later, earlier, duplicate]
                  @?= Left (PrefixAmbiguous prefix [earlier, later])
          Right unexpected -> assertFailure ("unexpected fixture count: " <> show unexpected),
      testCase "kind filtering produces a unique match" $
        case (mkAdrRef adrText, mkRecordRef recordText) of
          (Right adr, Right record) -> resolveObjectRef "a0123456" [record, adr] @?= Right adr
          other -> assertFailure ("fixture construction failed: " <> show other)
    ]

repoPathTests :: TestTree
repoPathTests =
  testGroup
    "repository paths"
    [ testCase "preserves a safe Unicode repository path" $
        fmap repoPathText (mkRepoPath "architecture/beslutning-ø/日本語.md")
          @?= Right "architecture/beslutning-ø/日本語.md",
      testCase "rejects every unsafe path class" $ do
        let unsafe =
              [ ("", RepoPathEmpty),
                ("/absolute", RepoPathAbsolute),
                ("C:/drive", RepoPathDriveQualified),
                ("//server/share", RepoPathUnc),
                ("a\\b", RepoPathBackslash),
                ("a//b", RepoPathEmptySegment),
                ("a/./b", RepoPathDotSegment),
                ("a/../b", RepoPathParentSegment),
                ("a/.GiT/b", RepoPathGitSegment),
                ("a/\n/b", RepoPathControlCharacter)
              ]
        traverse_ (\(input, expected) -> mkRepoPath input @?= Left expected) unsafe
    ]

digestAndTokenTests :: TestTree
digestAndTokenTests =
  testGroup
    "content identities"
    [ testCase "digest is exactly 32 bytes" $ do
        fmap digestBytes (mkDigest (BS.replicate 32 0x5a)) @?= Right (BS.replicate 32 0x5a)
        mkDigest (BS.replicate 31 0) @?= Left (DigestWrongLength 31),
      testCase "state token has strict prefix, length, and alphabet" $ do
        fmap stateTokenText (mkStateToken "Sabcdefghijklmnopqrstuv")
          @?= Right "Sabcdefghijklmnopqrstuv"
        mkStateToken "Tabcdefghijklmnopqrstuv" @?= Left (StateTokenWrongPrefix 'T')
        mkStateToken "Sabcdefghijklmnopqrstu=" @?= Left (StateTokenInvalidCharacter '=')
        mkStateToken "Sshort" @?= Left (StateTokenWrongLength 6)
    ]

exitAndConfigTests :: TestTree
exitAndConfigTests =
  testGroup
    "schemas, exits, and configuration"
    [ testCase "exit classes are exactly 0/2/3/4" $ do
        map exitClassCode [ExitSuccess, ExitUserError, ExitConflict, ExitCheckFailed]
          @?= [0, 2, 3, 4]
        map exitClassFromCode [0, 2, 3, 4, 1]
          @?= map Just [ExitSuccess, ExitUserError, ExitConflict, ExitCheckFailed] <> [Nothing],
      testCase "default config has the committed schema, paths, and trunk refs" $ do
        configSchema defaultConfig @?= ConfigSchemaV1
        repoPathText (managedDecisionPath (configManagedPaths defaultConfig))
          @?= "architecture/adrai/decisions"
        repoPathText (managedConnectionPath (configManagedPaths defaultConfig))
          @?= "architecture/adrai/connections"
        map logicalLineId (configLogicalLines defaultConfig) @?= ["trunk"]
        case configLogicalLines defaultConfig of
          [trunkLine] ->
            map gitRefText (logicalLineRefs trunkLine)
              @?= [ "refs/heads/main",
                    "refs/remotes/origin/main",
                    "refs/heads/master",
                    "refs/remotes/origin/master"
                  ]
          unexpected -> assertFailure ("unexpected default logical lines: " <> show unexpected),
      testCase "Git refs require fully qualified canonical names" $ do
        assertBool "shorthand branch ref" (isLeft (mkGitRef "main"))
        fmap gitRefText (mkGitRef "refs/heads/main") @?= Right "refs/heads/main",
      testCase "constructs a strict custom config" $
        case (mkRepoPath "adr", mkRepoPath "relations", mkGitRef "refs/heads/main") of
          (Right decisions, Right connections, Right ref) ->
            case (mkManagedPaths decisions connections, mkLogicalLine "trunk" [ref]) of
              (Right paths, Right line) ->
                fmap configSchema (mkConfig ConfigSchemaV1 paths [line]) @?= Right ConfigSchemaV1
              other -> assertFailure ("fixture construction failed: " <> show other)
          other -> assertFailure ("fixture construction failed: " <> show other)
    ]

sharedRecordTests :: TestTree
sharedRecordTests =
  testGroup
    "shared request and result records"
    [ testCase "constructs actor, mutation context, provenance, and commit result" $
        case (mkOperationId operationText, mkActor LlmActor "agent-7" (Just "model-x"), mkRepoPath "architecture/adrai/decisions/a.md") of
          (Right operationId, Right actor, Right path) -> do
            let provenance = ProvenanceInputs Nothing Nothing Nothing
                mutation = MutationContext operationId actor "working-tree" Nothing provenance
                result = CommitResult operationId "0123456789abcdef" [path] True
            mutationOperationId mutation @?= operationId
            mutationActor mutation @?= actor
            mutationProvenanceInputs mutation @?= provenance
            commitOperationId result @?= operationId
            commitCreatedPaths result @?= [path]
            commitIndexUpdated result @?= True
          other -> assertFailure ("fixture construction failed: " <> show other),
      testCase "constructs shared selectors and modes" $ do
        WorkingRevision @?= WorkingRevision
        AtRevision "HEAD~1" @?= AtRevision "HEAD~1"
        [CollapsedView, ExplodedView] @?= [CollapsedView, ExplodedView]
    ]

mkAdrRef :: T.Text -> Either IdViolation ObjectRef
mkAdrRef = fmap adrObjectRef . mkAdrId

mkRecordRef :: T.Text -> Either IdViolation ObjectRef
mkRecordRef = fmap recordObjectRef . mkRecordId

adrText :: T.Text
adrText = "A0123456789ABCDEFGHJKMNPQRS"

adrTextB :: T.Text
adrTextB = "A0123456789ABCDEFGHJKMNPQRT"

recordText :: T.Text
recordText = "R0123456789ABCDEFGHJKMNPQRS"

connectionText :: T.Text
connectionText = "C0123456789ABCDEFGHJKMNPQRS"

operationText :: T.Text
operationText = "O0123456789ABCDEFGHJKMNPQRS"
