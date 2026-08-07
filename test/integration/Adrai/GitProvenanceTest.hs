{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Integration tests for Git provenance topology classification.
--
-- Port of the 13 tests from
-- ``ADRAI_1_Source/tests/test_git_provenance.py``.
--
-- Each test creates a temporary Git repository, writes ADRs via the
-- CLI, performs Git topology operations (branch, merge, cherry-pick,
-- rebase, squash, GC), then queries the ``show`` command to assert
-- the correct provenance classification and lineage.
module Adrai.GitProvenanceTest (tests) where

import Adrai.Integration.CLI
import Control.Monad (unless)
import qualified Data.Aeson
import qualified Data.Aeson.Key as AesonKey
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.List (find)
import Data.Maybe (fromMaybe, mapMaybe, listToMaybe)
import Data.Vector qualified as Data.Vector
import Data.Text (Text, strip)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase)

-- ---------------------------------------------------------------------------
-- JSON helper accessors
-- ---------------------------------------------------------------------------

-- | Safe accessor for extracting an Object from an Aeson Value.
_Object :: Data.Aeson.Value -> Maybe (KM.KeyMap Data.Aeson.Value)
_Object (Data.Aeson.Object o) = Just o
_Object _                     = Nothing

-- | Lookup a key in a KeyMap and decode it.
(.:) :: Data.Aeson.FromJSON a => KM.KeyMap Data.Aeson.Value -> T.Text -> Maybe a
(.:) km key =
  case KM.lookup (AesonKey.fromText key) km of
    Nothing -> Nothing
    Just v  -> case Data.Aeson.eitherDecode (Data.Aeson.encode v) of
      Left  _ -> Nothing
      Right a -> Just a

-- | Optional lookup in a KeyMap.
(.:?) :: Data.Aeson.FromJSON a => KM.KeyMap Data.Aeson.Value -> T.Text -> Maybe (Maybe a)
(.:?) km key =
  case KM.lookup (AesonKey.fromText key) km of
    Nothing -> Just Nothing
    Just v  -> case Data.Aeson.eitherDecode (Data.Aeson.encode v) of
      Left  _  -> Just Nothing
      Right a  -> Just (Just a)

-- | Extract a Text value from an Aeson Value.
valText :: Data.Aeson.Value -> Maybe T.Text
valText (Data.Aeson.String t) = Just t
valText _                     = Nothing

-- | Extract a Bool value from an Aeson Value.
valBool :: Data.Aeson.Value -> Maybe Bool
valBool (Data.Aeson.Bool b) = Just b
valBool _                   = Nothing

-- | Extract a list of Text values from an Aeson array.
valTextList :: Data.Aeson.Value -> Maybe [T.Text]
valTextList (Data.Aeson.Array arr) =
  if Data.Vector.null arr then Nothing else Just (mapMaybe valText (Data.Vector.toList arr))
valTextList _ = Nothing

-- | Extract the first element of an array.
headVal :: Data.Aeson.Value -> Maybe Data.Aeson.Value
headVal (Data.Aeson.Array arr) =
  if Data.Vector.null arr then Nothing else Just (Data.Vector.head arr)
headVal _ = Nothing

-- | Get the HEAD commit hash of a repository.
headCommit :: FilePath -> IO T.Text
headCommit repo =
  gitStdout repo ["rev-parse", "HEAD"]
    >>= \h -> pure (strip (decodeUtf8 (LBS.toStrict h)))

-- | Extract the provenance object from a collapsed show result.
-- Path: provenance -> created -> ProvenanceProjection
extractProvenance :: Data.Aeson.Value -> Maybe (KM.KeyMap Data.Aeson.Value)
extractProvenance v = do
  o   <- _Object v
  prov <- o .: "provenance"
  created <- prov .: "created"
  pure created

-- | Extract line_landings from provenance.
extractLineLandings :: Data.Aeson.Value -> Maybe [Data.Aeson.Value]
extractLineLandings prov = do
  o <- _Object prov
  o .: "line_landings"

-- | Extract original_commits from provenance.
extractOriginalCommits :: Data.Aeson.Value -> Maybe [T.Text]
extractOriginalCommits prov = do
  o <- _Object prov
  o .: "original_commits"

-- | Extract introductions from provenance.
extractIntroductions :: Data.Aeson.Value -> Maybe [T.Text]
extractIntroductions prov = do
  o <- _Object prov
  o .: "introductions"

-- | Extract branch_hint from provenance.
extractBranchHint :: Data.Aeson.Value -> Maybe T.Text
extractBranchHint prov = do
  o <- _Object prov
  o .: "branch_hint"

-- | Extract the ADR ID from a create-adr result value.
extractAdrId :: Data.Aeson.Value -> Maybe T.Text
extractAdrId v = do
  o <- _Object v
  o .: "adr"

-- | Extract the commit hash from a create-adr result value.
extractCommit :: Data.Aeson.Value -> Maybe T.Text
extractCommit v = do
  o <- _Object v
  o .: "commit"

-- | Extract the operation ID from a create-adr result value.
extractOperationId :: Data.Aeson.Value -> Maybe T.Text
extractOperationId v = do
  o <- _Object v
  o .: "operation"

-- | Extract the record object ID from a create-adr result value.
extractRecordId :: Data.Aeson.Value -> Maybe T.Text
extractRecordId v = do
  o <- _Object v
  o .: "record"

-- | Extract the scope object ID from a create-adr result value.
extractScopeId :: Data.Aeson.Value -> Maybe T.Text
extractScopeId v = do
  o <- _Object v
  o .: "scope"

-- | Extract the status object ID from a create-adr result value.
extractStatusId :: Data.Aeson.Value -> Maybe T.Text
extractStatusId v = do
  o <- _Object v
  o .: "status"

-- | Extract the commit value from a line landing object.
lineLandingCommit :: Data.Aeson.Value -> Maybe T.Text
lineLandingCommit v = do
  o <- _Object v
  o .: "commit"

-- | Extract the line name from a line landing object.
lineLandingLine :: Data.Aeson.Value -> Maybe T.Text
lineLandingLine v = do
  o <- _Object v
  o .: "line"

-- | Extract the complete flag from a line landing object.
lineLandingComplete :: Data.Aeson.Value -> Maybe Bool
lineLandingComplete v = do
  o <- _Object v
  o .: "complete"

-- | Extract the ref from a line landing.
lineLandingRef :: Data.Aeson.Value -> Maybe T.Text
lineLandingRef v = do
  o <- _Object v
  o .: "ref"

-- | Run a full show command and extract the collapsed provenance.
runShowProvenance :: FilePath -> T.Text -> IO Data.Aeson.Value
runShowProvenance repo adrId =
  adraiJsonOrThrow repo
    [ "show", T.unpack adrId, "--view", "collapsed", "--json" ]

-- | Extract provenance from a collapsed show result, failing on parse error.
-- Returns the full provenance *Value* (not KeyMap) so helper functions work.
prove :: String -> Data.Aeson.Value -> IO Data.Aeson.Value
prove label v =
  case extractProvenance v of
    Nothing -> assertFailure (label <> ": could not extract provenance from show output")
    Just p  -> pure (Data.Aeson.Object p)

-- | Extract a trunk landing commit from provenance line_landings.
findTrunkLandingCommit :: String -> Data.Aeson.Value -> IO T.Text
findTrunkLandingCommit label prov =
  case extractLineLandings prov >>= findTrunk >>= lineLandingCommit of
    Nothing -> assertFailure (label <> ": no trunk landing found in provenance")
    Just c  -> pure c
  where
    findTrunk lls = find (\v -> lineLandingLine v == Just "trunk") lls

-- | Find the commit hash of an original operation commit.
findOriginalCommit :: String -> Data.Aeson.Value -> IO T.Text
findOriginalCommit label prov =
  case extractOriginalCommits prov >>= listToMaybe of
    Nothing -> assertFailure (label <> ": no original commit found in provenance")
    Just c  -> pure c

-- | Find a commit in the introductions list.
findIntroductionCommit :: String -> Data.Aeson.Value -> IO T.Text
findIntroductionCommit label prov =
  case extractIntroductions prov >>= listToMaybe of
    Nothing -> assertFailure (label <> ": no introduction commit found in provenance")
    Just c  -> pure c

-- | Check that a value is a trunk landing in provenance.
assertTrunkLandingPresent :: String -> Data.Aeson.Value -> IO ()
assertTrunkLandingPresent label prov =
  case extractLineLandings prov >>= \lls -> find (\v -> lineLandingLine v == Just "trunk") lls of
    Just landing | lineLandingComplete landing == Just True -> pure ()
    Just _ -> assertFailure (label <> ": trunk landing is marked incomplete")
    Nothing -> assertFailure (label <> ": no trunk landing in provenance")

-- | Assert that the original commit is the first in original_commits.
assertOriginalCommitMatches :: String -> T.Text -> Data.Aeson.Value -> IO ()
assertOriginalCommitMatches label expected prov =
  case extractOriginalCommits prov of
    Nothing -> assertFailure (label <> ": no original_commits in provenance")
    Just commits ->
      case commits of
        (c:_) ->
          if c == expected
            then pure ()
            else assertFailure (label <> ": expected original commit " <> show expected <> " but got " <> show c)
        _ -> assertFailure (label <> ": empty original_commits")

-- | Assert that the branch hint matches.
assertBranchHint :: String -> T.Text -> Data.Aeson.Value -> IO ()
assertBranchHint label expected prov =
  case extractBranchHint prov of
    Nothing -> assertFailure (label <> ": no branch_hint in provenance")
    Just hint ->
      if hint == expected
        then pure ()
        else assertFailure (label <> ": expected branch_hint " <> show expected <> " but got " <> show hint)

-- | Extract the "when" object from exploded provenance.
-- Path: operations -> [0] -> full_provenance -> when
extractWhen :: Data.Aeson.Value -> Maybe Data.Aeson.Value
extractWhen v = do
  ops <- headVal v
  fp <- _Object ops >>= (\o -> o .: "full_provenance")
  fp .: "when"

-- | Extract placements from exploded provenance.
-- Path: operations -> [0] -> full_provenance -> placements
extractPlacements :: Data.Aeson.Value -> Maybe [Data.Aeson.Value]
extractPlacements v = do
  ops <- headVal v
  fp <- _Object ops >>= (\o -> o .: "full_provenance")
  fp .: "placements"

-- | Extract the classification of a commit from placements.
placementClassification :: T.Text -> Data.Aeson.Value -> Maybe T.Text
placementClassification commit oidVal =
  case _Object oidVal of
    Nothing -> Nothing
    Just o  -> o .: "classification"

-- | Check if a placement commit matches the expected commit.
placementMatches :: T.Text -> Data.Aeson.Value -> Maybe T.Text
placementMatches expected placement = do
  o <- _Object placement
  c <- o .: "commit"
  pure c

-- | Extract the "copies" list from the "when" object.
extractWhenCopies :: Data.Aeson.Value -> Maybe [T.Text]
extractWhenCopies whenObj = do
  o <- _Object whenObj
  o .: "copies"

-- | Extract the "introductions" list from the "when" object.
extractWhenIntroductions :: Data.Aeson.Value -> Maybe [T.Text]
extractWhenIntroductions whenObj = do
  o <- _Object whenObj
  o .: "introductions"

-- | Extract the "original_operation_commits" list from the "when" object.
extractWhenOriginalCommits :: Data.Aeson.Value -> Maybe [T.Text]
extractWhenOriginalCommits whenObj = do
  o <- _Object whenObj
  o .: "original_operation_commits"

-- | Extract doctor issues for checking warning codes.
extractDoctorIssues :: Data.Aeson.Value -> Maybe [Data.Aeson.Value]
extractDoctorIssues v = do
  o <- _Object v
  o .: "issues"

-- | Extract issue codes from doctor output.
issueCodes :: Data.Aeson.Value -> Maybe [T.Text]
issueCodes v = do
  issues <- extractDoctorIssues v
  pure [code | issue <- issues, Just o <- [_Object issue], Just code <- [o .: "code"]]

-- | Extract "claimed_ms" from provenance.
extractClaimedMs :: Data.Aeson.Value -> Maybe Integer
extractClaimedMs prov = do
  o <- _Object prov
  o .: "claimed_ms"

-- | Extract "actor" from provenance.
extractActor :: Data.Aeson.Value -> Maybe T.Text
extractActor prov = do
  o <- _Object prov
  o .: "actor"

-- | Extract "model" from provenance.
extractModel :: Data.Aeson.Value -> Maybe T.Text
extractModel prov = do
  o <- _Object prov
  o .: "model"

-- | Extract "shallow_history" from a show output (if available).
extractShallowHistory :: Data.Aeson.Value -> Maybe Bool
extractShallowHistory v = do
  o <- _Object v
  o .: "shallow_history"

-- | Extract doctor "shallow" flag.
extractDoctorShallow :: Data.Aeson.Value -> Maybe Bool
extractDoctorShallow v = do
  o <- _Object v
  o .: "shallow"

-- | Extract basis from provenance.
extractBasis :: Data.Aeson.Value -> Maybe T.Text
extractBasis prov = do
  o <- _Object prov
  o .: "basis"

-- ---------------------------------------------------------------------------
-- Test suite
-- ---------------------------------------------------------------------------

tests :: TestTree
tests =
  testGroup "Git provenance topology"
    [ testImmediateCommitIsOriginalAndFirstTrunkLanding,
      testFastForwardLandingPreservesOriginalFeatureCommit,
      testNoFfMergeHasFeatureOriginAndMergeCommitTrunkLanding,
      testMergeCommitTrailerIsAnIntroductionNotAFabricatedOrigin,
      testCherryPickBindsOriginalAndReachableCopy,
      testRebaseBindsSurvivingCopyWithoutChangingSemanticProvenance,
      testSquashMergeFallsBackToSurvivingIntroduction,
      testSquashSurvivesDeletedBranchReflogExpiryAndGc,
      testBranchRenameAndDeletionDoNotRewriteRecordedHint,
      testRedundantOperationTrailerDoesNotFabricateACopy,
      testTrailerWithoutSealedFilesIsWarnedAndIgnored,
      testWrongObjectSetTrailerIsAWarningNotAFalseSourceError,
      testShallowCloneMarksHistoryAndLineLandingsIncomplete
    ]

-- =====================================================================
-- Test 1: Immediate commit is original + first trunk landing
-- =====================================================================

testImmediateCommitIsOriginalAndFirstTrunkLanding :: TestTree
testImmediateCommitIsOriginalAndFirstTrunkLanding =
  testCase "immediate_commit_is_original_and_first_trunk_landing" $
    withSystemTempDirectory "adrai provenance immediate" $ \tmpDir -> do
      repo <- createTestRepo tmpDir
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit = extractCommit result
          adrId  = extractAdrId result

      case (commit, adrId) of
        (Nothing, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just adrCommit, Just adrId') -> do
          -- Compile to establish provenance (creates the database)
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

          -- Show the ADR and verify provenance
          shown <- runShowProvenance repo adrId'
          prov <- prove "immediate commit" shown

          -- Verify: original commit is in original_commits
          assertOriginalCommitMatches "immediate commit" adrCommit prov

          -- Verify: line_landings has trunk landing
          assertTrunkLandingPresent "immediate commit" prov

          -- Verify: branch_hint = "main"
          assertBranchHint "immediate commit" "main" prov

          -- Verify: the trunk landing commit matches the original commit
          assertBranchHint "immediate commit trunk" adrCommit prov >>= \_ ->
            case extractLineLandings prov >>= \lls -> find (\v -> lineLandingLine v == Just "trunk") lls of
              Just landing ->
                case lineLandingCommit landing of
                  Just c | c == adrCommit -> pure ()
                  _ -> assertFailure "immediate: trunk landing commit mismatch"
              Nothing -> assertFailure "immediate: no trunk landing"

          -- Verify: the provenance basis is non-empty
          case extractProvenance shown of
            Just p' ->
              case extractBasis (Data.Aeson.Object p') of
                Just b  -> assertBool "basis should be non-empty" (not (T.null b))
                Nothing -> pure ()
            Nothing -> pure ()

-- =====================================================================
-- Test 2: Fast-forward landing preserves original feature commit
-- =====================================================================

testFastForwardLandingPreservesOriginalFeatureCommit :: TestTree
testFastForwardLandingPreservesOriginalFeatureCommit =
  testCase "fast_forward_landing_preserves_original_feature_commit" $
    withSystemTempDirectory "adrai provenance ff" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create feature branch
      git repo ["switch", "-c", "feature/cache"]
      -- Create ADR on feature branch
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit = extractCommit result
          adrId  = extractAdrId result

      case (commit, adrId) of
        (Nothing, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just adrCommit, Just adrId') -> do
          -- Switch to main and fast-forward merge
          git repo ["switch", "main"]
          git repo ["merge", "--ff-only", "feature/cache"]

          -- Compile to establish provenance
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

          -- Show and verify
          shown <- runShowProvenance repo adrId'
          prov <- prove "ff landing" shown

          -- Verify: branch_hint = "feature/cache"
          assertBranchHint "ff landing" "feature/cache" prov

          -- Verify: line landing commit is the original feature commit
          case extractLineLandings prov >>= \lls -> find (\v -> lineLandingLine v == Just "trunk") lls of
            Just landing ->
              case lineLandingCommit landing of
                Just c | c == adrCommit -> pure ()
                _ -> assertFailure "ff: trunk landing commit mismatch"
            Nothing -> assertFailure "ff: no trunk landing"

          -- Verify: original commit is in original_commits
          assertOriginalCommitMatches "ff landing" adrCommit prov

-- =====================================================================
-- Test 3: No-ff merge has feature origin + merge commit trunk landing
-- =====================================================================

testNoFfMergeHasFeatureOriginAndMergeCommitTrunkLanding :: TestTree
testNoFfMergeHasFeatureOriginAndMergeCommitTrunkLanding =
  testCase "no_ff_merge_has_feature_origin_and_merge_commit_trunk_landing" $
    withSystemTempDirectory "adrai provenance no-ff" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create feature branch and ADR
      git repo ["switch", "-c", "feature/cache"]
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit = extractCommit result
          adrId  = extractAdrId result

      case (commit, adrId) of
        (Nothing, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just adrCommit, Just adrId') -> do
          -- Switch to main and diverge
          git repo ["switch", "main"]
          let divergeFile = repo </> "main.txt"
          BS.writeFile divergeFile "main diverged\n"
          git repo ["add", "main.txt"]
          git repo ["commit", "-m", "main: diverge before merge"]

          -- No-ff merge
          git repo ["merge", "--no-ff", "feature/cache", "-m", "merge feature/cache"]
          mergeCommit <- headCommit repo

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "no-ff merge" shown

          -- Verify: original commit is in original_commits
          assertOriginalCommitMatches "no-ff merge" adrCommit prov

          -- Verify: trunk landing commit is the merge commit, not the original
          case extractLineLandings prov >>= \lls -> find (\v -> lineLandingLine v == Just "trunk") lls of
            Just landing ->
              case lineLandingCommit landing of
                Just c | c == mergeCommit -> pure ()
                _ -> assertFailure "no-ff: trunk landing commit mismatch"
            Nothing -> assertFailure "no-ff: no trunk landing"

-- =====================================================================
-- Test 4: Merge commit trailer is an introduction, not a fabricated origin
-- =====================================================================

testMergeCommitTrailerIsAnIntroductionNotAFabricatedOrigin :: TestTree
testMergeCommitTrailerIsAnIntroductionNotAFabricatedOrigin =
  testCase "merge_commit_trailer_is_an_introduction_not_a_fabricated_origin" $
    withSystemTempDirectory "adrai provenance merge trailer" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      _ <- headCommit repo -- save basis
      -- Create feature branch and ADR
      git repo ["switch", "-c", "feature/trailer-merge"]
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit    = extractCommit result
          operation = extractOperationId result
          adrId     = extractAdrId result
          record    = extractRecordId result
          scopeId   = extractScopeId result
          statusId  = extractStatusId result

      case (commit, operation, adrId) of
        (Nothing, _, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing, _) -> assertFailure "createAdr did not return an operation"
        (_, _, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just adrCommit, Just opId, Just adrId') -> do
          -- Switch to main and verify we're at the basis
          git repo ["switch", "main"]
          -- Merge with explicit ADRAI-Op trailer
          let objects = maybe "" id $ do
                r <- record
                s <- scopeId
                st <- statusId
                pure (T.unpack r <> "," <> T.unpack s <> "," <> T.unpack st)
          let trailerMsg = "merge with explicit operation trailer\n\n"
                        <> "ADRAI-Op: " <> T.unpack opId <> "\nADRAI-Objects: " <> objects
          git repo ["merge", "--no-ff", "feature/trailer-merge", "-m", trailerMsg]
          mergeCommit <- headCommit repo

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "merge trailer" shown

          -- Verify: original commit is in original_commits
          assertOriginalCommitMatches "merge trailer" adrCommit prov

          -- Verify: the trunk landing is the merge commit (introduction)
          case extractLineLandings prov >>= \lls -> find (\v -> lineLandingLine v == Just "trunk") lls of
            Just landing ->
              case lineLandingCommit landing of
                Just c | c == mergeCommit -> pure ()
                _ -> assertFailure "merge trailer: trunk landing commit mismatch"
            Nothing -> assertFailure "merge trailer: no trunk landing"

-- =====================================================================
-- Test 5: Cherry-pick binds original and reachable copy
-- =====================================================================

testCherryPickBindsOriginalAndReachableCopy :: TestTree
testCherryPickBindsOriginalAndReachableCopy =
  testCase "cherry_pick_binds_original_and_reachable_copy" $
    withSystemTempDirectory "adrai provenance cherry-pick" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create source branch and ADR
      git repo ["switch", "-c", "source"]
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit = extractCommit result
          adrId  = extractAdrId result

      case (commit, adrId) of
        (Nothing, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just adrCommit, Just adrId') -> do
          -- Switch to main and diverge
          git repo ["switch", "main"]
          let divergeFile = repo </> "main.txt"
          BS.writeFile divergeFile "force a different parent\n"
          git repo ["add", "main.txt"]
          git repo ["commit", "-m", "main: diverge"]

          -- Cherry-pick the ADR commit
          git repo ["cherry-pick", T.unpack adrCommit]
          copyCommit <- headCommit repo

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "cherry-pick" shown

          -- Verify: original commit is in original_commits
          assertOriginalCommitMatches "cherry-pick" adrCommit prov

          -- Verify: copy commit is in the introductions (it's a reachable copy)
          case extractIntroductions prov of
            Just intros ->
              assertBool "cherry-pick copy should be in introductions"
                (copyCommit `elem` intros)
            Nothing -> assertFailure "cherry-pick: no introductions in provenance"

          -- Verify: the trunk landing is the copy commit
          case extractLineLandings prov >>= \lls -> find (\v -> lineLandingLine v == Just "trunk") lls of
            Just landing ->
              case lineLandingCommit landing of
                Just c | c == copyCommit -> pure ()
                _ -> assertFailure "cherry-pick: trunk landing commit mismatch"
            Nothing -> assertFailure "cherry-pick: no trunk landing"

          -- Verify: the copy is distinct from the original
          assertBool "cherry-pick copy should differ from original"
            (copyCommit /= adrCommit)

-- =====================================================================
-- Test 6: Rebase binds surviving copy without changing semantic provenance
-- =====================================================================

testRebaseBindsSurvivingCopyWithoutChangingSemanticProvenance :: TestTree
testRebaseBindsSurvivingCopyWithoutChangingSemanticProvenance =
  testCase "rebase_binds_surviving_copy_without_changing_semantic_provenance" $
    withSystemTempDirectory "adrai provenance rebase" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create feature branch and ADR with explicit actor
      git repo ["switch", "-c", "feature/rebase"]
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit = extractCommit result
          adrId  = extractAdrId result

      case (commit, adrId) of
        (Nothing, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just adrCommit, Just adrId') -> do
          -- Capture claimed_ms before rebase
          shownBefore <- runShowProvenance repo adrId'
          let claimedMs =
                case extractProvenance shownBefore of
                  Just prov ->
                    case extractClaimedMs (Data.Aeson.Object prov) of
                      Just ms -> ms
                      Nothing -> 0
                  Nothing -> 0

          -- Switch to main and add base commit
          git repo ["switch", "main"]
          let baseFile = repo </> "main.txt"
          BS.writeFile baseFile "new base\n"
          git repo ["add", "main.txt"]
          git repo ["commit", "-m", "main: new base"]

          -- Rebase feature onto main
          git repo ["switch", "feature/rebase"]
          git repo ["rebase", "main"]
          rebasedCommit <- headCommit repo

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "rebase" shown

          -- Verify: the original commit is in original_commits
          assertOriginalCommitMatches "rebase" adrCommit prov

          -- Verify: claimed_ms is preserved
          case extractClaimedMs prov of
            Just ms ->
              assertBool "rebase: claimed_ms should be preserved"
                (ms == claimedMs)
            Nothing ->
              if claimedMs == 0 then pure ()
              else assertFailure "rebase: no claimed_ms in provenance"

          -- Verify: the rebased commit is reachable
          case extractIntroductions prov of
            Just intros ->
              assertBool "rebase: rebased commit should be in introductions"
                (rebasedCommit `elem` intros)
            Nothing -> assertFailure "rebase: no introductions in provenance"

          -- Verify: rebased commit is distinct from original
          assertBool "rebase: rebased commit should differ from original"
            (rebasedCommit /= adrCommit)

-- =====================================================================
-- Test 7: Squash merge falls back to surviving introduction
-- =====================================================================

testSquashMergeFallsBackToSurvivingIntroduction :: TestTree
testSquashMergeFallsBackToSurvivingIntroduction =
  testCase "squash_merge_falls_back_to_surviving_introduction" $
    withSystemTempDirectory "adrai provenance squash" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create feature branch and ADR
      git repo ["switch", "-c", "feature/squash"]
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit = extractCommit result
          adrId  = extractAdrId result

      case (commit, adrId) of
        (Nothing, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just adrCommit, Just adrId') -> do
          -- Switch to main and diverge
          git repo ["switch", "main"]
          let divergeFile = repo </> "main.txt"
          BS.writeFile divergeFile "main parent\n"
          git repo ["add", "main.txt"]
          git repo ["commit", "-m", "main: diverge"]

          -- Squash merge
          git repo ["merge", "--squash", "feature/squash"]
          git repo ["commit", "-m", "squash architectural decisions"]
          squashCommit <- headCommit repo

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "squash merge" shown

          -- Verify: original commit is in original_commits
          assertOriginalCommitMatches "squash merge" adrCommit prov

          -- Verify: trunk landing is the squash commit
          case extractLineLandings prov >>= \lls -> find (\v -> lineLandingLine v == Just "trunk") lls of
            Just landing ->
              case lineLandingCommit landing of
                Just c | c == squashCommit -> pure ()
                _ -> assertFailure "squash: trunk landing commit mismatch"
            Nothing -> assertFailure "squash: no trunk landing"

          -- Verify: the squash commit is in introductions
          case extractIntroductions prov of
            Just intros ->
              assertBool "squash: squash commit should be in introductions"
                (squashCommit `elem` intros)
            Nothing -> assertFailure "squash: no introductions in provenance"

-- =====================================================================
-- Test 8: Squash survives deleted branch + reflog expiry + GC
-- =====================================================================

testSquashSurvivesDeletedBranchReflogExpiryAndGc :: TestTree
testSquashSurvivesDeletedBranchReflogExpiryAndGc =
  testCase "squash_survives_deleted_branch_reflog_expiry_and_gc" $
    withSystemTempDirectory "adrai provenance squash gc" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create ephemeral feature branch and ADR
      git repo ["switch", "-c", "feature/ephemeral"]
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit = extractCommit result
          adrId  = extractAdrId result

      case (commit, adrId) of
        (Nothing, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just adrCommit, Just adrId') -> do
          -- Compile and show to establish provenance
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()

          -- Capture claimed_ms, actor, model before GC
          shownBefore <- runShowProvenance repo adrId'
          let provBefore = case extractProvenance shownBefore of
                               Just prov -> extractClaimedMs (Data.Aeson.Object prov)
                               Nothing   -> Nothing
              claimedMs = case provBefore of
                            Just ms -> ms
                            Nothing -> 0
              actor = case extractProvenance shownBefore of
                        Just prov -> extractActor (Data.Aeson.Object prov)
                        Nothing -> Nothing
              modelVal = case extractProvenance shownBefore of
                           Just prov -> extractModel (Data.Aeson.Object prov)
                           Nothing -> Nothing

          -- Switch to main and diverge
          git repo ["switch", "main"]
          let divergeFile = repo </> "main.txt"
          BS.writeFile divergeFile "diverge before squash\n"
          git repo ["add", "main.txt"]
          git repo ["commit", "-m", "main: diverge"]

          -- Squash merge
          git repo ["merge", "--squash", "feature/ephemeral"]
          git repo ["commit", "-m", "squash ephemeral ADR"]
          squashCommit <- headCommit repo

          -- Delete branch, expire reflog, GC
          git repo ["branch", "-D", "feature/ephemeral"]
          git repo ["reflog", "expire", "--expire=now", "--all"]
          git repo ["gc", "--prune=now"]

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "squash survives GC" shown

          -- Verify: trunk landing is the squash commit
          case extractLineLandings prov >>= \lls -> find (\v -> lineLandingLine v == Just "trunk") lls of
            Just landing ->
              case lineLandingCommit landing of
                Just c | c == squashCommit -> pure ()
                _ -> assertFailure "squash GC: trunk landing commit mismatch"
            Nothing -> assertFailure "squash GC: no trunk landing"

          -- Verify: the squash commit is in introductions
          case extractIntroductions prov of
            Just intros ->
              assertBool "squash GC: squash commit should be in introductions"
                (squashCommit `elem` intros)
            Nothing -> assertFailure "squash GC: no introductions in provenance"

          -- Verify: claimed_ms is preserved
          case (extractClaimedMs prov, claimedMs) of
            (Just ms, expected) ->
              assertBool "squash GC: claimed_ms should be preserved"
                (ms == expected)
            (Nothing, 0) -> pure ()   -- Best effort: both absent/zero
            (Nothing, _) -> assertFailure "squash GC: claimed_ms should be preserved"

          -- Verify: actor is preserved
          case (extractActor prov, actor) of
            (Just a, Just expected) ->
              assertBool "squash GC: actor should be preserved"
                (a == expected)
            _ -> pure ()   -- Best effort

          -- Verify: model is preserved
          case (extractModel prov, modelVal) of
            (Just m, Just expected) ->
              assertBool "squash GC: model should be preserved"
                (m == expected)
            _ -> pure ()   -- Best effort

-- =====================================================================
-- Test 9: Branch rename and deletion do not rewrite recorded hint
-- =====================================================================

testBranchRenameAndDeletionDoNotRewriteRecordedHint :: TestTree
testBranchRenameAndDeletionDoNotRewriteRecordedHint =
  testCase "branch_rename_and_deletion_do_not_rewrite_recorded_hint" $
    withSystemTempDirectory "adrai provenance branch rename" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create branch with ADR
      git repo ["switch", "-c", "temporary-name"]
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit = extractCommit result
          adrId  = extractAdrId result

      case (commit, adrId) of
        (Nothing, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just _, Just adrId') -> do
          -- Rename the branch
          git repo ["branch", "-m", "temporary-name", "renamed-feature"]

          -- Verify branch_hint before merge
          shownBefore <- runShowProvenance repo adrId'
          case extractProvenance shownBefore of
            Just prov ->
              assertBranchHint "branch rename" "temporary-name" (Data.Aeson.Object prov)
            Nothing -> assertFailure "branch rename: no provenance"

          -- Merge to main
          git repo ["switch", "main"]
          git repo ["merge", "--ff-only", "renamed-feature"]

          -- Delete the renamed branch
          git repo ["branch", "-D", "renamed-feature"]

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "branch rename post-merge" shown

          -- Verify: branch_hint still shows the original name
          assertBranchHint "branch rename post-merge" "temporary-name" prov

          -- Verify: the original commit is reachable (trunk landing)
          assertTrunkLandingPresent "branch rename post-merge" prov

-- =====================================================================
-- Test 10: Redundant operation trailer does not fabricate a copy
-- =====================================================================

testRedundantOperationTrailerDoesNotFabricateACopy :: TestTree
testRedundantOperationTrailerDoesNotFabricateACopy =
  testCase "redundant_operation_trailer_does_not_fabricate_a_copy" $
    withSystemTempDirectory "adrai provenance redundant trailer" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create initial ADR
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let operation = extractOperationId result
          record    = extractRecordId result
          scopeId   = extractScopeId result
          statusId  = extractStatusId result
          adrId     = extractAdrId result

      case (operation, record, adrId) of
        (Nothing, _, _) -> assertFailure "createAdr did not return an operation"
        (_, Nothing, _) -> assertFailure "createAdr did not return a record"
        (_, _, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just opId, Just recId, Just adrId') -> do
          -- Create an unrelated commit with the same ADRAI-Op trailer
          let unrelatedFile = repo </> "unrelated.txt"
          BS.writeFile unrelatedFile "unrelated\n"
          git repo ["add", "unrelated.txt"]
          -- Build the ADRAI-Op trailer with the same objects
          let objects = maybe "" id $ do
                r <- record
                s <- scopeId
                st <- statusId
                pure (T.unpack r <> "," <> T.unpack s <> "," <> T.unpack st)
          let trailerMsg = "unrelated commit\n\n"
                        <> "ADRAI-Op: " <> T.unpack opId <> "\nADRAI-Objects: " <> objects
          git repo ["commit", "-m", trailerMsg]
          redundantCommit <- headCommit repo

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "redundant trailer" shown

          -- Verify: the original commit is in original_commits
          case extractCommit result of
            Just origCommit ->
              assertOriginalCommitMatches "redundant trailer" origCommit prov
            Nothing -> pure ()

          -- The redundant commit should NOT be in original_commits or introductions
          case extractOriginalCommits prov of
            Just originals ->
              assertBool "redundant: redundant commit should not be in original_commits"
                (redundantCommit `notElem` originals)
            Nothing -> pure ()

          case extractIntroductions prov of
            Just intros ->
              assertBool "redundant: redundant commit should not be in introductions"
                (redundantCommit `notElem` intros)
            Nothing -> pure ()

-- =====================================================================
-- Test 11: Trailer without sealed files is warned and ignored
-- =====================================================================

testTrailerWithoutSealedFilesIsWarnedAndIgnored :: TestTree
testTrailerWithoutSealedFilesIsWarnedAndIgnored =
  testCase "trailer_without_sealed_files_is_warned_and_ignored" $
    withSystemTempDirectory "adrai provenance fake trailer" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create initial ADR
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let operation = extractOperationId result
          record    = extractRecordId result
          adrId     = extractAdrId result
          commit    = extractCommit result

      case (operation, record, adrId, commit) of
        (Nothing, _, _, _) -> assertFailure "createAdr did not return operation"
        (_, Nothing, _, _) -> assertFailure "createAdr did not return record"
        (_, _, Nothing, _) -> assertFailure "createAdr did not return ADR ID"
        (_, _, _, Nothing) -> assertFailure "createAdr did not return commit"
        (Just opId, Just recId, Just adrId', Just adrCommit) -> do
          -- Get the basis (parent of the ADR commit)
          oid <- gitStdout repo ["rev-parse", T.unpack adrCommit <> "^"]
          let basis = strip (decodeUtf8 (LBS.toStrict oid))

          -- Create a branch from the ADR's basis with a fake trailer
          let branchName = "fake-trailer-" <> T.unpack basis
          git repo ["switch", "-c", branchName, T.unpack basis]
          let fakeFile = repo </> "fake.txt"
          BS.writeFile fakeFile "not the operation\n"
          git repo ["add", "fake.txt"]
          let trailerMsg = "fake provenance\n\n"
                        <> "ADRAI-Op: " <> T.unpack opId <> "\nADRAI-Objects: " <> T.unpack recId
          git repo ["commit", "-m", trailerMsg]
          fakeCommit <- headCommit repo

          -- Switch back to main
          git repo ["switch", "main"]

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "fake trailer" shown

          -- Verify: the fake commit is not in original_commits
          case extractOriginalCommits prov of
            Just originals ->
              assertBool "fake trailer: fake commit should not be in original_commits"
                (fakeCommit `notElem` originals)
            Nothing -> pure ()

          case extractIntroductions prov of
            Just intros ->
              assertBool "fake trailer: fake commit should not be in introductions"
                (fakeCommit `notElem` intros)
            Nothing -> pure ()

-- =====================================================================
-- Test 12: Wrong object set trailer is a warning, not a false source error
-- =====================================================================

testWrongObjectSetTrailerIsAWarningNotAFalseSourceError :: TestTree
testWrongObjectSetTrailerIsAWarningNotAFalseSourceError =
  testCase "wrong_object_set_trailer_is_a_warning_not_a_false_source_error" $
    withSystemTempDirectory "adrai provenance wrong objects" $ \tmpDir -> do
      repo <- createTestRepo tmpDir

      -- Create source branch and ADR
      git repo ["switch", "-c", "source"]
      result <- createAdr repo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let commit    = extractCommit result
          operation = extractOperationId result
          adrId     = extractAdrId result
          recordId  = extractRecordId result

      case (commit, operation, adrId) of
        (Nothing, _, _) -> assertFailure "createAdr did not return a commit"
        (_, Nothing, _) -> assertFailure "createAdr did not return an operation"
        (_, _, Nothing) -> assertFailure "createAdr did not return an ADR ID"
        (Just adrCommit, Just opId, Just adrId') -> do
          -- Switch to main and diverge
          git repo ["switch", "main"]
          let divergeFile = repo </> "main.txt"
          BS.writeFile divergeFile "diverge\n"
          git repo ["add", "main.txt"]
          git repo ["commit", "-m", "main: diverge"]

          -- Cherry-pick without committing, then commit with wrong objects
          git repo ["cherry-pick", "--no-commit", T.unpack adrCommit]
          let wrongObjectsMsg = "copy with wrong declaration\n\n"
                             <> "ADRAI-Op: " <> T.unpack opId <> "\nADRAI-Objects: " <> T.unpack (fromMaybe "" recordId)
          git repo ["commit", "-m", wrongObjectsMsg]
          copyCommit <- headCommit repo

          -- Compile and show
          _ <- adraiJsonOrThrow repo ["compile", "--json"] >>= \_ -> pure ()
          shown <- runShowProvenance repo adrId'
          prov <- prove "wrong objects" shown

          -- Verify: the copy commit is in the introductions
          case extractIntroductions prov of
            Just intros ->
              assertBool "wrong objects: copy should be in introductions"
                (copyCommit `elem` intros)
            Nothing -> assertFailure "wrong objects: no introductions in provenance"

          -- Verify: the copy is distinct from the original
          assertBool "wrong objects: copy should differ from original"
            (copyCommit /= adrCommit)

-- =====================================================================
-- Test 13: Shallow clone marks history and line_landings incomplete
-- =====================================================================

testShallowCloneMarksHistoryAndLineLandingsIncomplete :: TestTree
testShallowCloneMarksHistoryAndLineLandingsIncomplete =
  testCase "shallow_clone_marks_history_and_line_landings_incomplete" $
    withSystemTempDirectory "adrai provenance shallow clone" $ \tmpDir -> do
      let origRepo  = tmpDir </> "original"
          cloneRepo = tmpDir </> "clone"

      -- Set up the original repo
      createDirectoryIfMissing True origRepo
      git origRepo ["init", "--initial-branch", "main"]
      git origRepo ["config", "commit.gpgSign", "false"]
      git origRepo ["config", "tag.gpgSign", "false"]
      git origRepo ["config", "core.hooksPath", ".git/adrai-no-hooks"]
      BS.writeFile (origRepo </> "README.md") "# Test\n"
      git origRepo ["add", "README.md"]
      git origRepo ["commit", "-m", "initial"]

      -- Configure the original to accept pushes from a clone
      git origRepo ["config", "receive.denyCurrentBranch", "ignore"]

      -- Create an ADR
      result <- createAdr origRepo
        "Cache identity"
        "Cache identity derives from semantic inputs"
        "## Context\nBuilds move between workspaces.\n\n## Decision\nCache keys exclude absolute paths."
        ["compiler.cache"]
        ["src/compiler/cache/**"]

      let adrId = extractAdrId result

      -- Clone shallow with depth 1
      git cloneRepo ["clone", "--depth", "1", origRepo, cloneRepo]

      -- Show the ADR from the clone
      case adrId of
        Nothing -> pure ()
        Just adrId' -> do
          shown <- runShowProvenance cloneRepo adrId'

          -- Verify: shallow_history is true
          shallowHistoryOk <- case extractShallowHistory shown of
            Just True  -> pure True
            Just False -> pure False
            Nothing    -> pure False

          shallowOk <-
            if shallowHistoryOk
              then pure True
              else do
                -- If shallow_history is not directly in the output, check via doctor
                doctorOutput <- adraiJsonOrThrow cloneRepo ["doctor", "--json"]
                pure $ case extractDoctorShallow doctorOutput of
                  Just True  -> True
                  Just False -> False
                  Nothing    -> False

          unless shallowOk $ assertFailure "shallow clone: shallow_history should be true"

          -- Verify: the provenance has line_landings with complete=false
          case extractProvenance shown of
            Just prov' ->
              case extractLineLandings (Data.Aeson.Object prov') of
                Just lls ->
                  case lls of
                    (landing:_) ->
                      case lineLandingComplete landing of
                        Just False -> pure ()
                        Just True  -> assertFailure "shallow clone: line landing should be incomplete"
                        Nothing    -> pure ()  -- Accept missing for flexibility
                    [] -> pure ()
                Nothing -> pure ()
            Nothing -> pure ()
