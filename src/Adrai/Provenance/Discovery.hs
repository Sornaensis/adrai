{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Provenance overlay candidate discovery module.
--
-- Mirrors the Python prototype at
-- ADRAI_1_Source/adrai_core/gitops.py functions @list_refs@,
-- @reflog_commit_roots@, @rev_list_delta@,
-- @commit_log_snapshot_for_oids@, and @added_paths_for_commits@, plus the
-- overlay helper @_current_roots@ and @_observation_fingerprint@ from
-- provenance_cache.py.
--
-- This module discovers commit candidates that feed the overlay:
-- reference observations, reflog roots, rev-list deltas, commit snapshots,
-- and managed-path additions.  Classification / processing logic lives in
-- later chunks.
module Adrai.Provenance.Discovery
  ( -- | Reference observation
    listRefs,

    -- | Reflog commit roots
    reflogCommitRoots,

    -- | Combined observation roots
    observationRoots,

    -- | Rev-list delta
    revListDelta,

    -- | Commit log snapshot
    commitLogSnapshotForOids,

    -- | Managed path additions
    addedPathsForCommits,
    decodeAddedPathsOutput,
    managedPathAdditions,
    managedSuffixes,

    -- | Observation fingerprint
    observationFingerprint,

    -- | First-parent path landings (line landing support)
    firstParentPathLandings,
  )
where

import Adrai.Git
  ( GitError (GitCommandFailed, GitInvalidOutput, GitInvalidUtf8Path),
    GitProtocolError (GitMalformedObjectHeader, GitMalformedPathOutput),
    GitOid,
    GitObjectType (GitCommitObject),
    GitObjectInfo (..),
    GitProcessResult (..),
    Repository (..),
    batchObjectInfo,
    gitOidText,
    runRepository,
  )
import Adrai.Provenance
  ( OverlayFingerprint (..),
    mkGitOid,
    sha256Digest,
  )
import Adrai.Provenance.Overlay
  ( CommitObservation (..),
    ManagedPathAddition (..),
    ObservationRoot (..),
    RefObservation (..),
  )
import Adrai.Types (Digest, digestBytes)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Control.Monad (forM)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as Lazy (toStrict)
import Data.List (sort, sortBy)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Ord (comparing)
import Numeric (showHex)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Data.Vector as Vector
import System.Exit (ExitCode (..))

-- | Managed file suffixes used to filter path additions.
--
-- Mirrors the Python @MANAGED_SUFFIXES@ constant from @adrai_core.formats@.
managedSuffixes :: [Text]
managedSuffixes = [".decision.md", ".connection.md"]

-- | List all refs (heads, remotes, tags) with their tip OID and object type.
--
-- Uses @git for-each-ref@ with a NUL-delimited format.  Mirrors the Python
-- @list_refs@ function from @adrai_core.gitops@.
listRefs :: Repository -> IO (Either GitError [RefObservation])
listRefs repository = do
  result <-
    runRepository
      repository
      "list refs"
      [ "for-each-ref",
        "--format=%(refname)%00%(objectname)%00%(objecttype)",
        "refs/heads",
        "refs/remotes",
        "refs/tags"
      ]
      BS.empty
  pure $ do
    processResult <- result
    if processExitCode processResult /= ExitSuccess
      then Left (commandFailure "list refs" processResult)
      else pure (decodeRefOutput (processStdout processResult))

-- | Decode NUL-delimited ref output from @git for-each-ref@.
decodeRefOutput :: ByteString -> [RefObservation]
decodeRefOutput raw
  | BS.null raw = []
  | otherwise =
      let text = TextEncoding.decodeUtf8 raw
          lines' = Text.splitOn "\n" text
       in [RefObservation refName tipOid objectType
          | line <- lines',
            let parts = Text.splitOn "\0" line,
            length parts == 3,
            let refName = parts !! 0,
            let tipOid = parts !! 1,
            let objectType = parts !! 2,
            not (Text.null refName),
            not (Text.null tipOid)]

-- | Return unique commit OIDs named by all locally available reflogs.
--
-- Mirrors the Python @reflog_commit_roots@ function.  Reflog selectors are
-- intentionally not persisted: their numeric positions shift whenever a new
-- entry is appended.  The commit OIDs are the durable observation roots.
reflogCommitRoots :: Repository -> IO (Either GitError [GitOid])
reflogCommitRoots repository = do
  result <-
    runRepository
      repository
      "reflog commit roots"
      [ "reflog",
        "show",
        "--all",
        "--format=%H"
      ]
      BS.empty
  pure $ do
    processResult <- result
    if processExitCode processResult /= ExitSuccess
      then Left (commandFailure "reflog commit roots" processResult)
      else pure (decodeReflogOids (processStdout processResult))

-- | Decode commit OIDs from reflog show output.  Deduplicates and sorts.
decodeReflogOids :: ByteString -> [GitOid]
decodeReflogOids raw
  | BS.null raw = []
  | otherwise =
      let text = TextEncoding.decodeUtf8 raw
          lines' = Text.lines text
          unique = go Set.empty (sort lines')
       in unique
  where
    go :: Set.Set Text -> [Text] -> [GitOid]
    go _ [] = []
    go seen (line:rest) =
      case Text.strip line of
        "" -> go seen rest
        oid
          | oid `Set.member` seen -> go seen rest
          | otherwise ->
              case mkGitOid oid of
                Left _ -> go seen rest
                Right gitOid -> gitOid : go (Set.insert oid seen) rest

-- | Combine all observation roots for overlay maintenance.
--
-- Mirrors the Python @_current_roots@ function from
-- @adrai_core.provenance_cache@.  Returns a tuple of:
--
-- * All refs as 'RefObservation' records
-- * Observation roots combining:
--   - @\"ref\"@ kind roots from ref tip OIDs
--   - @\"reflog\"@ kind roots from reflog commit OIDs
--   - @\"query\"@ kind roots from prior query roots
--   - The target revision as a @\"query\"@ root if not already covered
--
-- The @priorQueryRoots@ argument holds commit OIDs previously recorded in
-- the overlay's @observation_root@ table under the @\"query\"@ kind.
observationRoots
  :: Repository
  -> GitOid           -- ^ Target revision for the current overlay run
  -> [GitOid]         -- ^ Prior query roots from the overlay
  -> IO (Either GitError ([RefObservation], [ObservationRoot]))
observationRoots repository targetRevision priorQueryRoots = do
  refsResult <- listRefs repository
  reflogResult <- reflogCommitRoots repository
  case (refsResult, reflogResult) of
    (Left err, _) -> pure (Left err)
    (_, Left err) -> pure (Left err)
    (Right refs, Right reflogOids) ->
      pure (Right (buildObservationRoots refs reflogOids priorQueryRoots targetRevision))

-- | Build observation roots from ref observations, reflog OIDs, and prior
-- query roots.
buildObservationRoots
  :: [RefObservation]
  -> [GitOid]
  -> [GitOid]
  -> GitOid
  -> ([RefObservation], [ObservationRoot])
buildObservationRoots refs reflogOids priorQueryRoots targetRevision =
  let -- Ref roots: one per ref whose tip is a commit
      refRoots =
        mapMaybe parseRefRoot refs
        where
          parseRefRoot (RefObservation refName tipOid objectType)
            | objectType == "commit" =
                case mkGitOid tipOid of
                  Right oid -> Just (ObservationRoot "ref" refName oid)
                  Left _ -> Nothing
          parseRefRoot _ = Nothing

      -- Reflog roots: one per unique OID from the reflog
      reflogRoots =
        [ ObservationRoot "reflog" (gitOidText oid) oid
          | oid <- sort reflogOids
        ]

      -- Collect all OIDs already covered by ref roots and reflog roots
      -- Only commit refs contribute OIDs (non-commit refs have no ObservationRoot)
      alreadyCovered :: Set.Set GitOid
      alreadyCovered =
        Set.fromList [oid | ObservationRoot _ _ oid <- refRoots]
          `Set.union` Set.fromList reflogOids

      -- Prior query roots, deduplicated, with target revision added if not covered
      targetOid :: GitOid
      targetOid = targetRevision
      priorOids :: Set.Set GitOid
      priorOids = Set.fromList priorQueryRoots
      covered :: Set.Set GitOid
      covered = alreadyCovered `Set.union` priorOids
      queryOids :: [GitOid]
      queryOids = sort (Set.toList (Set.insert targetOid priorOids `Set.difference` covered))
      queryRoots :: [ObservationRoot]
      queryRoots =
        [ ObservationRoot "query" (gitOidText oid) oid
          | oid <- queryOids
        ]

      allRoots = refRoots ++ reflogRoots ++ queryRoots
  in (refs, allRoots)

-- | Return commits reachable from new roots but not prior observation roots.
--
-- Uses @git rev-list --stdin --topo-order@ to avoid command-line limits when
-- a repository has a large collection of branches and reflog roots.  Missing
-- old roots are silently ignored (they can no longer serve as exclusions
-- after object pruning).  The result is filtered to only available (existing)
-- commit objects.
--
-- Mirrors the Python @rev_list_delta@ function.
revListDelta
  :: Repository
  -> [GitOid]   -- ^ New root OIDs
  -> [GitOid]   -- ^ Old root OIDs (to exclude)
  -> IO (Either GitError [GitOid])
revListDelta repository newRoots oldRoots = do
  let newUnique = dedup newRoots
      allRoots = newUnique ++ dedup oldRoots

  -- Check which roots are available commits
  infoResult <- batchObjectInfo repository allRoots
  case infoResult of
    Left err -> pure (Left err)
    Right info ->
      let availableNew =
            [ oid | oid <- newUnique,
                    Just (Just objInfo) <- [Map.lookup oid info],
                    objectInfoType objInfo == GitCommitObject
                  ]
          availableOld =
            [ oid | oid <- oldRoots,
                    Just (Just objInfo) <- [Map.lookup oid info],
                    objectInfoType objInfo == GitCommitObject
                  ]
      in if null availableNew
         then pure (Right [])
         else do
              let newPayload = Text.intercalate "\n" (map gitOidText availableNew)
                  oldPayload = Text.intercalate "\n"
                                (map (\oid -> "^" <> gitOidText oid) availableOld)
                  fullPayload = newPayload <> "\n" <> oldPayload
              result <-
                runRepository
                  repository
                  "rev-list delta"
                  [ "rev-list",
                    "--topo-order",
                    "--stdin"
                  ]
                  (TextEncoding.encodeUtf8 fullPayload)
              pure $ do
                processResult <- result
                if processExitCode processResult /= ExitSuccess
                  then Left (commandFailure "rev-list delta" processResult)
                  else pure (decodeRevListOutput (processStdout processResult))

-- | Decode rev-list output: one OID per line.  Deduplicates and sorts.
decodeRevListOutput :: ByteString -> [GitOid]
decodeRevListOutput raw
  | BS.null raw = []
  | otherwise =
      let text = TextEncoding.decodeUtf8 raw
          lines' = Text.lines text
          unique = go Set.empty (sort lines')
       in unique
  where
    go :: Set.Set Text -> [Text] -> [GitOid]
    go _ [] = []
    go seen (line:rest) =
      case Text.strip line of
        "" -> go seen rest
        oid
          | oid `Set.member` seen -> go seen rest
          | otherwise ->
              case mkGitOid oid of
                Left _ -> go seen rest
                Right gitOid -> gitOid : go (Set.insert oid seen) rest

-- | Read commit metadata / messages for a bounded set of commits.
--
-- Git command lines have platform-specific limits, so the requests are
-- chunked into groups of 256 OIDs.  Each chunk is processed via
-- @git show -s --no-patch@ with a NUL-delimited format string.
--
-- Mirrors the Python @commit_log_snapshot_for_oids@ function.
commitLogSnapshotForOids
  :: Repository
  -> [GitOid]
  -> IO (Either GitError [CommitObservation])
commitLogSnapshotForOids repository oids = do
  let unique = dedup oids
      chunks' = chunksOf 256 unique
  results <- forM chunks' $ \chunk -> do
    result <-
      runRepository
        repository
        "commit log snapshot"
        (["show", "-s", "--no-patch",
          "--format=%x1e%H%x00%P%x00%at%x00%ct%x00%s%x00%B"]
         ++ map (Text.unpack . gitOidText) chunk)
        BS.empty
    case result of
      Left err -> pure (Left err)
      Right processResult
        | processExitCode processResult /= ExitSuccess ->
            pure (Left (commandFailure "commit log snapshot" processResult))
        | otherwise -> pure (Right (decodeCommitSnapshotOutput (processStdout processResult)))
  case sequence results of
    Left err -> pure (Left err)
    Right chunks'' ->
      let combined = concat chunks''
      in pure (Right combined)

-- | Decode the NUL-delimited commit snapshot output from @git show@.
--
-- Format per record: @%x1e@ separated records, each containing
-- @%x00@ separated fields: oid, parents, authored_ts, committed_ts, subject, message.
decodeCommitSnapshotOutput :: ByteString -> [CommitObservation]
decodeCommitSnapshotOutput raw
  | BS.null raw = []
  | otherwise =
      let text = TextEncoding.decodeUtf8 raw
          records = Text.splitOn "\x1e" text
          unique = go Set.empty records
       in unique
  where
    go :: Set.Set GitOid -> [Text] -> [CommitObservation]
    go _ [] = []
    go seen (record:rest) =
      case Text.stripStart record of
        "" -> go seen rest
        rec
          | Text.null rec -> go seen rest
          | otherwise ->
              let parts = Text.splitOn "\0" rec
              in if length parts < 6
                 then go seen rest
                 else
                   let oidText = Text.strip (parts !! 0)
                       parentsText = Text.strip (parts !! 1)
                       authoredText = Text.strip (parts !! 2)
                       committedText = Text.strip (parts !! 3)
                       subjectText = Text.strip (parts !! 4)
                       messageRaw = parts !! 5
                       messageText = Text.stripEnd messageRaw
                   in case (mkGitOid oidText,
                            readMaybeInt authoredText,
                            readMaybeInt committedText) of
                        (Right oid, Just authored, Just committed) ->
                          let oidKey = oid
                              existing = Set.member oidKey seen
                              newSeen = Set.insert oidKey seen
                          in if existing
                             then go newSeen rest
                             else
                               let parentsList =
                                     if Text.null parentsText
                                       then []
                                       else Text.words parentsText
                               in CommitObservation oid
                                     (Text.unwords parentsList)
                                     authored
                                     committed
                                     subjectText
                                     messageText : go newSeen rest
                        _ -> go seen rest

-- | Read paths added by each commit (including merge-parent introductions).
--
-- Uses @git diff-tree --stdin --root -m -r --no-renames --diff-filter=A@
-- to find paths added by each commit.  The @-m@ flag ensures merge commits
-- are compared against every parent, which is exactly what provenance
-- discovery needs.
--
-- Mirrors the Python @added_paths_for_commits@ function.
addedPathsForCommits
  :: Repository
  -> [GitOid]
  -> IO (Either GitError (Map GitOid (Set Text)))
addedPathsForCommits repository oids = do
  let unique = dedup oids
  if null unique
    then pure (Right Map.empty)
    else do
      let payload = Text.intercalate "\n" (map gitOidText unique) <> "\n"
      result <-
        runRepository
          repository
          "added paths"
          [ "diff-tree",
            "--stdin",
            "--root",
            "-m",
            "-r",
            "--no-renames",
            "--diff-filter=A",
            "--name-status",
            "-z",
            "--pretty=tformat:%x1e%H"
          ]
          (TextEncoding.encodeUtf8 payload)
      pure $ do
        processResult <- result
        if processExitCode processResult /= ExitSuccess
          then Left (commandFailure "added paths" processResult)
          else decodeAddedPathsOutput (Set.fromList unique) (processStdout processResult)

-- | Decode the byte protocol emitted by @git diff-tree -z
-- --pretty=tformat:%x1e%H --name-status@.
--
-- Each record starts with an ASCII record separator, a canonical commit OID,
-- a NUL byte, and diff-tree's required pretty-print newline.  It is followed
-- by zero or more @A NUL path NUL@ pairs.  The state machine recognises a
-- following record separator only when it is expecting another status token,
-- never while it is consuming a path; an arbitrary path byte string may
-- therefore begin with a record separator.  The parser deliberately validates
-- every record against the exact supplied OID set:
-- observations can neither leak repository history nor silently lose a
-- malformed record.
decodeAddedPathsOutput
  :: Set GitOid
  -> ByteString
  -> Either GitError (Map GitOid (Set Text))
decodeAddedPathsOutput expected raw
  | Set.null expected =
      if BS.null raw
        then Right Map.empty
        else Left (malformedFraming raw)
  | BS.null raw = Right initial
  | otherwise = records initial raw
  where
    initial = Map.fromSet (const Set.empty) expected

    records acc bytes
      | BS.null bytes = Right acc
      | BS.head bytes /= recordSeparator = Left (malformedFraming bytes)
      | otherwise = do
          (oid, afterHeader) <- header (BS.tail bytes)
          if oid `Set.member` expected
            then statusOrRecord oid acc afterHeader
            else Left (malformedHeader (TextEncoding.encodeUtf8 (gitOidText oid)))

    header bytes =
      case BS.break (== 0) bytes of
        (_, terminator) | BS.null terminator -> Left (malformedHeader bytes)
        (rawOid, terminator)
          | BS.null rawOid -> Left (malformedHeader bytes)
          | BS.any (> 127) rawOid -> Left (malformedHeader rawOid)
          | otherwise ->
              case mkGitOid (Text.pack (BS8.unpack rawOid)) of
                Left _ -> Left (malformedHeader rawOid)
                Right oid ->
                  case BS.uncons (BS.tail terminator) of
                    Just (10, afterHeader) -> Right (oid, afterHeader)
                    _ -> Left (malformedFraming bytes)

    statusOrRecord oid acc bytes
      | BS.null bytes = records acc bytes
      | BS.head bytes == recordSeparator = records acc bytes
      | otherwise =
          case BS.break (== 0) bytes of
            (_, terminator) | BS.null terminator -> Left (malformedFraming bytes)
            (status, terminator)
              | status /= "A" -> Left (malformedStatus status)
              | otherwise -> path oid acc (BS.tail terminator)

    path oid acc bytes =
      case BS.break (== 0) bytes of
        (_, terminator) | BS.null terminator -> Left (malformedFraming bytes)
        (rawPath, terminator)
          | BS.null rawPath -> Left (malformedFraming bytes)
          | otherwise ->
              case TextEncoding.decodeUtf8' rawPath of
                Left _ -> Left (GitInvalidUtf8Path (BS.take diagnosticLimit rawPath))
                Right decodedPath ->
                  let acc' = Map.insertWith Set.union oid (Set.singleton decodedPath) acc
                   in statusOrRecord oid acc' (BS.tail terminator)

    recordSeparator = 0x1e

malformedHeader :: ByteString -> GitError
malformedHeader = GitInvalidOutput "added paths" . GitMalformedObjectHeader . BS.take diagnosticLimit

malformedFraming :: ByteString -> GitError
malformedFraming = GitInvalidOutput "added paths" . GitMalformedPathOutput . BS.take diagnosticLimit

malformedStatus :: ByteString -> GitError
malformedStatus = GitInvalidOutput "added paths" . GitMalformedPathOutput . BS.take diagnosticLimit

-- | Filter path additions to only paths whose basename ends with one of the
-- managed suffixes.
--
-- Mirrors the Python managed path filtering done inside @_store_new_commits@.
managedPathAdditions
  :: [GitOid]
  -> Map GitOid (Set Text)
  -> IO (Either GitError [ManagedPathAddition])
-- The _oids parameter is accepted for API consistency but the Python
-- prototype filters all added paths without per-OID constraint.
managedPathAdditions _oids addedPaths =
  pure (Right (filterManagedPaths addedPaths))

-- | Filter to paths ending with managed suffixes.
filterManagedPaths :: Map GitOid (Set Text) -> [ManagedPathAddition]
filterManagedPaths addedPaths =
  [ ManagedPathAddition path oid
    | (oid, paths) <- Map.toList addedPaths,
      path <- Set.toList paths,
      any (`Text.isSuffixOf` (basename path)) managedSuffixes
  ]

-- | Return the basename of a path (last component after '/').
basename :: Text -> Text
basename p =
  case Text.reverse p of
    "" -> ""
    r -> case Text.break (== '/') r of
           (_, "") -> r
           (_, rs) -> Text.drop 1 rs

-- | Compute a JSON-serialised SHA-256 fingerprint of sorted refs and roots.
--
-- Mirrors the Python @_observation_fingerprint@ function.
observationFingerprint
  :: [RefObservation]
  -> [ObservationRoot]
  -> IO OverlayFingerprint
observationFingerprint refs roots = do
  let sortedRefs = sortBy (comparing refObservationRefName) refs
      sortedRoots = sortBy (comparing observationRootKind) roots
      payload = Aeson.Object (KeyMap.fromList [
          (Key.fromText "refs", Aeson.Array (Vector.fromList (map refToJson sortedRefs))),
          (Key.fromText "roots", Aeson.Array (Vector.fromList (map rootToJson sortedRoots)))
        ])
      json = Lazy.toStrict (Aeson.encode payload)
      digest = sha256Digest json
  pure (OverlayFingerprint (digestToHex digest))

-- | Convert a 'RefObservation' to a JSON array [refName, tipOid, objectType].
refToJson :: RefObservation -> Aeson.Value
refToJson ref =
  Aeson.Array (Vector.fromList
    [ Aeson.String (refObservationRefName ref),
      Aeson.String (refObservationTipOid ref),
      Aeson.String (refObservationObjectType ref)
    ])

-- | Convert an 'ObservationRoot' to a JSON array [kind, name, commitOid].
rootToJson :: ObservationRoot -> Aeson.Value
rootToJson root =
  Aeson.Array (Vector.fromList
    [ Aeson.String (observationRootKind root),
      Aeson.String (observationRootName root),
      Aeson.String (gitOidText (observationRootCommitOid root))
    ])

-- | Deduplicate a list while preserving first-seen order.
dedup :: (Ord a, Eq a) => [a] -> [a]
dedup = go Set.empty
  where
    go _ [] = []
    go seen (x:xs)
      | x `Set.member` seen = go seen xs
      | otherwise = x : go (Set.insert x seen) xs

-- | Split a list into chunks of the given size.
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf size values = take size values : chunksOf size (drop size values)

-- | Parse a decimal integer from text, returning 'Nothing' on failure.
readMaybeInt :: Text -> Maybe Integer
readMaybeInt t =
  case Text.words t of
    [w] -> readDecimal w
    _ -> Nothing
  where
    readDecimal :: Text -> Maybe Integer
    readDecimal val
      | Text.null val = Nothing
      | otherwise =
          let chars = Text.unpack val
          in if all (\c -> c >= '0' && c <= '9') chars
             then case reads (Text.unpack val) :: [(Integer, String)] of
                    [(n, "")] -> Just n
                    _ -> Nothing
             else Nothing

-- | Build a 'GitCommandFailed' error from a process result.
commandFailure :: Text -> GitProcessResult -> GitError
commandFailure operation result =
  GitCommandFailed
    operation
    (exitCodeNumber (processExitCode result))
    (boundedDiagnostic (processStdout result))
    (boundedDiagnostic (processStderr result))

exitCodeNumber :: ExitCode -> Int
exitCodeNumber ExitSuccess = 0
exitCodeNumber (ExitFailure value) = value

diagnosticLimit :: Int
diagnosticLimit = 4096

boundedDiagnostic :: ByteString -> Text
boundedDiagnostic raw =
  normalizeNewlines
    . TextEncoding.decodeUtf8With TextEncodingError.lenientDecode
    $ if BS8.length raw <= diagnosticLimit
      then raw
      else BS8.take diagnosticLimit raw <> "...[truncated]"
  where
    normalizeNewlines = Text.replace "\r" "\n" . Text.replace "\r\n" "\n"

-- | Convert a 'Digest' to a zero-padded 64-character SHA-256 hex string.
digestToHex :: Digest -> Text
digestToHex = Text.pack . concatMap byteToHex . BS.unpack . digestBytes
  where
    byteToHex byte =
      case showHex byte "" of
        [digit] -> ['0', digit]
        digits -> digits

-- | Return the first first-parent commit that adds each managed path on a
-- logical line.  Mirrors the Python @first_parent_path_landings()@ from
-- @adrai_core.gitops@.
--
-- Uses @git log --first-parent --reverse --diff-filter=A --no-renames@ to
-- find the earliest commit on the first-parent chain that introduces each
-- managed path, scoped to the given reference and optionally restricted to
-- paths under the supplied root directories.
firstParentPathLandings
  :: Repository
  -> Text           -- ^ Git ref (branch name) to walk
  -> [Text]         -- ^ Root directory paths to scope the walk (-- <roots>)
  -> IO (Either GitError (Map Text GitOid))
firstParentPathLandings repo ref roots = do
  let args = [ "log",
               "--first-parent",
               "--reverse",
               "--diff-filter=A",
               "--no-renames",
               "--format=%x1e%H",
               "--name-only",
               Text.unpack ref
             ] ++ if null roots then []
                  else ["--"] ++ map Text.unpack roots
  result <- runRepository repo "first parent path landings" args BS.empty
  pure $ do
    processResult <- result
    if processExitCode processResult /= ExitSuccess
      then Left (commandFailure "first parent path landings" processResult)
      else pure (decodeFirstParentPathLandings (processStdout processResult))

-- | Decode first-parent path landing output.
--
-- Format: @%x1e@ separated records, each containing one commit OID followed
-- by one path per line.
decodeFirstParentPathLandings :: ByteString -> Map Text GitOid
decodeFirstParentPathLandings raw
  | BS.null raw = Map.empty
  | otherwise =
      let text = TextEncoding.decodeUtf8 raw
          records = Text.splitOn "\x1e" text
          result = go Map.empty records
      in result
  where
    go :: Map Text GitOid -> [Text] -> Map Text GitOid
    go acc [] = acc
    go acc (record:rest) =
      case Text.stripStart record of
        "" -> go acc rest
        rec
          | Text.null rec -> go acc rest
          | otherwise ->
              let pathLines = Text.lines rec
                  filteredPaths = [Text.strip l | l <- pathLines, not (Text.null (Text.strip l))]
              in case filteredPaths of
                   [] -> go acc rest
                   (commitHash:pathLines') ->
                     case mkGitOid commitHash of
                       Left _ -> go acc rest
                       Right commitOid ->
                         let acc' = foldl (\m p ->
                                case Text.strip p of
                                  "" -> m
                                  path | Map.notMember path m -> Map.insert path commitOid m
                                       | otherwise -> m) acc pathLines'
                         in go acc' rest
