{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Deliberately small, caller-owned attribution for a cold compilation.
--
-- The normal compiler uses 'inertColdCompileAttribution'.  In particular, the
-- inert observer does not allocate a counter, inspect the environment, read a
-- clock, or touch a path.  The file observer is intentionally opt-in and is
-- suitable only for the stress harness' explicitly supplied output path.
module Adrai.Compiler.Attribution
  ( AttributionPhase (..),
    AttributionCounter (..),
    ColdCompileAttribution,
    AttributionDependencies (..),
    defaultAttributionDependencies,
    inertColdCompileAttribution,
    newFileColdCompileAttribution,
    newFileColdCompileAttributionWith,
    closeColdCompileAttribution,
    attributionEnabled,
    withAttributionPhase,
    withAttributionEitherPhase,
    recordAttributionCounter,
    forceAttributionValue,
    appendAttributionEvidence,
    AttributionArtifact (..),
    AttributionRow (..),
    parseAttributionArtifact,
  )
where

import Control.Exception (bracketOnError, evaluate, mask, onException, throwIO)
import Control.Monad (when)
import Data.Char (isDigit)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as Text
import GHC.Clock (getMonotonicTimeNSec)
import System.IO (Handle, IOMode (AppendMode, WriteMode), hClose, hFlush, hPutStrLn, openFile, withFile)
import Text.Read (readMaybe)

data AttributionPhase
  = CliPreflight
  | PreflightCurrentObservation
  | CurrentTreeBlobObservation
  | HistoryGraphEnumeration
  | HistoryPathSelectionDiff
  | HistoryReplayParse
  | BasisChecks
  | AnalysisGate
  | SearchMaterializationPhase
  | Fingerprinting
  | SqliteSchema
  | ManagedSourceRestream
  | SemanticInserts
  | SearchInserts
  | FtsInserts
  | Verification
  | CompileOutcomeEvaluation
  | DatabaseClose
  | PostCloseProvenanceRefresh
  | PostCloseFingerprintValidation
  | ImmutablePublication
  | CurrentAliasCopy
  | CacheSelection
  | CacheReuseProof
  | CliRevisionResolution
  | SqliteMetadataInitial
  | SqliteMetadataFinal
  deriving (Eq, Ord, Enum, Bounded, Show)

data AttributionCounter
  = CounterNodes
  | CounterEdges
  | CounterSelectedNodes
  | CounterChanges
  | CounterBlobRequests
  | CounterParsedBlobs
  | CounterCurrentEntries
  | CounterBytes
  | CounterRows
  deriving (Eq, Ord, Enum, Bounded, Show)

data ColdCompileAttribution
  = InertAttribution
  | FileAttribution !(IORef AttributionState)

-- | The file observer's effects are explicit so its fail-closed lifecycle can
-- be exercised without depending on platform-specific broken-handle tricks.
-- This seam is used only by unit tests; production uses
-- 'defaultAttributionDependencies'.
data AttributionDependencies = AttributionDependencies
  { attributionOpenFile :: FilePath -> IO Handle,
    attributionWriteLine :: Handle -> String -> IO (),
    attributionFlush :: Handle -> IO (),
    attributionClose :: Handle -> IO (),
    attributionMonotonicNs :: IO Integer
  }

data AttributionState = AttributionState
  { attributionHandle :: !Handle,
    attributionDependencies :: !AttributionDependencies,
    attributionNextSequence :: !Integer,
    attributionOpenPhase :: !(Maybe (AttributionPhase, Integer)),
    attributionCounters :: !(Map AttributionCounter Integer)
  }

data AttributionArtifact = AttributionArtifact
  { attributionArtifactRows :: [AttributionRow]
  }
  deriving (Eq, Show)

data AttributionRow
  = AttributionStart !Integer !AttributionPhase !Integer
  | AttributionCounterRow !Integer !AttributionPhase !AttributionCounter !Integer
  | AttributionEnd !Integer !AttributionPhase !Integer !Integer !Integer !Bool
  | AttributionEvidence !Integer !String !Integer
  deriving (Eq, Show)

artifactHeader :: String
artifactHeader = "adrai-cold-compile-attribution-v1"

inertColdCompileAttribution :: ColdCompileAttribution
inertColdCompileAttribution = InertAttribution

attributionEnabled :: ColdCompileAttribution -> Bool
attributionEnabled InertAttribution = False
attributionEnabled (FileAttribution _) = True

defaultAttributionDependencies :: AttributionDependencies
defaultAttributionDependencies =
  AttributionDependencies
    { attributionOpenFile = flip openFile WriteMode,
      attributionWriteLine = hPutStrLn,
      attributionFlush = hFlush,
      attributionClose = hClose,
      attributionMonotonicNs = fromIntegral <$> getMonotonicTimeNSec
    }

-- | Opening, writing, flushing, and closing are all deliberately fatal in the
-- opt-in mode.  A partial profiling artifact must never be mistaken for a
-- successful observation.
newFileColdCompileAttribution :: FilePath -> IO ColdCompileAttribution
newFileColdCompileAttribution = newFileColdCompileAttributionWith defaultAttributionDependencies

newFileColdCompileAttributionWith :: AttributionDependencies -> FilePath -> IO ColdCompileAttribution
newFileColdCompileAttributionWith dependencies path =
  bracketOnError (attributionOpenFile dependencies path) (attributionClose dependencies) $ \handle -> do
    attributionWriteLine dependencies handle artifactHeader
    attributionFlush dependencies handle
    state <- newIORef (AttributionState handle dependencies 1 Nothing Map.empty)
    pure (FileAttribution state)

-- | Close the caller-owned artifact after the compilation terminates.  This is
-- deliberately a no-op for the normal path; an I/O failure while closing the
-- explicitly requested artifact remains visible to the caller.
closeColdCompileAttribution :: ColdCompileAttribution -> IO ()
closeColdCompileAttribution InertAttribution = pure ()
closeColdCompileAttribution (FileAttribution stateRef) = do
  state <- readAttributionState stateRef
  attributionClose (attributionDependencies state) (attributionHandle state)

withAttributionPhase :: ColdCompileAttribution -> AttributionPhase -> IO value -> IO value
withAttributionPhase InertAttribution _ action = action
withAttributionPhase (FileAttribution stateRef) phase action = mask $ \restore -> do
  started <- attributionMonotonicNs . attributionDependencies =<< readAttributionState stateRef
  appendStart stateRef phase started
  result <- restore action `onException` finish False
  -- Attribute evaluation to the named phase as well as the IO that produced
  -- it.  The inert observer deliberately keeps the old non-strict behaviour.
  forced <- restore (evaluate result) `onException` finish False
  finish True
  pure forced
  where
    finish succeeded = do
      finished <- attributionMonotonicNs . attributionDependencies =<< readAttributionState stateRef
      appendEnd stateRef phase finished succeeded

-- | Like 'withAttributionPhase', but mark the recorded phase failed when a
-- typed compiler result is 'Left'.  The result itself is preserved exactly.
withAttributionEitherPhase :: ColdCompileAttribution -> AttributionPhase -> IO (Either problem value) -> IO (Either problem value)
withAttributionEitherPhase InertAttribution _ action = action
withAttributionEitherPhase (FileAttribution stateRef) phase action = mask $ \restore -> do
  started <- attributionMonotonicNs . attributionDependencies =<< readAttributionState stateRef
  appendStart stateRef phase started
  result <- restore action `onException` finish False
  forced <- restore (evaluate result) `onException` finish False
  finish (either (const False) (const True) forced)
  pure forced
  where
    finish succeeded = do
      finished <- attributionMonotonicNs . attributionDependencies =<< readAttributionState stateRef
      appendEnd stateRef phase finished succeeded

recordAttributionCounter :: ColdCompileAttribution -> AttributionCounter -> Integer -> IO ()
recordAttributionCounter InertAttribution _ _ = pure ()
recordAttributionCounter (FileAttribution stateRef) counter value
  | value < 0 = throwIO (userError "cold compile attribution counter is negative")
  | otherwise =
      atomicModifyIORef' stateRef $ \state ->
        case attributionOpenPhase state of
          Nothing -> error "cold compile attribution counter has no open phase"
          Just _ ->
            let counters = Map.insertWith (+) counter value (attributionCounters state)
             in (state {attributionCounters = counters}, ())

-- | Only an enabled profile pays this forcing boundary.  The caller supplies
-- the normal form it needs; ordinary compilation leaves evaluation unchanged.
forceAttributionValue :: ColdCompileAttribution -> (value -> ()) -> value -> IO value
forceAttributionValue InertAttribution _ value = pure value
forceAttributionValue (FileAttribution _) forceValue value = forceValue value `seq` pure value

-- | Append harness-owned evidence only after the compiler child has closed its
-- artifact.  The parser keeps these rows as a terminal, uniquely named suffix,
-- so a partial or stale profile cannot masquerade as a complete observation.
appendAttributionEvidence :: FilePath -> [(String, Integer)] -> IO ()
appendAttributionEvidence path evidence = do
  raw <- readFile path
  artifact <- either (ioError . userError) pure (parseAttributionArtifact raw)
  let rows = attributionArtifactRows artifact
      names = map fst evidence
      nextSequence = fromIntegral (length rows + 1)
  when (null evidence || any (null . fst) evidence || any (any (== '\t') . fst) evidence || any ((< 0) . snd) evidence) $
    throwIO (userError "cold compile attribution evidence is malformed")
  when (length names /= Set.size (Set.fromList names)) $
    throwIO (userError "cold compile attribution evidence names are duplicated")
  when (any isEvidence rows) $
    throwIO (userError "cold compile attribution already contains terminal evidence")
  withFile path AppendMode $ \handle -> do
    mapM_ (hPutStrLn handle . renderEvidence nextSequence) (zip evidence [0 :: Integer ..])
    hFlush handle
  where
    isEvidence (AttributionEvidence _ _ _) = True
    isEvidence _ = False
    renderEvidence first ((name, value), offset) =
      Text.unpack (Text.intercalate "\t" ["evidence", decimal (first + offset), Text.pack name, decimal value])

appendStart :: IORef AttributionState -> AttributionPhase -> Integer -> IO ()
appendStart stateRef phase started = do
  writeStart <- atomicModifyIORef' stateRef $ \state ->
    case attributionOpenPhase state of
      Just _ -> error "cold compile attribution phases may not overlap"
      Nothing ->
        let sequenceNumber = attributionNextSequence state
            handle = attributionHandle state
            dependencies = attributionDependencies state
            next = state {attributionNextSequence = sequenceNumber + 1, attributionOpenPhase = Just (phase, started), attributionCounters = Map.empty}
         in (next, attributionWriteLine dependencies handle (renderStart sequenceNumber phase started) >> attributionFlush dependencies handle)
  writeStart

appendEnd :: IORef AttributionState -> AttributionPhase -> Integer -> Bool -> IO ()
appendEnd stateRef phase finished succeeded = do
  writeEnd <- atomicModifyIORef' stateRef $ \state ->
    case attributionOpenPhase state of
      Just (open, started) | open == phase ->
        let handle = attributionHandle state
            dependencies = attributionDependencies state
            startSequence = attributionNextSequence state
            counters = Map.toAscList (attributionCounters state)
            counterRows = zipWith (renderCounter phase) [startSequence ..] counters
            endSequence = startSequence + fromIntegral (length counterRows)
            next = state {attributionNextSequence = endSequence + 1, attributionOpenPhase = Nothing, attributionCounters = Map.empty}
            writeRows = mapM_ (attributionWriteLine dependencies handle) counterRows >> attributionWriteLine dependencies handle (renderEnd endSequence phase started finished succeeded) >> attributionFlush dependencies handle
         in (next, writeRows)
      _ -> error "cold compile attribution phase end does not match its start"
  writeEnd

phaseToken :: AttributionPhase -> String
phaseToken = Text.unpack . Text.toLower . Text.pack . show

counterToken :: AttributionCounter -> String
counterToken = Text.unpack . Text.toLower . Text.pack . drop 7 . show

renderStart :: Integer -> AttributionPhase -> Integer -> String
renderStart sequenceNumber phase started = Text.unpack (Text.intercalate "\t" ["start", decimal sequenceNumber, Text.pack (phaseToken phase), decimal started])

renderCounter :: AttributionPhase -> Integer -> (AttributionCounter, Integer) -> String
renderCounter phase sequenceNumber (counter, value) = Text.unpack (Text.intercalate "\t" ["counter", decimal sequenceNumber, Text.pack (phaseToken phase), Text.pack (counterToken counter), decimal value])

renderEnd :: Integer -> AttributionPhase -> Integer -> Integer -> Bool -> String
renderEnd sequenceNumber phase started finished succeeded = Text.unpack (Text.intercalate "\t" ["end", decimal sequenceNumber, Text.pack (phaseToken phase), decimal started, decimal finished, decimal (finished - started), if succeeded then "ok" else "failed"])

decimal :: Integer -> Text
decimal = Text.pack . show

parseAttributionArtifact :: String -> Either String AttributionArtifact
parseAttributionArtifact raw = do
  rows <- case map dropLineEndingCarriageReturn (lines raw) of
    header : remaining | header == artifactHeader -> traverse parseRow remaining
    _ -> Left "cold compile attribution header is missing or unsupported"
  when (null rows) (Left "cold compile attribution contains no rows")
  when (length rows > maximumAttributionRows) (Left "cold compile attribution has too many rows")
  validateRows rows
  pure (AttributionArtifact rows)

-- 'openFile' uses the platform text mode.  On Windows that means each flushed
-- record is CRLF; normalise only the terminal CR left by 'lines', not arbitrary
-- carriage returns within a token.
dropLineEndingCarriageReturn :: String -> String
dropLineEndingCarriageReturn line =
  case reverse line of
    '\r' : remaining -> reverse remaining
    _ -> line

parseRow :: String -> Either String AttributionRow
parseRow line =
  case splitTabs line of
    ["start", sequenceNumber, phase, started] -> AttributionStart <$> parseNatural sequenceNumber <*> parsePhase phase <*> parseNatural started
    ["counter", sequenceNumber, phase, counter, value] -> AttributionCounterRow <$> parseNatural sequenceNumber <*> parsePhase phase <*> parseCounter counter <*> parseNatural value
    ["end", sequenceNumber, phase, started, finished, elapsed, outcome] -> AttributionEnd <$> parseNatural sequenceNumber <*> parsePhase phase <*> parseNatural started <*> parseNatural finished <*> parseNatural elapsed <*> parseOutcome outcome
    ["evidence", sequenceNumber, name, value] -> AttributionEvidence <$> parseNatural sequenceNumber <*> parseEvidenceName name <*> parseNatural value
    _ -> Left "cold compile attribution row is malformed"

maximumAttributionRows :: Int
maximumAttributionRows = 128

validateRows :: [AttributionRow] -> Either String ()
validateRows = go 1 Nothing Set.empty Set.empty Set.empty False
  where
    go _ Nothing _ _ _ _ [] = Right ()
    go _ (Just _) _ _ _ _ [] = Left "cold compile attribution ends with an unfinished phase"
    go expected open seenPhases seenCounters seenEvidence evidenceStarted (row : remaining)
      | rowSequence row /= expected = Left "cold compile attribution sequence is not strictly monotonic"
      | otherwise = case row of
          AttributionStart _ phase started ->
            if evidenceStarted
              then Left "cold compile attribution phase follows terminal evidence"
              else case open of
                Nothing
                  | Set.member phase seenPhases -> Left "cold compile attribution phase is duplicated"
                  | otherwise -> go (expected + 1) (Just (phase, started)) (Set.insert phase seenPhases) seenCounters seenEvidence False remaining
                Just _ -> Left "cold compile attribution phases overlap"
          AttributionCounterRow _ phase counter _ ->
            if evidenceStarted
              then Left "cold compile attribution counter follows terminal evidence"
              else if fmap fst open == Just phase
              then
                let key = (phase, counter)
                 in if Set.member key seenCounters
                      then Left "cold compile attribution counter is duplicated"
                      else go (expected + 1) open seenPhases (Set.insert key seenCounters) seenEvidence False remaining
              else Left "cold compile attribution counter is outside its phase"
          AttributionEnd _ phase started finished elapsed _ ->
            if evidenceStarted
              then Left "cold compile attribution phase end follows terminal evidence"
              else case open of
                Just (openPhase, openStarted)
                  | openPhase == phase && openStarted == started && finished >= started && elapsed == finished - started -> go (expected + 1) Nothing seenPhases seenCounters seenEvidence False remaining
                _ -> Left "cold compile attribution end does not match its start"
          AttributionEvidence _ name _ ->
            case open of
              Just _ -> Left "cold compile attribution evidence is inside a phase"
              Nothing
                | Set.member name seenEvidence -> Left "cold compile attribution evidence is duplicated"
                | otherwise -> go (expected + 1) Nothing seenPhases seenCounters (Set.insert name seenEvidence) True remaining

rowSequence :: AttributionRow -> Integer
rowSequence = \case
  AttributionStart sequenceNumber _ _ -> sequenceNumber
  AttributionCounterRow sequenceNumber _ _ _ -> sequenceNumber
  AttributionEnd sequenceNumber _ _ _ _ _ -> sequenceNumber
  AttributionEvidence sequenceNumber _ _ -> sequenceNumber

parseNatural :: String -> Either String Integer
parseNatural value
  | null value || any (not . isDigit) value = Left "cold compile attribution integer is malformed"
  | otherwise = maybe (Left "cold compile attribution integer is malformed") Right (readMaybe value)

parsePhase :: String -> Either String AttributionPhase
parsePhase token =
  maybe (Left "cold compile attribution phase is unknown") Right
    (lookup token [(phaseToken phase, phase) | phase <- [minBound .. maxBound]])

parseCounter :: String -> Either String AttributionCounter
parseCounter token =
  maybe (Left "cold compile attribution counter is unknown") Right
    (lookup token [(counterToken counter, counter) | counter <- [minBound .. maxBound]])

parseOutcome :: String -> Either String Bool
parseOutcome "ok" = Right True
parseOutcome "failed" = Right False
parseOutcome _ = Left "cold compile attribution outcome is malformed"

parseEvidenceName :: String -> Either String String
parseEvidenceName value
  | null value || any (\character -> character == '\t' || character == '\r' || character == '\n') value = Left "cold compile attribution evidence name is malformed"
  | otherwise = Right value

splitTabs :: String -> [String]
splitTabs = foldr step [""]
  where
    step '\t' values = "" : values
    step character (value : values) = (character : value) : values
    step _ [] = error "splitTabs requires an initial accumulator"

readAttributionState :: IORef AttributionState -> IO AttributionState
readAttributionState stateRef = atomicModifyIORef' stateRef (\state -> (state, state))
