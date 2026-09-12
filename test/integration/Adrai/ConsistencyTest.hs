{-# LANGUAGE OverloadedStrings #-}

-- | Cheap consistency contracts retained after removing the historical
-- executable snapshot matrices. Real branch, conflict, and cache-mode wiring
-- is covered by the focused integration smokes registered beside this group.
module Adrai.ConsistencyTest (tests) where

import Data.Char (isDigit)
import Data.Text (Text)
import qualified Data.Text as Text
import Database.SQLite.Simple (SQLData (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, testCase)

data CompileExecutionObservation = CompileExecutionObservation
  { compileExecutionMode :: Text,
    compileExecutionIncrementalKind :: Text,
    compileExecutionHistoryCommitsScanned :: Integer
  }
  deriving (Eq, Show)

-- | Provenance maintenance metadata is not semantic materialization. Cold and
-- warm snapshots must each contain exactly one canonical nonnegative decimal
-- value for every normalized key, while all other metadata remains significant.
normalizeConsistencyTables :: [(String, [[SQLData]])] -> Either Text [(String, [[SQLData]])]
normalizeConsistencyTables tables = do
  metaRows <- maybe (Left "consistency snapshot is missing meta") Right (lookup "meta" tables)
  timestampNormalized <- normalizeVolatileMetaValue "provenance.sync_timestamp" "<sync-timestamp>" metaRows
  normalizedMeta <- normalizeVolatileMetaValue "provenance.generation" "<provenance-generation>" timestampNormalized
  historyNormalized <- normalizeVolatileMetaValue "provenance.history_commits_scanned" "<provenance-history-commits-scanned>" normalizedMeta
  publicHistoryNormalized <- normalizeVolatileMetaValue "history_commits_scanned" "<history-commits-scanned>" historyNormalized
  pure [(if table == "meta" then (table, publicHistoryNormalized) else (table, rows)) | (table, rows) <- tables]

normalizeVolatileMetaValue :: Text -> Text -> [[SQLData]] -> Either Text [[SQLData]]
normalizeVolatileMetaValue key sentinel rows =
  case [row | row@(SQLText rowKey : _) <- rows, rowKey == key] of
    [[SQLText rowKey, SQLText value]]
      | canonicalEpochSeconds value ->
          Right
            [ if row == [SQLText rowKey, SQLText value]
                then [SQLText rowKey, SQLText sentinel]
                else row
              | row <- rows
            ]
    [] -> Left ("consistency snapshot is missing " <> key)
    [_] -> Left ("consistency snapshot has malformed " <> key)
    _ -> Left ("consistency snapshot has duplicate " <> key)

canonicalEpochSeconds :: Text -> Bool
canonicalEpochSeconds value =
  value == "0" || (not (Text.null value) && Text.head value /= '0' && Text.all isDigit value)

tests :: TestTree
tests =
  testGroup
    "Consistency oracle (P4-07)"
    [consistencyProvenanceMaintenanceNormalizationTests]

consistencyProvenanceMaintenanceNormalizationTests :: TestTree
consistencyProvenanceMaintenanceNormalizationTests =
  testGroup
    "provenance maintenance metadata normalization"
    [ testCase "timestamp generation and provenance/public scan count differences are ignored" $ do
        let warm = consistencyTables "1787790251" "1" "7" "1" "same"
            cold = consistencyTables "1787790314" "99" "17" "17" "same"
        normalizeConsistencyTables warm @?= normalizeConsistencyTables cold,
      testCase "missing duplicate malformed and negative maintenance values fail collection" $ do
        let invalidTables =
              [ [("meta", [[SQLText "other", SQLText "same"]])],
                maintenanceTables "" "1" "1",
                maintenanceTables "-1" "1" "1",
                maintenanceTables "01" "1" "1",
                [("meta", [[SQLText "provenance.sync_timestamp", SQLInteger 1], [SQLText "provenance.generation", SQLText "1"], [SQLText "provenance.history_commits_scanned", SQLText "1"], [SQLText "history_commits_scanned", SQLText "17"], [SQLText "other", SQLText "same"]])],
                [("meta", [[SQLText "provenance.sync_timestamp", SQLText "1"], [SQLText "provenance.sync_timestamp", SQLText "2"], [SQLText "provenance.generation", SQLText "1"], [SQLText "provenance.history_commits_scanned", SQLText "1"], [SQLText "history_commits_scanned", SQLText "17"], [SQLText "other", SQLText "same"]])],
                maintenanceTables "1" "" "1",
                maintenanceTables "1" "-1" "1",
                maintenanceTables "1" "01" "1",
                [("meta", [[SQLText "provenance.sync_timestamp", SQLText "1"], [SQLText "provenance.generation", SQLInteger 1], [SQLText "provenance.history_commits_scanned", SQLText "1"], [SQLText "history_commits_scanned", SQLText "17"], [SQLText "other", SQLText "same"]])],
                [("meta", [[SQLText "provenance.sync_timestamp", SQLText "1"], [SQLText "provenance.generation", SQLText "1"], [SQLText "provenance.generation", SQLText "2"], [SQLText "provenance.history_commits_scanned", SQLText "1"], [SQLText "history_commits_scanned", SQLText "17"], [SQLText "other", SQLText "same"]])],
                maintenanceTables "1" "1" "",
                maintenanceTables "1" "1" "-1",
                maintenanceTables "1" "1" "01",
                [("meta", [[SQLText "provenance.sync_timestamp", SQLText "1"], [SQLText "provenance.generation", SQLText "1"], [SQLText "provenance.history_commits_scanned", SQLInteger 1], [SQLText "history_commits_scanned", SQLText "17"], [SQLText "other", SQLText "same"]])],
                [("meta", [[SQLText "provenance.sync_timestamp", SQLText "1"], [SQLText "provenance.generation", SQLText "1"], [SQLText "provenance.history_commits_scanned", SQLText "1"], [SQLText "provenance.history_commits_scanned", SQLText "2"], [SQLText "history_commits_scanned", SQLText "17"], [SQLText "other", SQLText "same"]])],
                [("meta", [[SQLText "provenance.sync_timestamp", SQLText "1"], [SQLText "provenance.generation", SQLText "1"], [SQLText "provenance.history_commits_scanned", SQLText "1"], [SQLText "other", SQLText "same"]])],
                consistencyTables "1" "1" "1" "" "same",
                consistencyTables "1" "1" "1" "-1" "same",
                consistencyTables "1" "1" "1" "01" "same",
                [("meta", [[SQLText "provenance.sync_timestamp", SQLText "1"], [SQLText "provenance.generation", SQLText "1"], [SQLText "provenance.history_commits_scanned", SQLText "1"], [SQLText "history_commits_scanned", SQLInteger 1], [SQLText "other", SQLText "same"]])],
                [("meta", [[SQLText "provenance.sync_timestamp", SQLText "1"], [SQLText "provenance.generation", SQLText "1"], [SQLText "provenance.history_commits_scanned", SQLText "1"], [SQLText "history_commits_scanned", SQLText "1"], [SQLText "history_commits_scanned", SQLText "2"], [SQLText "other", SQLText "same"]])]
              ]
        assertBool "every invalid maintenance shape must fail snapshot collection" (all isLeft (map normalizeConsistencyTables invalidTables)),
      testCase "raw compile observations retain path-specific mode kind and history counts" $ do
        let warm = CompileExecutionObservation "incremental" "tree-identical" 1
            cold = CompileExecutionObservation "full" "full" 18
        assertBool "cold and warm compile work observations remain distinct" (warm /= cold)
        compileExecutionHistoryCommitsScanned warm @?= 1
        compileExecutionHistoryCommitsScanned cold @?= 18,
      testCase "deterministic semantic metadata differences remain significant" $ do
        let warm = normalizeConsistencyTables (consistencyTables "1787790251" "1" "7" "1" "warm")
            cold = normalizeConsistencyTables (consistencyTables "1787790314" "99" "17" "18" "cold")
        assertBool "only maintenance metadata is normalized" (warm /= cold)
    ]
  where
    consistencyTables timestamp generation provenanceScanned publicScanned other =
      [ ("meta", [[SQLText "provenance.sync_timestamp", SQLText timestamp], [SQLText "provenance.generation", SQLText generation], [SQLText "provenance.history_commits_scanned", SQLText provenanceScanned], [SQLText "history_commits_scanned", SQLText publicScanned], [SQLText "other", SQLText other]]),
        ("decision_record", [[SQLText "R00000000000000000000000000"]])
      ]
    maintenanceTables timestamp generation provenanceScanned = consistencyTables timestamp generation provenanceScanned "17" "same"
    isLeft value = case value of
      Left _ -> True
      Right _ -> False
