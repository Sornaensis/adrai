{-# LANGUAGE StrictData #-}

-- | Immutable, source-authorized evidence used only to bound provenance
-- recovery for a descendant target.  It is deliberately separate from the
-- semantic/search payload: callers must still build and publish the current
-- target's semantic snapshot.
module Adrai.Provenance.RecoveryWitness
  ( ProvenanceRecoveryWitness (..),
  )
where

import Adrai.Git (GitOid)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)

data ProvenanceRecoveryWitness = ProvenanceRecoveryWitness
  { recoveryWitnessSourceTarget :: GitOid,
    recoveryWitnessSignatures :: Map Text Text,
    recoveryWitnessCoveredOperations :: Set Text,
    recoveryWitnessPlacedOperations :: Set Text,
    recoveryWitnessSourceReachability :: Set GitOid,
    recoveryWitnessPlacements :: Set (Text, Text, Text, Integer, Integer, Text, Text),
    recoveryWitnessIssues :: Set (Text, Text, Text, Maybe Text, Maybe Text, Maybe Text, Text),
    recoveryWitnessLandings :: Set (Text, Text, Text, Text, Text, Integer),
    recoveryWitnessLineConfigs :: Set (Text, Text)
  }
  deriving (Eq, Show)
