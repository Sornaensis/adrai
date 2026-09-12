-- | Public post-commit indexing surface. Trusted exact publication cloning is
-- internal-only; public cross-revision reuse requires an open 'CacheLease'.
module Adrai.Service.PostCommitIndex
  ( IndexWarning (..), PostCommitIndexError (..), PostCommitIndexPublishError (..),
    PostCommitIndexResult (..), PostCommitIndexDependencies (..),
    postCommitIndexDependencies, compilePostCommitIndex,
    compilePostCommitIndexWithAttribution, compilePostCommitIndexWithAttributionAndRefresh,
    compilePostCommitIndexWith,
    clonePostCommitIndexFromLeaseWithHistoryCountAndRefresh )
where
import Adrai.Service.PostCommitIndex.Internal
