{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Repository-bound WAI application.  Admission runs before route parsing or
-- body buffering, and every response carries process generation/snapshot
-- metadata.
module Adrai.Web.Application
  ( ApplicationRuntime,
    ApplicationServices (..),
    newApplicationRuntime,
    stopApplicationRuntime,
    applicationEventCoordinator,
    applicationActiveFileRegistry,
    subscribeApplicationEvents,
    publishWatcherEvent,
    defaultApplicationServices,
    webSocketUpgradeAdmitted,
    webApplication,
  )
where

import qualified Adrai.CliTypes as CliTypes
import Adrai.Compiler.CacheSelection (ExactArchiveBusy (..))
import Adrai.Git
  ( GitError (..),
    GitHeadState (..),
    GitOid,
    Repository,
    RevisionSpec (..),
    gitOidText,
    repositoryHeadState,
    resolveRevision,
  )
import qualified Adrai.Graph as Graph
import Adrai.History (ReadSnapshot (..))
import Adrai.Query (RelevantRequest (..))
import Adrai.Repository
  ( repositorySnapshot,
    repositorySnapshotManagedPaths,
  )
import Adrai.Provenance.Git.Lock (GitLockError (..), withGitLock)
import qualified Adrai.Service.Mutation as Mutation
import qualified Adrai.Service.PostCommitIndex as PostCommit
import qualified Adrai.Service.Compilation as Compilation
import qualified Adrai.Service.Query as Query
import qualified Adrai.Service.Runtime as Runtime
import Adrai.Service.Transaction
  ( ExpectedRepositoryBasis (..),
    TransactionError (..),
  )
import Adrai.Types (RevisionSelector (..), gitRefText)
import qualified Adrai.Types as Types
import qualified Adrai.Web.Api as Api
import Adrai.Web.Assets (applicationCss, applicationJavaScript, indexHtml)
import qualified Adrai.Web.Security as Security
import qualified Adrai.Web.Events as Events
import qualified Adrai.Web.Watch as Watch
import Adrai.Web.Socket (EventsTransport (..))
import Control.Exception
  ( SomeAsyncException,
    SomeException,
    catch,
    displayException,
    fromException,
    mask,
    onException,
    throwIO,
    try,
  )
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar)
import Control.Concurrent.STM (atomically)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LazyByteString
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import qualified Data.Text.Encoding.Error as TextEncodingError
import qualified Network.HTTP.Types as Http
import qualified Network.HTTP.Types.Header as Header
import System.Timeout (timeout)
import Network.Wai
  ( Application,
    Request,
    Response,
    pathInfo,
    getRequestBodyChunk,
    queryString,
    rawQueryString,
    requestHeaders,
    requestMethod,
    responseLBS,
  )

data ApplicationServices = ApplicationServices
  { dispatchApplicationRequest :: Compilation.CompilationCoordinator -> (Repository -> GitOid -> IO (Either Text FilePath)) -> IO () -> Api.Generation -> IO Api.Generation -> IO () -> (GitOid -> IO Api.Generation) -> Api.Repo -> Api.ApiRequest -> IO (Either Api.ApiError (Api.ApiResult, Api.ResponseAsOf, Api.Generation)),
    applicationCompileExact :: Repository -> GitOid -> IO (Either Text FilePath),
    applicationAfterCompilationJoin :: IO (),
    applicationAfterQueryResolution :: IO (),
    applicationBeforeCommitPublication :: GitOid -> IO ()
  }

data ApplicationRuntime = ApplicationRuntime
  { applicationRepo :: Api.Repo,
    applicationAuthority :: Security.BoundAuthority,
    applicationSecret :: Security.ProcessSecret,
    applicationLimits :: Api.ApiLimits,
    applicationCoordinator :: Events.EventCoordinator,
    applicationCompilation :: Compilation.CompilationCoordinator,
    applicationActiveFiles :: Watch.ActiveFileRegistry,
    applicationServices :: ApplicationServices,
    applicationEvents :: EventsTransport
  }

newApplicationRuntime :: Api.Repo -> Security.BoundAuthority -> Security.ProcessSecret -> Api.ApiLimits -> ApplicationServices -> EventsTransport -> IO ApplicationRuntime
newApplicationRuntime repo authority secret limits services events = do
  coordinator <- Events.newEventCoordinator
  compilation <- Compilation.newCompilationCoordinator
  activeFiles <- Watch.newActiveFileRegistry
  pure (ApplicationRuntime repo authority secret limits coordinator compilation activeFiles services events)

stopApplicationRuntime :: ApplicationRuntime -> IO ()
stopApplicationRuntime = Compilation.stopCompilationCoordinator . applicationCompilation

applicationEventCoordinator :: ApplicationRuntime -> Events.EventCoordinator
applicationEventCoordinator = applicationCoordinator

applicationActiveFileRegistry :: ApplicationRuntime -> Watch.ActiveFileRegistry
applicationActiveFileRegistry = applicationActiveFiles

defaultApplicationServices :: ApplicationServices
defaultApplicationServices = ApplicationServices dispatchProduction Runtime.ensureExactArchive (pure ()) (pure ()) (const (pure ()))

-- | Decide from the original WAI request whether it may enter the WebSocket
-- adapter.  Rejected upgrade attempts stay in the ordinary application so its
-- versioned error envelope and metadata remain authoritative.
webSocketUpgradeAdmitted :: ApplicationRuntime -> Request -> Bool
webSocketUpgradeAdmitted runtime request =
  requestMethod request == Http.methodGet
    && pathInfo request == ["api", "v1", "events"]
    && encodedQueryBytes request <= Api.maximumQueryBytes (applicationLimits runtime)
    && case Security.admitRequest (applicationAuthority runtime) (applicationSecret runtime) (rawSecurityRequest runtime request) of
      Right _ -> True
      Left _ -> False

webApplication :: ApplicationRuntime -> Application
webApplication runtime request respond =
  webApplicationWithGeneration runtime request respond `catch` \Events.GenerationExhausted ->
    respond (generationExhaustedResponse (Api.ResponseMetadata (Api.mkGeneration maxBound) (Api.AsOfUnavailable "generation-exhausted")))

webApplicationWithGeneration :: ApplicationRuntime -> Application
webApplicationWithGeneration runtime request respond
  | encodedQueryBytes request > Api.maximumQueryBytes (applicationLimits runtime) =
      case Security.admitRequest (applicationAuthority runtime) (applicationSecret runtime) (rawSecurityRequestWithoutQuery runtime request) of
        Left problem -> respond (errorResponse preAuthenticationMetadata (securityApiError problem))
        Right _ -> respond (errorResponse preAuthenticationMetadata queryTooLarge)
  | otherwise = do
      let securityRequest = rawSecurityRequest runtime request
      case Security.admitRequest (applicationAuthority runtime) (applicationSecret runtime) securityRequest of
        Left problem -> respond (errorResponse preAuthenticationMetadata (securityApiError problem))
        Right credential ->
          if null (pathInfo request)
            then do
              metadata <- unavailableMetadata runtime "static-resource"
              respond (assetResponse metadata "text/html; charset=utf-8" (bootstrapCookie credential) indexHtml)
            else if pathInfo request == ["app.css"]
              then unavailableMetadata runtime "static-resource" >>= \metadata -> respond (assetResponse metadata "text/css; charset=utf-8" [] applicationCss)
              else if pathInfo request == ["app.js"]
                then unavailableMetadata runtime "static-resource" >>= \metadata -> respond (assetResponse metadata "text/javascript; charset=utf-8" [] applicationJavaScript)
                else handleApi runtime request respond
  where
    preAuthenticationMetadata = Api.ResponseMetadata (Api.mkGeneration 0) (Api.AsOfUnavailable "pre-authentication")
    queryTooLarge = Api.ApiError Api.MalformedInput 413 "query-too-large" "encoded query exceeds the configured bound"

handleApi :: ApplicationRuntime -> Request -> (Response -> IO value) -> IO value
handleApi runtime request respond = do
  generation <- nextGeneration runtime
  let metadata = Api.ResponseMetadata generation (Api.AsOfUnavailable "request-failed")
  case httpMethod request of
    method ->
      case Api.parseApiRoute method (pathInfo request) of
        Left problem -> respond (errorResponse metadata problem)
        Right route
          | route == Api.EventsRoute -> do
              let eventMetadata = Api.ResponseMetadata generation (Api.AsOfUnavailable "events-transport-unavailable")
              supplied <- runEventsTransport (applicationEvents runtime) request eventMetadata
              respond (maybe (errorResponse eventMetadata (Api.ApiError Api.ServiceFailure 503 "events-unavailable" "live repository events are not available in this increment")) id supplied)
          | otherwise -> do
              case strictQuery request of
                Left problem -> respond (errorResponse metadata problem)
                Right values -> do
                  body <- boundedBody (Api.maximumRequestBytes (applicationLimits runtime)) request
                  case body of
                    Left problem -> respond (errorResponse metadata problem)
                    Right bytes ->
                      case Api.decodeApiRequest (applicationLimits runtime) route values bytes of
                        Left problem -> respond (errorResponse metadata problem)
                        Right decoded -> do
                          let services = applicationServices runtime
                              publish oid = do
                                applicationBeforeCommitPublication services oid
                                envelope <- Events.publishInvalidation (applicationCoordinator runtime) (Events.EventAt oid) [minBound .. maxBound]
                                pure (Api.mkGeneration (Events.eventGeneration envelope))
                          dispatched <- trySynchronous (dispatchApplicationRequest services (applicationCompilation runtime) (applicationCompileExact services) (applicationAfterCompilationJoin services) generation (nextGeneration runtime) (applicationAfterQueryResolution services) publish (applicationRepo runtime) decoded)
                          case dispatched of
                            Left exception -> case fromException exception of
                              Just Events.GenerationExhausted -> respond (generationExhaustedResponse metadata)
                              Nothing -> case fromException exception of
                                Just ExactArchiveBusy -> respond (errorResponse metadata (Api.ApiError Api.ServiceFailure 503 "repository-busy" "exact archive is temporarily busy"))
                                Nothing -> respond (errorResponse metadata (Api.serviceFailure (Text.pack (displayException exception))))
                            Right (Left problem) -> respond (errorResponse metadata problem)
                            Right (Right (payload, asOf, observedGeneration)) ->
                              let responseMetadata = Api.ResponseMetadata observedGeneration asOf
                               in respond (jsonResponse Http.status200 (Api.responseJson (Api.ApiResponse responseMetadata payload)) (Api.responseHeaders responseMetadata))

dispatchProduction :: Compilation.CompilationCoordinator -> (Repository -> GitOid -> IO (Either Text FilePath)) -> IO () -> Api.Generation -> IO Api.Generation -> IO () -> (GitOid -> IO Api.Generation) -> Api.Repo -> Api.ApiRequest -> IO (Either Api.ApiError (Api.ApiResult, Api.ResponseAsOf, Api.Generation))
dispatchProduction compilation compileExact afterCompilationJoin fallbackGeneration allocateGeneration afterResolution publishGeneration bound request = case request of
  Api.ApiRepositoryRequest -> do
    basis <- captureObservation repository afterResolution allocateGeneration (observeRepositoryBasis bound)
    case basis of
      Left problem -> pure (Left problem)
      Right (observed, generation) -> do
        pure (Right (Api.ApiRepositoryResult (Api.repositoryResult bound (Api.basisHead observed) (Api.basisHeadRef observed)), Api.AsOfCommit (Api.basisHead observed), generation))
  Api.ApiShowRequest value -> do
    exact <- captureRevision repository afterResolution allocateGeneration (Query.showRequestRevision value)
    case exact of
      Left problem -> pure (Left problem)
      Right (oid, generation) -> do
        result <- Query.runWebShow repository value {Query.showRequestRevision = gitOidText oid}
        pure $ case result of
          Left failure -> Left (showApiError failure)
          Right projection -> Right (Api.ApiShowResult projection, Api.AsOfCommit oid, generation)
  Api.ApiHistoryRequest value -> do
    exact <- captureRevision repository afterResolution allocateGeneration (Query.historyRequestRevision value)
    case exact of
      Left problem -> pure (Left problem)
      Right (oid, generation) -> do
        result <- Query.runHistory repository value {Query.historyRequestRevision = gitOidText oid}
        pure (either (Left . historyApiError) (\projection -> Right (Api.ApiHistoryResult projection, Api.AsOfCommit oid, generation)) result)
  Api.ApiCompareRequest value -> do
    exact <- captureObservation repository afterResolution allocateGeneration $ do
      before <- exactRevision repository (Query.compareRequestFrom value)
      after <- exactRevision repository (Query.compareRequestTo value)
      pure ((,) <$> before <*> after)
    case exact of
      Right ((fromOid, toOid), generation) -> do
        result <- Query.runCompare repository value {Query.compareRequestFrom = gitOidText fromOid, Query.compareRequestTo = gitOidText toOid}
        pure (either (Left . compareApiError) (\projection -> Right (Api.ApiCompareResult projection, Api.AsOfComparison fromOid toOid, generation)) result)
      Left problem -> pure (Left problem)
  Api.ApiSearchRequest value -> do
    exact <- captureRevision repository afterResolution allocateGeneration (Query.searchServiceRevision value)
    case exact of
      Left problem -> pure (Left problem)
      Right (oid, generation) -> do
        prepared <- ensureCompiled compilation compileExact afterCompilationJoin repository oid
        result <- case prepared of Left problem -> pure (Left (Query.SearchCompilerFailure problem)); Right _ -> Query.runWebSearchExact repository oid value
        pure $ case result of
          Left failure -> Left (searchApiError failure)
          Right projection -> Right (Api.ApiSearchResult projection, Api.AsOfCommit oid, generation)
  Api.ApiRelevantRequest value -> do
    let requested = case relevantRequestRevision value of AtRevision revision -> revision; WorkingRevision -> "HEAD"
    exact <- captureRevision repository afterResolution allocateGeneration requested
    case exact of
      Left problem -> pure (Left problem)
      Right (oid, generation) -> do
        prepared <- ensureCompiled compilation compileExact afterCompilationJoin repository oid
        result <- case prepared of
          Left problem -> pure (Left (Query.RelevantCompilerFailure problem))
          Right _ -> Query.runRelevantQueryExact repository oid value
        pure (either (Left . relevantApiError) (\projection -> Right (Api.ApiRelevantResult projection, Api.AsOfCommit oid, generation)) result)
  Api.ApiConflictsRequest value -> do
    exact <- captureRevision repository afterResolution allocateGeneration (Api.revisionRequestRevision value)
    case exact of
      Left problem -> pure (Left problem)
      Right (oid, generation) -> do
        snapshot <- Query.readSnapshotAt repository (gitOidText oid)
        pure $ case snapshot of
          Left _ -> Left (Api.serviceFailure "unable to read repository conflicts")
          Right observed -> Right (Api.ApiConflictsResult (Api.ConflictsResult (mapMaybe Graph.classifyAdrConflict (Graph.graphReductionAdrs (readSnapshotReduction observed)))), Api.AsOfCommit oid, generation)
  Api.ApiDoctorRequest value -> do
    exact <- captureRevision repository afterResolution allocateGeneration (Api.revisionRequestRevision value)
    case exact of
      Left problem -> pure (Left problem)
      Right (oid, generation) -> do
        prepared <- ensureCompiled compilation compileExact afterCompilationJoin repository oid
        result <- case prepared of
          Left problem -> pure (Left problem)
          Right artifact -> Runtime.runDoctorFromExactArchive repository oid (Compilation.compiledArtifactDatabase artifact)
        pure (either (Left . Api.serviceFailure) (\output -> Right (Api.ApiDoctorResult output, Api.AsOfCommit oid, generation)) result)
  Api.ApiEventsRequest -> pure (Left (Api.ApiError Api.ServiceFailure 503 "events-unavailable" "live repository events are not available in this increment"))
  Api.ApiCreateRequest basis value -> runMutation bound basis $ \expected paths -> do
    publication <- newEmptyMVar
    result <- Mutation.createAdrCommandAutoCheckedPublishing (recordPublication publication publishGeneration) expected repository paths (CliTypes.requestActor value) (CliTypes.requestTitle value) (CliTypes.requestSummary value) (CliTypes.requestBody value) (CliTypes.requestDomains value) (CliTypes.requestScopes value) (CliTypes.requestInputDigest value) (CliTypes.requestPromptDigest value) (CliTypes.requestContextDigest value)
    completeMutation compilation compileExact afterCompilationJoin fallbackGeneration publication repository result Mutation.createCommitOid Mutation.createPublicationError (\mutation indexed warning -> Api.ApiCreateResult mutation (CliTypes.requestDomains value) indexed warning)
  Api.ApiAmendRequest existing -> runExisting bound existing $ \expected _paths value -> do
    let inputs = Types.ProvenanceInputs (CliTypes.amendRequestInputDigest value) (CliTypes.amendRequestPromptDigest value) (CliTypes.amendRequestContextDigest value)
    publication <- newEmptyMVar
    result <- Mutation.amendCurrentAdrCommandCheckedPublishing (recordPublication publication publishGeneration) expected repository (CliTypes.amendRequestActor value) (CliTypes.amendRequestAdr value) (CliTypes.amendRequestExpectedState value) (CliTypes.amendRequestChangeSummary value) (CliTypes.amendRequestTitle value) (CliTypes.amendRequestSummary value) (CliTypes.amendRequestBody value) inputs
    completeMutation compilation compileExact afterCompilationJoin fallbackGeneration publication repository result Mutation.amendCommitOid Mutation.amendPublicationError Api.ApiAmendResult
  Api.ApiScopeRequest existing -> runExisting bound existing $ \expected paths value -> do
    publication <- newEmptyMVar
    result <- Mutation.changeScopeCommandCheckedPublishing (recordPublication publication publishGeneration) expected repository paths (CliTypes.scopeRequestActor value) (CliTypes.scopeRequestAdr value) (CliTypes.scopeRequestExpectedState value) (CliTypes.scopeRequestReason value) (CliTypes.scopeRequestChange value) (Types.ProvenanceInputs (CliTypes.scopeRequestInputDigest value) (CliTypes.scopeRequestPromptDigest value) (CliTypes.scopeRequestContextDigest value))
    completeMutation compilation compileExact afterCompilationJoin fallbackGeneration publication repository result Mutation.scopeChangeCommitOid Mutation.scopeChangePublicationError Api.ApiScopeResult
  Api.ApiDomainRequest existing -> runExisting bound existing $ \expected paths value -> do
    publication <- newEmptyMVar
    result <- Mutation.changeDomainCommandCheckedPublishing (recordPublication publication publishGeneration) expected repository paths (CliTypes.domainRequestActor value) (CliTypes.domainRequestAdr value) (CliTypes.domainRequestExpectedState value) (CliTypes.domainRequestReason value) (CliTypes.domainRequestChange value) (Types.ProvenanceInputs (CliTypes.domainRequestInputDigest value) (CliTypes.domainRequestPromptDigest value) (CliTypes.domainRequestContextDigest value))
    completeMutation compilation compileExact afterCompilationJoin fallbackGeneration publication repository result Mutation.domainChangeCommitOid Mutation.domainChangePublicationError Api.ApiDomainResult
  Api.ApiObsoleteRequest existing -> runExisting bound existing $ \expected paths value -> do
    publication <- newEmptyMVar
    result <- Mutation.obsoleteCommandCheckedPublishing (recordPublication publication publishGeneration) expected repository paths (CliTypes.obsoleteRequestActor value) (Api.existingAdr existing) (CliTypes.obsoleteIntent value) (CliTypes.obsoleteRequestInputs value)
    completeMutation compilation compileExact afterCompilationJoin fallbackGeneration publication repository result Mutation.obsoleteCommitOid Mutation.obsoletePublicationError Api.ApiObsoleteResult
  Api.ApiReactivateRequest existing -> runExisting bound existing $ \expected paths value -> do
    publication <- newEmptyMVar
    result <- Mutation.reactivateCommandCheckedPublishing (recordPublication publication publishGeneration) expected repository paths (CliTypes.reactivateRequestActor value) (Api.existingAdr existing) (CliTypes.reactivateIntent value) (CliTypes.reactivateRequestInputs value)
    completeMutation compilation compileExact afterCompilationJoin fallbackGeneration publication repository result Mutation.reactivateCommitOid Mutation.reactivatePublicationError Api.ApiReactivateResult
  where
    repository = Api.repoRepository bound

runMutation :: Api.Repo -> Api.RepositoryBasis -> (ExpectedRepositoryBasis -> Types.ManagedPaths -> IO (Either Api.ApiError (Api.ApiResult, Api.ResponseAsOf, Api.Generation))) -> IO (Either Api.ApiError (Api.ApiResult, Api.ResponseAsOf, Api.Generation))
runMutation bound basis action =
  if Api.basisToken basis /= Api.mkRepositoryStateToken bound (Api.basisHead basis) (Api.basisHeadRef basis)
    then pure (Left (Api.staleStateFailure "repository state token does not match its submitted basis"))
    else do
      snapshot <- repositorySnapshot (Api.repoRepository bound) (RevisionSpec (gitOidText (Api.basisHead basis)))
      case snapshot of
        Left problem -> pure (Left (Api.serviceFailure (Text.pack (show problem))))
        Right observed -> action (toExpected basis) (repositorySnapshotManagedPaths observed)

runExisting :: Api.Repo -> Api.ExistingMutation request -> (ExpectedRepositoryBasis -> Types.ManagedPaths -> request -> IO (Either Api.ApiError (Api.ApiResult, Api.ResponseAsOf, Api.Generation))) -> IO (Either Api.ApiError (Api.ApiResult, Api.ResponseAsOf, Api.Generation))
runExisting bound existing action =
  runMutation bound (Api.existingRepositoryBasis existing) (\expected paths -> action expected paths (Api.existingSharedRequest existing))

toExpected :: Api.RepositoryBasis -> ExpectedRepositoryBasis
toExpected basis = ExpectedRepositoryBasis (Api.basisHead basis) $ case Api.basisHeadRef basis of
  Api.AttachedHead ref -> GitHeadAttached ref
  Api.DetachedHead -> GitHeadDetached

recordPublication :: MVar (Either SomeException Api.Generation) -> (GitOid -> IO Api.Generation) -> GitOid -> IO ()
recordPublication publication publish oid = do
  outcome <- trySynchronous (publish oid)
  putMVar publication outcome
  either throwIO (const (pure ())) outcome

completeMutation :: Compilation.CompilationCoordinator -> (Repository -> GitOid -> IO (Either Text FilePath)) -> IO () -> Api.Generation -> MVar (Either SomeException Api.Generation) -> Repository -> Either TransactionError mutation -> (mutation -> GitOid) -> (mutation -> Maybe Text) -> (mutation -> PostCommit.PostCommitIndexResult -> Maybe Text -> Api.ApiResult) -> IO (Either Api.ApiError (Api.ApiResult, Api.ResponseAsOf, Api.Generation))
completeMutation compilation compileExact afterCompilationJoin fallbackGeneration publication repository result commitOf publicationErrorOf render = case result of
  Left problem -> pure (Left (transactionApiError problem))
  Right mutation -> do
    published <- tryTakeMVar publication
    let servicePublicationError = publicationErrorOf mutation
        publicationWarning = case (published, servicePublicationError) of
          (Just (Right _), Nothing) -> Nothing
          (Nothing, Nothing) -> Just "commit generation publication did not report a result"
          _ -> Just "commit generation publication failed after the durable commit"
        generation = case (published, publicationWarning) of
          (Just (Right observed), Nothing) -> observed
          _ -> fallbackGeneration
        responseAsOf = case publicationWarning of
          Nothing -> Api.AsOfCommit (commitOf mutation)
          Just _ -> Api.AsOfUnavailable "commit-publication-failed"
    compilationOutcome <- trySynchronous (ensureCompiled compilation compileExact afterCompilationJoin repository (commitOf mutation))
    compiled <- case compilationOutcome of
      Left exception -> case fromException exception of
        Just ExactArchiveBusy -> pure (Left "exact archive temporarily busy after durable commit")
        Nothing -> throwIO exception
      Right compiledResult -> pure compiledResult
    let indexed = case compiled of
          Left problem -> PostCommit.PostCommitIndexResult False Nothing Nothing [] (Just (PostCommit.PostCommitIndexCompileException problem))
          Right artifact -> PostCommit.PostCommitIndexResult True (Just (Compilation.compiledArtifactDatabase artifact)) (Just (commitOf mutation)) [] Nothing
    pure (Right (render mutation indexed publicationWarning, responseAsOf, generation))

transactionApiError :: TransactionError -> Api.ApiError
transactionApiError problem = case problem of
  Stage2AcquireLock message -> Api.ApiError Api.ServiceFailure 503 "repository-lock-unavailable" message
  Stage3ValidateState message
    | any (`Text.isInfixOf` message) staleMarkers -> Api.staleStateFailure message
    | any (`Text.isInfixOf` message) conflictMarkers -> semanticConflict message
    | otherwise -> Api.ApiError Api.MalformedInput 400 "invalid-mutation" message
  Stage4GenerateFiles message -> Api.ApiError Api.MalformedInput 400 "invalid-mutation" message
  Stage5ValidateGenerated message -> Api.ApiError Api.MalformedInput 400 "invalid-mutation" message
  _ -> Api.serviceFailure (Text.pack (show problem))
  where
    staleMarkers = ["expected repository", "expected head mismatch", "expected repository commit mismatch", "stale ADR state:"]
    conflictMarkers = ["conflicted", "conflict requires", "not active", "not obsolete", "already obsolete", "already active", "no unambiguous", "requires a conflicted", "replacement ADR"]

exactRevision :: Repository -> Text -> IO (Either Api.ApiError GitOid)
exactRevision repository requested = do
  resolved <- resolveRevision repository (RevisionSpec requested)
  pure (either (Left . revisionApiError) Right resolved)

captureRevision :: Repository -> IO () -> IO Api.Generation -> Text -> IO (Either Api.ApiError (GitOid, Api.Generation))
captureRevision repository afterResolution allocate requested =
  captureObservation repository afterResolution allocate (exactRevision repository requested)

captureObservation :: Repository -> IO () -> IO Api.Generation -> IO (Either Api.ApiError observed) -> IO (Either Api.ApiError (observed, Api.Generation))
captureObservation repository afterResolution allocate observe = do
  captured <- trySynchronous $ withGitLock repository $ do
    observation <- observe
    case observation of
      Left problem -> pure (Left problem)
      Right value -> do
        afterResolution
        generation <- allocate
        pure (Right (value, generation))
  case captured of
    Right result -> pure result
    Left failure -> case fromException failure of
      Just Events.GenerationExhausted -> throwIO Events.GenerationExhausted
      Nothing -> pure $ case fromException failure :: Maybe GitLockError of
        Just _ -> Left (Api.ApiError Api.ServiceFailure 503 "repository-busy" "repository observation is temporarily unavailable")
        Nothing -> Left (Api.serviceFailure (Text.pack (displayException failure)))

revisionApiError :: GitError -> Api.ApiError
revisionApiError problem = case problem of
  GitUnknownRevision _ -> missing
  GitRevisionNotCommit _ -> missing
  GitInvalidRevisionSpec _ -> Api.ApiError Api.MalformedInput 400 "invalid-revision" (Text.pack (show problem))
  _ -> Api.serviceFailure (Text.pack (show problem))
  where
    missing = Api.ApiError Api.MissingResource 404 "revision-not-found" (Text.pack (show problem))

showApiError :: Query.ShowFailure -> Api.ApiError
showApiError failure = case failure of
  Query.ShowReferenceFailure _ -> missingResource (Query.showFailureText failure)
  Query.ShowProjectionFailure _ -> malformedQuery (Query.showFailureText failure)
  Query.ShowRawRequiresExploded -> malformedQuery (Query.showFailureText failure)
  Query.ShowSemanticConflict _ -> semanticConflict (Query.showFailureText failure)
  _ -> Api.serviceFailure (Query.showFailureText failure)

historyApiError :: Query.HistoryFailure -> Api.ApiError
historyApiError failure = case failure of
  Query.HistoryReferenceFailure _ -> missingResource (Query.historyFailureText failure)
  Query.HistoryProjectionFailure _ -> malformedQuery (Query.historyFailureText failure)
  _ -> Api.serviceFailure (Query.historyFailureText failure)

compareApiError :: Query.CompareFailure -> Api.ApiError
compareApiError failure = case failure of
  Query.CompareProjectionFailure _ -> malformedQuery (Query.compareFailureText failure)
  _ -> Api.serviceFailure (Query.compareFailureText failure)

searchApiError :: Query.SearchFailure -> Api.ApiError
searchApiError failure = case failure of
  Query.SearchQueryFailure _ -> malformedQuery (Query.searchFailureText failure)
  Query.SearchSemanticConflict _ -> semanticConflict (Query.searchFailureText failure)
  _ -> Api.serviceFailure (Query.searchFailureText failure)

relevantApiError :: Query.RelevantFailure -> Api.ApiError
relevantApiError failure = case failure of
  Query.RelevantSourceFailure _ -> Api.ApiError Api.MissingResource 404 "source-not-found" (Query.relevantFailureText failure)
  Query.RelevantQueryFailure _ -> malformedQuery (Query.relevantFailureText failure)
  _ -> Api.serviceFailure (Query.relevantFailureText failure)

missingResource, malformedQuery, semanticConflict :: Text -> Api.ApiError
missingResource = Api.ApiError Api.MissingResource 404 "missing-resource"
malformedQuery = Api.ApiError Api.MalformedInput 400 "malformed-query"
semanticConflict = Api.ApiError Api.SemanticConflict 409 "semantic-conflict"

observeRepositoryBasis :: Api.Repo -> IO (Either Api.ApiError Api.RepositoryBasis)
observeRepositoryBasis bound = go (3 :: Int)
  where
    repository = Api.repoRepository bound
    go remaining = do
      before <- repositoryHeadState repository
      case before of
        Left problem -> pure (Left (Api.serviceFailure (Text.pack (show problem))))
        Right state -> do
          resolved <- resolveRevision repository (RevisionSpec (basisRevision state))
          after <- repositoryHeadState repository
          case (resolved, after) of
            (Right oid, Right same) | state == same -> pure (Right (Api.RepositoryBasis (Api.mkRepositoryStateToken bound oid (apiBasis state)) oid (apiBasis state)))
            _ | remaining > 1 -> go (remaining - 1)
            (Left problem, _) -> pure (Left (Api.ApiError Api.ServiceFailure 503 "repository-unavailable" (Text.pack (show problem))))
            _ -> pure (Left (Api.ApiError Api.StaleState 409 "unstable-repository-basis" "repository HEAD changed while it was being observed"))
    basisRevision GitHeadDetached = "HEAD"
    basisRevision (GitHeadAttached ref) = gitRefText ref
    apiBasis GitHeadDetached = Api.DetachedHead
    apiBasis (GitHeadAttached ref) = Api.AttachedHead ref

unavailableMetadata :: ApplicationRuntime -> Text -> IO Api.ResponseMetadata
unavailableMetadata runtime reason = Api.ResponseMetadata <$> nextGeneration runtime <*> pure (Api.AsOfUnavailable reason)

nextGeneration :: ApplicationRuntime -> IO Api.Generation
nextGeneration runtime = Api.mkGeneration <$> Events.nextEventGeneration (applicationCoordinator runtime)

ensureCompiled :: Compilation.CompilationCoordinator -> (Repository -> GitOid -> IO (Either Text FilePath)) -> IO () -> Repository -> GitOid -> IO (Either Text Compilation.CompiledArtifact)
ensureCompiled coordinator compileExact afterJoin repository oid = do
  acquired <- trySynchronous $ Compilation.acquireExactCompilationObserved coordinator repository oid afterJoin $ do
    archive <- compileExact repository oid
    pure (Compilation.CompiledArtifact oid <$> archive)
  case acquired of
    Left failure -> case fromException failure of
      Just ExactArchiveBusy -> throwIO ExactArchiveBusy
      Nothing -> pure (Left (Text.pack (displayException failure)))
    Right result -> pure result

subscribeApplicationEvents :: ApplicationRuntime -> (Api.Repo -> IO Watch.RepositorySnapshot) -> IO (Either Text Events.EventSubscriber)
subscribeApplicationEvents runtime takeSnapshot = attempt (2 :: Int)
  where
    coordinator = applicationCoordinator runtime
    bound = applicationRepo runtime
    registry = applicationActiveFiles runtime
    attempt remaining = do
      terminal <- Events.isGenerationExhausted coordinator
      if terminal then pure (Left "generation-exhausted") else do
        before <- Events.readEventGeneration coordinator
        -- The observer's bounded filesystem scan must never hold the Git lock.
        sampled <- timeout 2000000 (takeSnapshot bound)
        case sampled of
          Nothing -> retry remaining
          Just snapshot -> do
            provisional <- newIORef Nothing
            let discardProvisional = do
                  owned <- readIORef provisional
                  maybe (pure ()) (Events.unregisterSubscriber coordinator) owned
            captured <- trySynchronous $ (withGitLock (Api.repoRepository bound) $ do
              current <- Events.readEventGeneration coordinator
              epochMatches <- atomically (Watch.observationEpochMatches registry (snapshotEpoch snapshot))
              if current == maxBound then pure (Left "generation-exhausted")
              else if current /= before || not epochMatches then pure (Right Nothing)
              else do
                asOf <- initialAsOf snapshot
                case asOf of
                  Nothing -> pure (Right Nothing)
                  Just initial -> mask $ \_ -> do
                    registered <- Events.registerSubscriberWithInitial coordinator initial
                    case registered of
                      Left problem -> pure (Left problem)
                      Right subscriber -> do
                        writeIORef provisional (Just subscriber)
                        after <- Events.readEventGeneration coordinator
                        epochStillMatches <- atomically (Watch.observationEpochMatches registry (snapshotEpoch snapshot))
                        let coherent = after == current + 1 && epochStillMatches
                        if coherent
                          then pure (Right (Just subscriber))
                          else do
                            Events.unregisterSubscriber coordinator subscriber
                            writeIORef provisional Nothing
                            pure (Right Nothing)) `onException` discardProvisional
            case captured of
              Right (Right (Just subscriber)) -> pure (Right subscriber)
              Right (Right Nothing) -> retry remaining
              Right (Left problem) -> terminalOr (Left problem)
              Left _ -> retry remaining
    retry remaining
      | remaining > 1 = attempt (remaining - 1)
      | otherwise = terminalOr (Left "repository snapshot changed during subscription")
    snapshotEpoch (Watch.RepositorySnapshot epoch _) = epoch
    snapshotEpoch (Watch.RepositorySnapshotFailed epoch _) = epoch
    initialAsOf (Watch.RepositorySnapshotFailed _ _) = pure (Just (Events.EventAsOfUnavailable "repository-observation-failed"))
    initialAsOf (Watch.RepositorySnapshot _ facts) = do
      basis <- observeRepositoryBasis bound
      pure $ case basis of
        Right observed
          | Watch.factsHead facts == Just (Api.basisHead observed)
              && Watch.factsHeadState facts == Just (basisHeadState observed) ->
              Just (Events.EventAt (Api.basisHead observed))
        _ -> Nothing
    basisHeadState observed = case Api.basisHeadRef observed of
      Api.AttachedHead ref -> GitHeadAttached ref
      Api.DetachedHead -> GitHeadDetached
    terminalOr fallback = do
      terminal <- Events.isGenerationExhausted coordinator
      pure (if terminal then Left "generation-exhausted" else fallback)

publishWatcherEvent :: ApplicationRuntime -> Watch.RepositoryEvent -> IO ()
publishWatcherEvent runtime event = case event of
  Watch.RepositoryObservationFailure epoch _ -> publishObserved epoch (Events.EventAsOfUnavailable "repository-observation-failed") [minBound .. maxBound]
  Watch.RepositoryFactsChanged epoch (Watch.RepositorySnapshot _ facts) invalidations -> do
    throwIfTerminal
    let repository = Api.repoRepository (applicationRepo runtime)
    captured <- trySynchronous $ withGitLock repository $ do
      actualState <- repositoryHeadState repository
      actualHead <- exactRevision repository "HEAD"
      case (actualState, actualHead, Watch.factsHeadState facts, Watch.factsHead facts) of
        (Right state, Right oid, Just expectedState, Just expectedOid) | state == expectedState && oid == expectedOid -> do
          published <- Events.publishInvalidationWhen (Watch.observationEpochMatches (applicationActiveFiles runtime) epoch) (applicationCoordinator runtime) (Events.EventAt oid) invalidations
          maybe (ioError (userError "repository observation epoch was superseded before publication")) (const (pure ())) published
        _ -> ioError (userError "repository observation was superseded before publication")
    throwOnFailure captured
  Watch.RepositoryFactsChanged _ _ _ -> pure ()
  where
    publishObserved epoch asOf invalidations = do
      throwIfTerminal
      let repository = Api.repoRepository (applicationRepo runtime)
      captured <- trySynchronous $ withGitLock repository $ do
        published <- Events.publishInvalidationWhen (Watch.observationEpochMatches (applicationActiveFiles runtime) epoch) (applicationCoordinator runtime) asOf invalidations
        maybe (ioError (userError "repository observation epoch was superseded before publication")) (const (pure ())) published
      throwOnFailure captured
    throwIfTerminal = do
      terminal <- Events.isGenerationExhausted (applicationCoordinator runtime)
      if terminal then throwIO Events.GenerationExhausted else pure ()
    throwOnFailure (Right ()) = pure ()
    throwOnFailure (Left failure) = throwIfTerminal >> throwIO failure

rawSecurityRequest :: ApplicationRuntime -> Request -> Security.SecurityRequest
rawSecurityRequest _runtime request =
  Security.SecurityRequest
    { Security.securityMethod = securityMethod request,
      Security.securityTarget = targetFor request,
      Security.securityHosts = headerTexts Header.hHost request,
      Security.securityOrigins = headerTexts "Origin" request,
      Security.securityAuthorization = headerTexts Header.hAuthorization request,
      Security.securityCookies = concatMap parseCookieHeader (headerTexts Header.hCookie request),
      Security.securityQuery = lenientQuery request,
      Security.securityContentTypes = map (Text.toLower . Text.strip) (headerTexts Header.hContentType request)
    }

rawSecurityRequestWithoutQuery :: ApplicationRuntime -> Request -> Security.SecurityRequest
rawSecurityRequestWithoutQuery runtime request = (rawSecurityRequest runtime request) {Security.securityQuery = []}

securityMethod :: Request -> Security.SecurityMethod
securityMethod request
  | requestMethod request == Http.methodGet = Security.SecurityGet
  | requestMethod request == Http.methodPost = Security.SecurityPost
  | otherwise = Security.SecurityOther (decodeLenient (requestMethod request))

httpMethod :: Request -> Api.HttpMethod
httpMethod request
  | requestMethod request == Http.methodGet = Api.GetMethod
  | requestMethod request == Http.methodPost = Api.PostMethod
  | otherwise = Api.OtherMethod (decodeLenient (requestMethod request))

targetFor :: Request -> Security.AdmissionTarget
targetFor request
  | null (pathInfo request) = if ByteString.null (rawQueryString request) then Security.StaticTarget else Security.BootstrapTarget
  | pathInfo request == ["app.css"] || pathInfo request == ["app.js"] = Security.StaticTarget
  | pathInfo request == ["api", "v1", "events"] = Security.WebSocketUpgradeTarget
  | requestMethod request == Http.methodPost = Security.MutationTarget
  | "api" `takePrefix` pathInfo request = Security.ApiReadTarget
  | otherwise = Security.StaticTarget
  where
    takePrefix value values = case values of first : _ -> first == value; [] -> False

strictQuery :: Request -> Either Api.ApiError Api.QueryParameters
strictQuery request = traverse decodePair (queryString request)
  where
    decodePair (key, value) = (,) <$> decode "query name" key <*> traverse (decode "query value") value
    decode label = either (const (Left (Api.ApiError Api.MalformedInput 400 "invalid-query-encoding" (label <> " is not UTF-8")))) Right . TextEncoding.decodeUtf8'

encodedQueryBytes :: Request -> Int
encodedQueryBytes request =
  let bytes = rawQueryString request
   in ByteString.length bytes - if ByteString.isPrefixOf "?" bytes then 1 else 0

lenientQuery :: Request -> [(Text, Maybe Text)]
lenientQuery = map (\(key, value) -> (decodeLenient key, decodeLenient <$> value)) . queryString

boundedBody :: Int -> Request -> IO (Either Api.ApiError ByteString)
boundedBody limit request =
  case contentLength request of
    Left problem -> pure (Left problem)
    Right (Just lengthValue) | lengthValue > fromIntegral limit -> pure (Left tooLarge)
    _ -> readChunks 0 []
  where
    tooLarge = Api.ApiError Api.MalformedInput 413 "body-too-large" "request body exceeds the configured bound"
    readChunks total chunks = do
      chunk <- getRequestBodyChunk request
      if ByteString.null chunk
        then pure (Right (ByteString.concat (reverse chunks)))
        else do
          let next = total + ByteString.length chunk
          if next > limit then pure (Left tooLarge) else readChunks next (chunk : chunks)

contentLength :: Request -> Either Api.ApiError (Maybe Integer)
contentLength request = case headerTexts Header.hContentLength request of
  [] -> Right Nothing
  [raw] -> case reads (Text.unpack raw) of
    [(value, "")] | value >= (0 :: Integer) -> Right (Just value)
    _ -> Left (Api.ApiError Api.MalformedInput 400 "invalid-content-length" "Content-Length must be one nonnegative decimal value")
  _ -> Left (Api.ApiError Api.MalformedInput 400 "invalid-content-length" "duplicate Content-Length headers are not allowed")

headerTexts :: Header.HeaderName -> Request -> [Text]
headerTexts name = map (decodeLenient . snd) . filter ((== name) . fst) . requestHeaders

parseCookieHeader :: Text -> [(Text, Text)]
parseCookieHeader = mapMaybe parseCookie . Text.splitOn ";"
  where
    parseCookie raw = case Text.breakOn "=" (Text.strip raw) of
      (name, value) | not (Text.null name) && not (Text.null value) -> Just (name, Text.drop 1 value)
      _ -> Nothing

decodeLenient :: ByteString -> Text
decodeLenient = TextEncoding.decodeUtf8With TextEncodingError.lenientDecode

bootstrapCookie :: Security.CredentialSource -> [(Header.HeaderName, ByteString)]
bootstrapCookie (Security.BootstrapQueryCredential cookie) = [(Header.hSetCookie, TextEncoding.encodeUtf8 cookie)]
bootstrapCookie _ = []

assetResponse :: Api.ResponseMetadata -> ByteString -> [(Header.HeaderName, ByteString)] -> ByteString -> Response
assetResponse metadata contentType extra bytes =
  responseLBS Http.status200 (metadataHeaders metadata <> securityHeaders <> [(Header.hContentType, contentType)] <> extra) (LazyByteString.fromStrict bytes)

jsonResponse :: Http.Status -> Aeson.Value -> [(Text, Text)] -> Response
jsonResponse status value metadata = responseLBS status (textHeaders metadata <> securityHeaders <> [(Header.hContentType, "application/json")]) (Aeson.encode value)

errorResponse :: Api.ResponseMetadata -> Api.ApiError -> Response
errorResponse metadata problem = jsonResponse (Http.mkStatus (Api.apiErrorStatus problem) "") (Api.errorResponseJson (Api.ApiErrorResponse metadata problem)) (Api.responseHeaders metadata)

generationExhaustedResponse :: Api.ResponseMetadata -> Response
generationExhaustedResponse metadata =
  errorResponse
    (metadata {Api.responseAsOf = Api.AsOfUnavailable "generation-exhausted"})
    (Api.ApiError Api.ServiceFailure 503 "generation-exhausted" "process generation exhausted; restart the web server")

metadataHeaders :: Api.ResponseMetadata -> [(Header.HeaderName, ByteString)]
metadataHeaders = textHeaders . Api.responseHeaders

textHeaders :: [(Text, Text)] -> [(Header.HeaderName, ByteString)]
textHeaders = map (\(name, value) -> (fromString (Text.unpack name), TextEncoding.encodeUtf8 value))

securityHeaders :: [(Header.HeaderName, ByteString)]
securityHeaders = textHeaders Security.bootstrapHeaders

securityApiError :: Security.SecurityError -> Api.ApiError
securityApiError problem = case problem of
  Security.InvalidHost -> origin
  Security.InvalidOrigin -> origin
  Security.MissingOrigin -> origin
  Security.JsonContentTypeRequired -> Api.ApiError Api.MalformedInput 415 "json-content-type-required" "application/json is required"
  Security.InvalidMethodForTarget -> Api.ApiError Api.MalformedInput 405 "wrong-method" "method is not allowed for this target"
  _ -> Api.ApiError Api.AuthenticationFailure 401 "authentication-failed" "request authentication failed"
  where
    origin = Api.ApiError Api.OriginFailure 403 "origin-rejected" "request origin was rejected"

trySynchronous :: IO value -> IO (Either SomeException value)
trySynchronous action = do
  result <- try action
  case result of
    Left exception -> case fromException exception of
      Just cancellation -> throwIO (cancellation :: SomeAsyncException)
      Nothing -> pure (Left exception)
    Right value -> pure (Right value)
