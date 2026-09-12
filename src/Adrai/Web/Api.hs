{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

-- | Typed, repository-bound HTTP contract.  This module has no WAI server and
-- performs no IO: P7-02 supplies transport and service dispatch.
module Adrai.Web.Api
  ( BoundPort,
    mkBoundPort,
    boundPortValue,
    WebOptions (..),
    Repo,
    RepositoryBindingError (..),
    validateRepositoryBinding,
    repoRepository,
    repoWorktreeRoot,
    repoGitDirectory,
    repoCommonDirectory,
    RepositoryStateToken,
    RepositoryHeadBasis (..),
    repositoryStateTokenText,
    mkRepositoryStateToken,
    parseRepositoryStateToken,
    RepositoryBasis (..),
    HttpMethod (..),
    ApiRoute (..),
    parseApiRoute,
    QueryParameters,
    ApiLimits (..),
    defaultApiLimits,
    ApiRequest (..),
    RevisionRequest (..),
    ExistingMutation (..),
    decodeApiRequest,
    Generation,
    mkGeneration,
    ResponseAsOf (..),
    ResponseMetadata (..),
    RepositoryResult (..),
    repositoryResult,
    ConflictsResult (..),
    ApiResult (..),
    apiResultPayload,
    ApiResponse (..),
    responseJson,
    responseHeaders,
    ApiErrorResponse (..),
    errorResponseJson,
    errorResponseHeaders,
    ApiErrorCategory (..),
    ApiError (..),
    serviceFailure,
    staleStateFailure,
  )
where

import Adrai.CliTypes
  ( AmendRequest (..),
    CreateRequest (..),
    DoctorOutput,
    DomainRequest (..),
    ObsoleteCliRequest (..),
    ReactivateCliRequest (..),
    ScopeRequest (..),
    doctorOutputJson,
    toAesonValue,
  )
import Adrai.Domain (Domain, canonicalDomains, domainErrorText, domainRefinementText, domainText, parseDomainRefinement)
import qualified Adrai.Format as Format
import qualified Adrai.Graph as Graph
import Adrai.Git
  ( GitError,
    Repository (..),
    RepositoryLayout (..),
    mkRevisionSpec,
    revisionSpecText,
  )
import Adrai.History (HistoryOptions (..), HistoryOrder (..))
import qualified Adrai.History as History
import Adrai.Provenance (GitOid, encodeBase64Url, gitOidText, mkGitOid, sha256DigestFrames)
import Adrai.Query (RelevantRequest (..), SearchRequest (..), defaultSearchRequest)
import qualified Adrai.Query as Query
import Adrai.Retrieval (RetrievalMode (..))
import Adrai.Scope (mkScopePattern, scopePatternErrorText)
import qualified Adrai.Scope as Scope
import Adrai.Service.Mutation
  ( DomainChangeRequest (..),
    ObsoleteRequest (..),
    ReactivateRequest (..),
    ScopeChangeRequest (..),
  )
import Adrai.Service.Query
  ( CompareRequest (..),
    HistoryRequest (..),
    SearchServiceRequest (..),
    ShowRequest (..),
  )
import qualified Adrai.Service.Mutation as Mutation
import qualified Adrai.Service.PostCommitIndex as PostCommit
import qualified Adrai.Service.Query as ServiceQuery
import Adrai.Types
  ( ActorKind (..),
    AdrId,
    Actor,
    GitRef,
    ProvenanceInputs (..),
    RepoPath,
    RevisionSelector (..),
    StateToken,
    ViewMode (..),
    adrIdText,
    connectionIdText,
    digestBytes,
    mkActor,
    mkAdrId,
    mkGitRef,
    mkIdPrefix,
    idPrefixText,
    objectRefText,
    mkRepoPath,
    gitRefText,
    recordIdText,
    repoPathText,
    stateTokenText,
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bifunctor (first)
import qualified Data.ByteString as ByteString
import Data.ByteString (ByteString)
import Data.List (group, sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import Data.Word (Word64)
import Text.Read (readMaybe)

newtype BoundPort = BoundPort Int
  deriving (Eq, Ord, Show)

mkBoundPort :: Int -> Either RepositoryBindingError BoundPort
mkBoundPort value
  | value >= 1 && value <= 65535 = Right (BoundPort value)
  | otherwise = Left (InvalidWebPort value)

boundPortValue :: BoundPort -> Int
boundPortValue (BoundPort value) = value

data WebOptions = WebOptions
  { webRequestedPort :: Maybe BoundPort,
    webOpenBrowser :: Bool
  }
  deriving (Eq, Show)

data Repo = Repo
  { repoRepository :: Repository,
    repoWorktreeRoot :: FilePath,
    repoGitDirectory :: FilePath,
    repoCommonDirectory :: FilePath
  }
  deriving (Eq, Show)

data RepositoryBindingError
  = RepositoryDiscoveryFailed GitError
  | BareRepositoryRejected
  | WorktreeRootUnavailable
  | InvalidWebPort Int
  deriving (Eq, Show)

-- | Bind once to a successfully discovered worktree.  A linked worktree is
-- accepted even when its common object store reports bare; a direct bare
-- launch has no worktree root and is rejected.
validateRepositoryBinding :: Either GitError Repository -> Either RepositoryBindingError Repo
validateRepositoryBinding discovered = do
  repository <- first RepositoryDiscoveryFailed discovered
  case (repositoryLayout repository, repositoryWorktreeRoot repository) of
    (BareRepository, _) -> Left BareRepositoryRejected
    (_, Nothing) -> Left WorktreeRootUnavailable
    (MainWorktree, Just root) -> Right (bound repository root)
    (LinkedWorktree, Just root) -> Right (bound repository root)
  where
    bound repository root = Repo repository root (repositoryGitDir repository) (repositoryCommonDir repository)

newtype RepositoryStateToken = RepositoryStateToken Text
  deriving (Eq, Ord)

instance Show RepositoryStateToken where show _ = "<repository state token>"

repositoryStateTokenText :: RepositoryStateToken -> Text
repositoryStateTokenText (RepositoryStateToken value) = value

data RepositoryHeadBasis = AttachedHead GitRef | DetachedHead
  deriving (Eq, Show)

mkRepositoryStateToken :: Repo -> GitOid -> RepositoryHeadBasis -> RepositoryStateToken
mkRepositoryStateToken repository headOid headBasis =
  RepositoryStateToken
    ( "R"
        <> encodeBase64Url
          ( digestBytes
              ( sha256DigestFrames
                  [ TextEncoding.encodeUtf8 (Text.pack (repoWorktreeRoot repository)),
                    TextEncoding.encodeUtf8 (Text.pack (repoGitDirectory repository)),
                    TextEncoding.encodeUtf8 (Text.pack (repoCommonDirectory repository)),
                    TextEncoding.encodeUtf8 (gitOidText headOid),
                    TextEncoding.encodeUtf8 (headBasisText headBasis)
                  ]
              )
          )
    )

parseRepositoryStateToken :: Text -> Either ApiError RepositoryStateToken
parseRepositoryStateToken value
  | Text.length value == 44
      && Text.take 1 value == "R"
      && Text.all isBase64Url (Text.drop 1 value) = Right (RepositoryStateToken value)
  | otherwise = Left (badField "repository_state.token" "invalid repository state token")
  where
    isBase64Url character =
      character >= 'a' && character <= 'z'
        || character >= 'A' && character <= 'Z'
        || character >= '0' && character <= '9'
        || character == '-'
        || character == '_'

data RepositoryBasis = RepositoryBasis
  { basisToken :: RepositoryStateToken,
    basisHead :: GitOid,
    basisHeadRef :: RepositoryHeadBasis
  }
  deriving (Eq, Show)

data HttpMethod = GetMethod | PostMethod | OtherMethod Text
  deriving (Eq, Show)

data ApiRoute
  = RepositoryRoute
  | SearchRoute
  | RelevantRoute
  | AdrRoute AdrId
  | HistoryRoute
  | CompareRoute
  | ConflictsRoute
  | DoctorRoute
  | EventsRoute
  | CreateRoute
  | AmendRoute AdrId
  | ScopeRoute AdrId
  | DomainRoute AdrId
  | ObsoleteRoute AdrId
  | ReactivateRoute AdrId
  deriving (Eq, Show)

parseApiRoute :: HttpMethod -> [Text] -> Either ApiError ApiRoute
parseApiRoute method path = do
  (requiredMethod, route) <- routeForPath path
  if method == requiredMethod
    then Right route
    else Left (ApiError MalformedInput 405 "wrong-method" "method is not allowed for this route")
  where
    routeForPath segments = case segments of
      ["api", "v1", "repository"] -> Right (GetMethod, RepositoryRoute)
      ["api", "v1", "search"] -> Right (GetMethod, SearchRoute)
      ["api", "v1", "relevant"] -> Right (GetMethod, RelevantRoute)
      ["api", "v1", "adrs", raw] -> (GetMethod,) . AdrRoute <$> parseAdr raw
      ["api", "v1", "history"] -> Right (GetMethod, HistoryRoute)
      ["api", "v1", "compare"] -> Right (GetMethod, CompareRoute)
      ["api", "v1", "conflicts"] -> Right (GetMethod, ConflictsRoute)
      ["api", "v1", "doctor"] -> Right (GetMethod, DoctorRoute)
      ["api", "v1", "events"] -> Right (GetMethod, EventsRoute)
      ["api", "v1", "adrs"] -> Right (PostMethod, CreateRoute)
      ["api", "v1", "adrs", raw, "amend"] -> (PostMethod,) . AmendRoute <$> parseAdr raw
      ["api", "v1", "adrs", raw, "scope"] -> (PostMethod,) . ScopeRoute <$> parseAdr raw
      ["api", "v1", "adrs", raw, "domain"] -> (PostMethod,) . DomainRoute <$> parseAdr raw
      ["api", "v1", "adrs", raw, "obsolete"] -> (PostMethod,) . ObsoleteRoute <$> parseAdr raw
      ["api", "v1", "adrs", raw, "reactivate"] -> (PostMethod,) . ReactivateRoute <$> parseAdr raw
      _ -> Left (ApiError MissingRoute 404 "missing-route" "route does not exist")
    parseAdr = first (const (badField "id" "invalid ADR identifier")) . mkAdrId

type QueryParameters = [(Text, Maybe Text)]

data ApiLimits = ApiLimits
  { maximumRequestBytes :: Int,
    maximumQueryBytes :: Int,
    maximumResultLimit :: Int
  }
  deriving (Eq, Show)

defaultApiLimits :: ApiLimits
defaultApiLimits = ApiLimits (1024 * 1024) 4096 100

data RevisionRequest = RevisionRequest
  { revisionRequestRevision :: Text
  }
  deriving (Eq, Show)

data ExistingMutation request = ExistingMutation
  { existingAdr :: AdrId,
    existingRepositoryBasis :: RepositoryBasis,
    existingAdrState :: StateToken,
    existingSharedRequest :: request
  }
  deriving (Eq, Show)

data ApiRequest
  = ApiRepositoryRequest
  | ApiSearchRequest SearchServiceRequest
  | ApiRelevantRequest RelevantRequest
  | ApiShowRequest ShowRequest
  | ApiHistoryRequest HistoryRequest
  | ApiCompareRequest CompareRequest
  | ApiConflictsRequest RevisionRequest
  | ApiDoctorRequest RevisionRequest
  | ApiEventsRequest
  | ApiCreateRequest RepositoryBasis CreateRequest
  | ApiAmendRequest (ExistingMutation AmendRequest)
  | ApiScopeRequest (ExistingMutation ScopeRequest)
  | ApiDomainRequest (ExistingMutation DomainRequest)
  | ApiObsoleteRequest (ExistingMutation ObsoleteCliRequest)
  | ApiReactivateRequest (ExistingMutation ReactivateCliRequest)
  deriving (Eq, Show)

decodeApiRequest :: ApiLimits -> ApiRoute -> QueryParameters -> ByteString -> Either ApiError ApiRequest
decodeApiRequest limits route query body = do
  parameters <- validatedQuery limits query
  case route of
    RepositoryRoute -> noQuery parameters >> noBody >> Right ApiRepositoryRequest
    SearchRoute -> noBody >> ApiSearchRequest <$> decodeSearch limits parameters
    RelevantRoute -> noBody >> ApiRelevantRequest <$> decodeRelevant limits parameters
    AdrRoute adr -> noBody >> ApiShowRequest <$> decodeShow adr parameters
    HistoryRoute -> noBody >> ApiHistoryRequest <$> decodeHistory limits parameters
    CompareRoute -> noBody >> ApiCompareRequest <$> decodeCompare parameters
    ConflictsRoute -> noBody >> ApiConflictsRequest <$> decodeRevision parameters
    DoctorRoute -> noBody >> ApiDoctorRequest <$> decodeRevision parameters
    EventsRoute -> noQuery parameters >> noBody >> Right ApiEventsRequest
    CreateRoute -> noQuery parameters >> decodeCreate limits body
    AmendRoute adr -> noQuery parameters >> decodeAmend limits adr body
    ScopeRoute adr -> noQuery parameters >> decodeScope limits adr body
    DomainRoute adr -> noQuery parameters >> decodeDomain limits adr body
    ObsoleteRoute adr -> noQuery parameters >> decodeObsolete limits adr body
    ReactivateRoute adr -> noQuery parameters >> decodeReactivate limits adr body
  where
    noBody
      | ByteString.null body = Right ()
      | otherwise = Left (ApiError MalformedInput 400 "unexpected-body" "GET request body is not allowed")

validatedQuery :: ApiLimits -> QueryParameters -> Either ApiError [(Text, Text)]
validatedQuery limits query = do
  let byteCount = sum [ByteString.length (TextEncoding.encodeUtf8 key) + maybe 0 (ByteString.length . TextEncoding.encodeUtf8) value | (key, value) <- query]
      duplicates = mapMaybe duplicate (group (sort (map fst query)))
  if byteCount <= maximumQueryBytes limits then Right () else Left (ApiError MalformedInput 413 "query-too-large" "query exceeds the configured bound")
  if null duplicates then Right () else Left (ApiError MalformedInput 400 "duplicate-query" ("duplicate query fields: " <> Text.intercalate "," duplicates))
  traverse requireValue query
  where
    duplicate (value : _ : _) = Just value
    duplicate _ = Nothing
    requireValue (key, Just value) = Right (key, value)
    requireValue (key, Nothing) = Left (badField key "query field requires a value")

noQuery :: [(Text, Text)] -> Either ApiError ()
noQuery [] = Right ()
noQuery values = Left (ApiError MalformedInput 400 "unknown-query" ("query fields are not allowed: " <> Text.intercalate "," (map fst values)))

queryFields :: [Text] -> [(Text, Text)] -> Either ApiError ()
queryFields allowed values =
  case [key | (key, _) <- values, key `notElem` allowed] of
    [] -> Right ()
    unknown -> Left (ApiError MalformedInput 400 "unknown-query" ("unknown query fields: " <> Text.intercalate "," unknown))

queryValue :: Text -> [(Text, Text)] -> Maybe Text
queryValue = lookup

requiredQuery :: Text -> [(Text, Text)] -> Either ApiError Text
requiredQuery key values = maybe (Left (badField key "missing query field")) Right (queryValue key values)

decodeSearch :: ApiLimits -> [(Text, Text)] -> Either ApiError SearchServiceRequest
decodeSearch limits values = do
  queryFields ["q", "at", "mode", "view", "include_obsolete", "domain", "file", "limit", "shallow"] values
  needle <- requiredQuery "q" values
  if Text.null (Text.strip needle) then Left (badField "q" "search query must be nonblank") else Right ()
  mode <- maybe (Right HybridRetrieval) parseMode (queryValue "mode" values)
  view <- maybe (Right CollapsedView) parseView (queryValue "view" values)
  includeObsolete <- optionalBool "include_obsolete" False values
  shallow <- optionalBool "shallow" False values
  limit <- boundedLimit limits 10 values
  path <- traverse (first (const (badField "file" "invalid repository path")) . mkRepoPath) (queryValue "file" values)
  let base = defaultSearchRequest needle
      request = base
        { searchRequestMode = mode,
          searchRequestView = view,
          searchRequestIncludeObsolete = includeObsolete,
          searchRequestDomains = maybe [] (Text.splitOn ",") (queryValue "domain" values),
          searchRequestFile = path,
          searchRequestLimit = limit,
          searchRequestShallowHistory = shallow
        }
  revision <- validatedRevision "at" (fromMaybe "HEAD" (queryValue "at" values))
  Right (SearchServiceRequest revision request)

decodeRelevant :: ApiLimits -> [(Text, Text)] -> Either ApiError RelevantRequest
decodeRelevant limits values = do
  queryFields ["file", "at", "include_obsolete", "limit"] values
  rawPath <- requiredQuery "file" values
  path <- first (const (badField "file" "invalid repository path")) (mkRepoPath rawPath)
  includeObsolete <- optionalBool "include_obsolete" False values
  limit <- boundedLimit limits 10 values
  revision <- validatedRevision "at" (fromMaybe "HEAD" (queryValue "at" values))
  Right (RelevantRequest path (AtRevision revision) includeObsolete limit)

decodeShow :: AdrId -> [(Text, Text)] -> Either ApiError ShowRequest
decodeShow adr values = do
  queryFields ["at", "view", "raw"] values
  view <- maybe (Right CollapsedView) parseView (queryValue "view" values)
  raw <- optionalBool "raw" False values
  if raw && view /= ExplodedView then Left (badField "raw" "raw requires exploded view") else Right ()
  revision <- validatedRevision "at" (fromMaybe "HEAD" (queryValue "at" values))
  Right (ShowRequest (adrIdText adr) view revision raw)

decodeHistory :: ApiLimits -> [(Text, Text)] -> Either ApiError HistoryRequest
decodeHistory limits values = do
  queryFields ["adr", "at", "limit", "order"] values
  order <- case queryValue "order" values of
    Nothing -> Right NewestFirst
    Just "newest" -> Right NewestFirst
    Just "oldest" -> Right OldestFirst
    _ -> Left (badField "order" "expected newest or oldest")
  limit <- boundedLimit limits 20 values
  reference <- traverse validatedReference (queryValue "adr" values)
  revision <- validatedRevision "at" (fromMaybe "HEAD" (queryValue "at" values))
  Right (HistoryRequest reference revision (HistoryOptions order limit Nothing Nothing Nothing))

decodeCompare :: [(Text, Text)] -> Either ApiError CompareRequest
decodeCompare values = do
  queryFields ["from", "to", "include_unchanged"] values
  before <- validatedRevision "from" =<< requiredQuery "from" values
  after <- validatedRevision "to" =<< requiredQuery "to" values
  includeUnchanged <- optionalBool "include_unchanged" False values
  Right (CompareRequest before after includeUnchanged)

decodeRevision :: [(Text, Text)] -> Either ApiError RevisionRequest
decodeRevision values = do
  queryFields ["at"] values
  RevisionRequest <$> validatedRevision "at" (fromMaybe "HEAD" (queryValue "at" values))

validatedRevision :: Text -> Text -> Either ApiError Text
validatedRevision field raw =
  revisionSpecText <$> first (const (badField field "invalid revision selector")) (mkRevisionSpec raw)

validatedReference :: Text -> Either ApiError Text
validatedReference raw =
  idPrefixText <$> first (const (badField "adr" "invalid ADR/object reference")) (mkIdPrefix raw)

parseMode :: Text -> Either ApiError RetrievalMode
parseMode "hybrid" = Right HybridRetrieval
parseMode "fts" = Right FtsRetrieval
parseMode "vector" = Right VectorRetrieval
parseMode _ = Left (badField "mode" "expected hybrid, fts, or vector")

parseView :: Text -> Either ApiError ViewMode
parseView "collapsed" = Right CollapsedView
parseView "exploded" = Right ExplodedView
parseView _ = Left (badField "view" "expected collapsed or exploded")

optionalBool :: Text -> Bool -> [(Text, Text)] -> Either ApiError Bool
optionalBool key fallback values = case queryValue key values of
  Nothing -> Right fallback
  Just "true" -> Right True
  Just "false" -> Right False
  _ -> Left (badField key "expected true or false")

boundedLimit :: ApiLimits -> Int -> [(Text, Text)] -> Either ApiError Int
boundedLimit limits fallback values = case queryValue "limit" values of
  Nothing -> Right fallback
  Just raw -> case readMaybe (Text.unpack raw) of
    Just value | value >= 1 && value <= maximumResultLimit limits -> Right value
    _ -> Left (badField "limit" "limit is outside the configured bound")

decodeObject :: ApiLimits -> ByteString -> Either ApiError Aeson.Object
decodeObject limits body
  | ByteString.length body > maximumRequestBytes limits = Left (ApiError MalformedInput 413 "request-too-large" "request body exceeds the configured bound")
  | otherwise = do
      value <- first (const (ApiError MalformedInput 400 "malformed-json" "request body is not valid JSON")) (Aeson.eitherDecodeStrict' body)
      case value of
        Aeson.Object object -> Right object
        _ -> Left (ApiError MalformedInput 400 "invalid-json-shape" "request body must be an object")

ensureKeys :: [Text] -> [Text] -> Aeson.Object -> Either ApiError ()
ensureKeys required allowed object = do
  let actual = map Key.toText (KeyMap.keys object)
      missing = filter (`notElem` actual) required
      unknown = filter (`notElem` allowed) actual
  case missing of
    [] -> Right ()
    firstMissing : _ -> Left (badField firstMissing "missing JSON field")
  if null unknown then Right () else Left (ApiError MalformedInput 400 "unknown-json-field" ("unknown JSON fields: " <> Text.intercalate "," (sort unknown)))

fieldValue :: Text -> Aeson.Object -> Either ApiError Aeson.Value
fieldValue key object = maybe (Left (badField key "missing JSON field")) Right (KeyMap.lookup (Key.fromText key) object)

textField :: Text -> Aeson.Object -> Either ApiError Text
textField key object = fieldValue key object >>= \value -> case value of
  Aeson.String text -> Right text
  _ -> Left (badField key "expected string")

optionalTextField :: Text -> Aeson.Object -> Either ApiError (Maybe Text)
optionalTextField key object = case KeyMap.lookup (Key.fromText key) object of
  Nothing -> Right Nothing
  Just Aeson.Null -> Right Nothing
  Just (Aeson.String text) -> Right (Just text)
  _ -> Left (badField key "expected string or null")

boolFieldWith :: Text -> Bool -> Aeson.Object -> Either ApiError Bool
boolFieldWith key fallback object = case KeyMap.lookup (Key.fromText key) object of
  Nothing -> Right fallback
  Just (Aeson.Bool value) -> Right value
  _ -> Left (badField key "expected boolean")

textListFieldWith :: Text -> [Text] -> Aeson.Object -> Either ApiError [Text]
textListFieldWith key fallback object = case KeyMap.lookup (Key.fromText key) object of
  Nothing -> Right fallback
  Just (Aeson.Array values) -> traverse asText (foldr (:) [] values)
  _ -> Left (badField key "expected array of strings")
  where
    asText (Aeson.String value) = Right value
    asText _ = Left (badField key "expected array of strings")

requiredTextListField :: Text -> Aeson.Object -> Either ApiError [Text]
requiredTextListField key object =
  fieldValue key object >>= \value -> case value of
    Aeson.Array values -> traverse asText (foldr (:) [] values)
    _ -> Left (badField key "expected array of strings")
  where
    asText (Aeson.String item) = Right item
    asText _ = Left (badField key "expected array of strings")

ensureVariantFields :: [Text] -> [Text] -> Aeson.Object -> Either ApiError ()
ensureVariantFields required forbidden object = do
  case filter (not . present) required of
    [] -> Right ()
    missing : _ -> Left (badField missing "missing field for selected mode")
  case filter present forbidden of
    [] -> Right ()
    inactive : _ -> Left (badField inactive "field is not allowed for selected mode")
  where
    present key = KeyMap.member (Key.fromText key) object

parseBasis :: Aeson.Object -> Either ApiError RepositoryBasis
parseBasis object = do
  value <- fieldValue "repository_state" object
  state <- case value of
    Aeson.Object nested -> Right nested
    _ -> Left (badField "repository_state" "expected object")
  ensureKeys ["kind", "token", "head", "head_ref"] ["kind", "token", "head", "head_ref"] state
  kind <- textField "kind" state
  if kind == "repository" then Right () else Left (badField "repository_state.kind" "expected repository")
  token <- parseRepositoryStateToken =<< textField "token" state
  headOid <- first (const (badField "repository_state.head" "invalid Git commit OID")) . mkGitOid =<< textField "head" state
  headBasis <- case KeyMap.lookup "head_ref" state of
    Just Aeson.Null -> Right DetachedHead
    Just (Aeson.String referenceText) -> AttachedHead <$> first (const (badField "repository_state.head_ref" "invalid Git ref")) (mkGitRef referenceText)
    _ -> Left (badField "repository_state.head_ref" "expected a fully-qualified ref or null")
  Right (RepositoryBasis token headOid headBasis)

parseState :: Aeson.Object -> Either ApiError StateToken
parseState object = first (const (badField "state_token" "invalid ADR state token")) . Format.parseStateToken =<< textField "state_token" object

parseActorField :: Aeson.Object -> Either ApiError Actor
parseActorField object = do
  value <- fieldValue "actor" object
  actorObject <- case value of Aeson.Object nested -> Right nested; _ -> Left (badField "actor" "expected object")
  ensureKeys ["kind", "id"] ["kind", "id", "model"] actorObject
  kind <- textField "kind" actorObject >>= \raw -> case raw of
    "human" -> Right HumanActor
    "llm" -> Right LlmActor
    "service" -> Right ServiceActor
    _ -> Left (badField "actor.kind" "expected human, llm, or service")
  identifier <- textField "id" actorObject
  model <- optionalTextField "model" actorObject
  first (const (badField "actor" "invalid actor")) (mkActor kind identifier model)

parseInputs :: Aeson.Object -> Either ApiError ProvenanceInputs
parseInputs object = ProvenanceInputs
  <$> digest "input_digest"
  <*> digest "prompt_digest"
  <*> digest "context_digest"
  where
    digest key = optionalTextField key object >>= traverse (first (const (badField key "invalid digest")) . Format.parseDigest)

commonMutationKeys :: [Text]
commonMutationKeys = ["repository_state", "state_token", "actor", "input_digest", "prompt_digest", "context_digest"]

decodeCreate :: ApiLimits -> ByteString -> Either ApiError ApiRequest
decodeCreate limits body = do
  object <- decodeObject limits body
  let allowed = ["repository_state", "title", "summary", "body", "domains", "scopes", "actor", "input_digest", "prompt_digest", "context_digest"]
  ensureKeys ["repository_state", "title", "summary", "body", "actor"] allowed object
  basis <- parseBasis object
  title <- textField "title" object
  summary <- textField "summary" object
  content <- textField "body" object
  domains <- textListFieldWith "domains" [] object >>= first (badField "domains" . domainErrorText) . canonicalDomains
  scopes <- textListFieldWith "scopes" [] object >>= traverse (first (badField "scopes" . scopePatternErrorText) . mkScopePattern)
  actor <- parseActorField object
  inputs <- parseInputs object
  Right (ApiCreateRequest basis (CreateRequest title summary content domains scopes actor (provenanceInputDigest inputs) (provenancePromptDigest inputs) (provenanceContextDigest inputs)))

decodeAmend :: ApiLimits -> AdrId -> ByteString -> Either ApiError ApiRequest
decodeAmend limits adr body = do
  object <- decodeObject limits body
  let allowed = commonMutationKeys <> ["change_summary", "title", "summary", "body"]
  ensureKeys ["repository_state", "state_token", "change_summary", "title", "summary", "body", "actor"] allowed object
  basis <- parseBasis object
  token <- parseState object
  inputs <- parseInputs object
  actor <- parseActorField object
  shared <- AmendRequest adr (Just token) <$> textField "change_summary" object <*> textField "title" object <*> textField "summary" object <*> textField "body" object <*> pure actor <*> pure (provenanceInputDigest inputs) <*> pure (provenancePromptDigest inputs) <*> pure (provenanceContextDigest inputs)
  Right (ApiAmendRequest (ExistingMutation adr basis token shared))

decodeScope :: ApiLimits -> AdrId -> ByteString -> Either ApiError ApiRequest
decodeScope limits adr body = do
  object <- decodeObject limits body
  let allowed = commonMutationKeys <> ["reason", "mode", "add", "remove", "patterns"]
  ensureKeys ["repository_state", "state_token", "reason", "mode", "actor"] allowed object
  basis <- parseBasis object
  token <- parseState object
  mode <- textField "mode" object
  change <- case mode of
    "delta" -> do
      ensureVariantFields ["add", "remove"] ["patterns"] object
      ScopeDelta <$> scopes "add" object <*> scopes "remove" object
    "reviewed" -> do
      ensureVariantFields ["patterns"] ["add", "remove"] object
      ScopeReviewedSet <$> scopes "patterns" object
    _ -> Left (badField "mode" "expected delta or reviewed")
  actor <- parseActorField object
  inputs <- parseInputs object
  shared <- ScopeRequest adr (Just token) <$> textField "reason" object <*> pure change <*> pure actor <*> pure (provenanceInputDigest inputs) <*> pure (provenancePromptDigest inputs) <*> pure (provenanceContextDigest inputs)
  Right (ApiScopeRequest (ExistingMutation adr basis token shared))
  where
    scopes key object = requiredTextListField key object >>= traverse (first (badField key . scopePatternErrorText) . mkScopePattern)

decodeDomain :: ApiLimits -> AdrId -> ByteString -> Either ApiError ApiRequest
decodeDomain limits adr body = do
  object <- decodeObject limits body
  let allowed = commonMutationKeys <> ["reason", "mode", "add", "remove", "domains", "refinements"]
  ensureKeys ["repository_state", "state_token", "reason", "mode", "actor"] allowed object
  basis <- parseBasis object
  token <- parseState object
  mode <- textField "mode" object
  change <- case mode of
    "delta" -> do
      ensureVariantFields ["add", "remove"] ["domains", "refinements"] object
      DomainDelta <$> domains "add" object <*> domains "remove" object
    "reviewed" -> do
      ensureVariantFields ["domains"] ["add", "remove", "refinements"] object
      DomainReviewedSet <$> domains "domains" object
    "refine" -> do
      ensureVariantFields ["refinements"] ["add", "remove", "domains"] object
      refinements <- requiredTextListField "refinements" object >>= traverse (first (badField "refinements" . domainErrorText) . parseDomainRefinement)
      if null refinements then Left (badField "refinements" "refinement list must not be empty") else Right (DomainRefine refinements)
    _ -> Left (badField "mode" "expected delta, reviewed, or refine")
  actor <- parseActorField object
  inputs <- parseInputs object
  shared <- DomainRequest adr (Just token) <$> textField "reason" object <*> pure change <*> pure actor <*> pure (provenanceInputDigest inputs) <*> pure (provenancePromptDigest inputs) <*> pure (provenanceContextDigest inputs)
  Right (ApiDomainRequest (ExistingMutation adr basis token shared))
  where
    domains key object = requiredTextListField key object >>= first (badField key . domainErrorText) . canonicalDomains

decodeObsolete :: ApiLimits -> AdrId -> ByteString -> Either ApiError ApiRequest
decodeObsolete limits adr body = do
  object <- decodeObject limits body
  let allowed = commonMutationKeys <> ["reason", "resolve", "replacement"]
  ensureKeys ["repository_state", "state_token", "reason", "actor"] allowed object
  basis <- parseBasis object
  token <- parseState object
  reason <- textField "reason" object
  resolve <- boolFieldWith "resolve" False object
  replacement <- optionalTextField "replacement" object >>= traverse (first (const (badField "replacement" "invalid ADR identifier")) . mkAdrId)
  actor <- parseActorField object
  inputs <- parseInputs object
  let shared = ObsoleteCliRequest (ObsoleteRequest (Just token) reason resolve replacement) actor inputs
  Right (ApiObsoleteRequest (ExistingMutation adr basis token shared))

decodeReactivate :: ApiLimits -> AdrId -> ByteString -> Either ApiError ApiRequest
decodeReactivate limits adr body = do
  object <- decodeObject limits body
  let allowed = commonMutationKeys <> ["reason", "resolve"]
  ensureKeys ["repository_state", "state_token", "reason", "actor"] allowed object
  basis <- parseBasis object
  token <- parseState object
  reason <- textField "reason" object
  resolve <- boolFieldWith "resolve" False object
  actor <- parseActorField object
  inputs <- parseInputs object
  let shared = ReactivateCliRequest (ReactivateRequest (Just token) reason resolve) actor inputs
  Right (ApiReactivateRequest (ExistingMutation adr basis token shared))

newtype Generation = Generation Word64
  deriving (Eq, Ord, Show)

mkGeneration :: Word64 -> Generation
mkGeneration = Generation

data ResponseAsOf
  = AsOfCommit GitOid
  | AsOfComparison GitOid GitOid
  | AsOfUnavailable Text
  deriving (Eq, Show)

data ResponseMetadata = ResponseMetadata
  { responseGeneration :: Generation,
    responseAsOf :: ResponseAsOf
  }
  deriving (Eq, Show)

data RepositoryResult = RepositoryResult
  { repositoryResultWorktree :: FilePath,
    repositoryResultGitDirectory :: FilePath,
    repositoryResultCommonDirectory :: FilePath,
    repositoryResultHead :: GitOid,
    repositoryResultHeadBasis :: RepositoryHeadBasis,
    repositoryResultStateToken :: RepositoryStateToken
  }
  deriving (Eq, Show)

repositoryResult :: Repo -> GitOid -> RepositoryHeadBasis -> RepositoryResult
repositoryResult repository headOid headBasis = RepositoryResult
  { repositoryResultWorktree = repoWorktreeRoot repository,
    repositoryResultGitDirectory = repoGitDirectory repository,
    repositoryResultCommonDirectory = repoCommonDirectory repository,
    repositoryResultHead = headOid,
    repositoryResultHeadBasis = headBasis,
    repositoryResultStateToken = mkRepositoryStateToken repository headOid headBasis
  }

newtype ConflictsResult = ConflictsResult [Graph.AdrConflict]
  deriving (Eq, Show)

-- | Typed service outcomes.  Query constructors reuse the established public
-- projections; mutation constructors reuse the exact service result records
-- and preserve their committed/indexing fields.
data ApiResult
  = ApiRepositoryResult RepositoryResult
  | ApiShowResult ServiceQuery.ShowResult
  | ApiHistoryResult History.HistoryProjection
  | ApiCompareResult Query.CompareProjection
  | ApiSearchResult Query.SearchProjection
  | ApiRelevantResult Query.RelevantProjection
  | ApiConflictsResult ConflictsResult
  | ApiDoctorResult DoctorOutput
  | ApiCreateResult Mutation.CreateResult [Domain] PostCommit.PostCommitIndexResult
  | ApiAmendResult Mutation.AmendResult PostCommit.PostCommitIndexResult
  | ApiScopeResult Mutation.ScopeChangeResult PostCommit.PostCommitIndexResult
  | ApiDomainResult Mutation.DomainChangeResult PostCommit.PostCommitIndexResult
  | ApiObsoleteResult Mutation.ObsoleteResult PostCommit.PostCommitIndexResult
  | ApiReactivateResult Mutation.ReactivateResult PostCommit.PostCommitIndexResult
  deriving (Eq, Show)

apiResultPayload :: ApiResult -> Aeson.Value
apiResultPayload result = case result of
  ApiRepositoryResult value -> repositoryResultJson value
  ApiShowResult (ServiceQuery.ShowCollapsed projection) -> toAesonValue (Query.collapsedProjectionJson projection)
  ApiShowResult (ServiceQuery.ShowExploded projection) -> toAesonValue (Query.explodedProjectionJson projection)
  ApiHistoryResult projection -> toAesonValue (History.historyProjectionJson projection)
  ApiCompareResult projection -> toAesonValue (Query.compareProjectionJson projection)
  ApiSearchResult projection -> toAesonValue (Query.searchProjectionJson projection)
  ApiRelevantResult projection -> toAesonValue (Query.relevantProjectionJson projection)
  ApiConflictsResult value -> conflictsResultJson value
  ApiDoctorResult value -> doctorOutputJson value
  ApiCreateResult mutation domains indexed ->
    mutationJson (Mutation.createOperationId mutation) (Mutation.createCommitOid mutation) (Mutation.createCreatedPaths mutation) (Mutation.createIndexUpdated mutation) indexed
      [ "adr" Aeson..= adrIdText (Mutation.createAdrId mutation),
        "record" Aeson..= recordIdText (Mutation.createRecordId mutation),
        "scope" Aeson..= connectionIdText (Mutation.createScopeId mutation),
        "domain" Aeson..= connectionIdText (Mutation.createDomainId mutation),
        "domains" Aeson..= map domainText domains,
        "status" Aeson..= connectionIdText (Mutation.createStatusId mutation)
      ]
  ApiAmendResult mutation indexed ->
    mutationJson (Mutation.amendOperationId mutation) (Mutation.amendCommitOid mutation) (Mutation.amendCreatedPaths mutation) (Mutation.amendIndexUpdated mutation) indexed
      [ "adr" Aeson..= adrIdText (Mutation.amendAdrId mutation),
        "record" Aeson..= recordIdText (Mutation.amendRecordId mutation),
        "amends" Aeson..= amendParents (Mutation.amendAmends mutation),
        "connection" Aeson..= connectionIdText (Mutation.amendConnectionId mutation)
      ]
  ApiScopeResult mutation indexed ->
    mutationJson (Mutation.scopeChangeOperationId mutation) (Mutation.scopeChangeCommitOid mutation) (Mutation.scopeChangeCreatedPaths mutation) (Mutation.scopeChangeIndexUpdated mutation) indexed
      [ "adr" Aeson..= adrIdText (Mutation.scopeChangeAdrId mutation),
        "scope" Aeson..= connectionIdText (Mutation.scopeChangeConnectionId mutation),
        "scope_parents" Aeson..= map connectionIdText (Mutation.scopeChangeParents mutation),
        "mode" Aeson..= Mutation.scopeChangeMode mutation,
        "applies_to" Aeson..= map Scope.scopePatternText (Mutation.scopeChangeEffective mutation)
      ]
  ApiDomainResult mutation indexed ->
    mutationJson (Mutation.domainChangeOperationId mutation) (Mutation.domainChangeCommitOid mutation) (Mutation.domainChangeCreatedPaths mutation) (Mutation.domainChangeIndexUpdated mutation) indexed
      [ "adr" Aeson..= adrIdText (Mutation.domainChangeAdrId mutation),
        "domain" Aeson..= connectionIdText (Mutation.domainChangeConnectionId mutation),
        "domain_parents" Aeson..= map connectionIdText (Mutation.domainChangeParents mutation),
        "mode" Aeson..= Mutation.domainChangeMode mutation,
        "domains" Aeson..= map domainText (Mutation.domainChangeEffective mutation),
        "added" Aeson..= map domainText (Mutation.domainChangeAdded mutation),
        "removed" Aeson..= map domainText (Mutation.domainChangeRemoved mutation),
        "refinements" Aeson..= map domainRefinementText (Mutation.domainChangeRefinements mutation)
      ]
  ApiObsoleteResult mutation indexed ->
    mutationJson (Mutation.obsoleteOperationId mutation) (Mutation.obsoleteCommitOid mutation) (Mutation.obsoleteCreatedPaths mutation) (Mutation.obsoleteIndexUpdated mutation) indexed
      [ "adr" Aeson..= adrIdText (Mutation.obsoleteAdrId mutation),
        "connection" Aeson..= connectionIdText (Mutation.obsoleteConnectionId mutation),
        "obsolete" Aeson..= True,
        "resolved_status_conflict" Aeson..= Mutation.obsoleteResolvedConflict mutation,
        "covered_records" Aeson..= map recordIdText (Mutation.obsoleteRecordHeads mutation),
        "replacement" Aeson..= fmap adrIdText (Mutation.obsoleteReplacementAdr mutation)
      ]
  ApiReactivateResult mutation indexed ->
    mutationJson (Mutation.reactivateOperationId mutation) (Mutation.reactivateCommitOid mutation) (Mutation.reactivateCreatedPaths mutation) (Mutation.reactivateIndexUpdated mutation) indexed
      [ "adr" Aeson..= adrIdText (Mutation.reactivateAdrId mutation),
        "connection" Aeson..= connectionIdText (Mutation.reactivateConnectionId mutation),
        "obsolete" Aeson..= False,
        "resolved_status_conflict" Aeson..= Mutation.reactivateResolvedConflict mutation
      ]
  where
    amendParents [parent] = Aeson.String (recordIdText parent)
    amendParents parents = Aeson.toJSON (map recordIdText parents)

repositoryResultJson :: RepositoryResult -> Aeson.Value
repositoryResultJson result = Aeson.object
  [ "worktree" Aeson..= repositoryResultWorktree result,
    "git_directory" Aeson..= repositoryResultGitDirectory result,
    "common_git_directory" Aeson..= repositoryResultCommonDirectory result,
    "head" Aeson..= gitOidText (repositoryResultHead result),
    "head_ref" Aeson..= headReference (repositoryResultHeadBasis result),
    "repository_state" Aeson..= Aeson.object
      [ "kind" Aeson..= ("repository" :: Text),
        "token" Aeson..= repositoryStateTokenText (repositoryResultStateToken result),
        "head" Aeson..= gitOidText (repositoryResultHead result),
        "head_ref" Aeson..= headReference (repositoryResultHeadBasis result)
      ]
  ]
  where
    headReference (AttachedHead reference) = Just (gitRefText reference)
    headReference DetachedHead = Nothing

conflictsResultJson :: ConflictsResult -> Aeson.Value
conflictsResultJson (ConflictsResult conflicts) = Aeson.object
  [ "schema" Aeson..= ("adrai/conflicts/v1" :: Text),
    "conflicts" Aeson..= map conflictJson conflicts
  ]
  where
    conflictJson conflict = Aeson.object
      [ "code" Aeson..= Graph.adrConflictCode conflict,
        "adr" Aeson..= adrIdText (Graph.adrConflictAdr conflict),
        "count" Aeson..= Graph.adrConflictCount conflict,
        "summaries" Aeson..= Graph.adrConflictSummaries conflict,
        "state_token" Aeson..= stateTokenText (Graph.adrConflictStateToken conflict),
        "candidates" Aeson..= map candidateJson (Graph.adrConflictCandidates conflict)
      ]
    candidateJson candidate = Aeson.object
      [ "axis" Aeson..= axisText (Graph.conflictCandidateAxis candidate),
        "heads" Aeson..= map objectRefText (Graph.conflictCandidateHeads candidate),
        "head_count" Aeson..= Graph.conflictCandidateHeadCount candidate,
        "summary" Aeson..= Graph.conflictCandidateSummary candidate
      ]
    axisText Graph.DecisionAxis = "decision" :: Text
    axisText Graph.ScopeAxis = "scope"
    axisText Graph.DomainAxis = "domain"
    axisText Graph.StatusAxis = "status"

mutationJson :: String -> GitOid -> [RepoPath] -> Bool -> PostCommit.PostCommitIndexResult -> [Pair] -> Aeson.Value
mutationJson operation commit created indexUpdated indexed routeFields =
  Aeson.object
    ( [ "committed" Aeson..= True,
        "operation" Aeson..= operation,
        "commit" Aeson..= gitOidText commit,
        "created" Aeson..= map repoPathText created,
        "index_updated" Aeson..= indexUpdated,
        "indexed" Aeson..= PostCommit.postCommitIndexed indexed
      ]
        <> indexFields indexed
        <> routeFields
    )

indexFields :: PostCommit.PostCommitIndexResult -> [Pair]
indexFields indexed
  | PostCommit.postCommitIndexed indexed =
      [ "database" Aeson..= PostCommit.postCommitDatabase indexed,
        "index_revision" Aeson..= fmap gitOidText (PostCommit.postCommitIndexRevision indexed),
        "index_warnings" Aeson..= length (PostCommit.postCommitIndexWarnings indexed)
      ]
  | otherwise = ["index_error" Aeson..= fmap show (PostCommit.postCommitIndexError indexed)]

data ApiResponse = ApiResponse
  { apiResponseMetadata :: ResponseMetadata,
    apiResponsePayload :: ApiResult
  }
  deriving (Eq, Show)

responseJson :: ApiResponse -> Aeson.Value
responseJson response = Aeson.object
  [ "schema" Aeson..= ("adrai/api/v1" :: Text),
    "metadata" Aeson..= metadataJson (apiResponseMetadata response),
    "data" Aeson..= apiResultPayload (apiResponsePayload response)
  ]

metadataJson :: ResponseMetadata -> Aeson.Value
metadataJson metadata = Aeson.object
  [ "generation" Aeson..= generationValue (responseGeneration metadata),
    "as_of" Aeson..= asOfValue (responseAsOf metadata)
  ]
  where
    generationValue (Generation value) = value
    asOfValue (AsOfCommit oid) = Aeson.object ["kind" Aeson..= ("commit" :: Text), "oid" Aeson..= gitOidText oid]
    asOfValue (AsOfComparison before after) = Aeson.object ["kind" Aeson..= ("comparison" :: Text), "from" Aeson..= gitOidText before, "to" Aeson..= gitOidText after]
    asOfValue (AsOfUnavailable reason) = Aeson.object ["kind" Aeson..= ("unavailable" :: Text), "reason" Aeson..= reason]

responseHeaders :: ResponseMetadata -> [(Text, Text)]
responseHeaders metadata =
  [ ("X-Adrai-Generation", generationText (responseGeneration metadata)),
    ("X-Adrai-As-Of", asOfText (responseAsOf metadata))
  ]
  where
    generationText (Generation value) = Text.pack (show value)
    asOfText (AsOfCommit oid) = gitOidText oid
    asOfText (AsOfComparison before after) = gitOidText before <> ".." <> gitOidText after
    asOfText (AsOfUnavailable reason) = "unavailable:" <> reason

data ApiErrorResponse = ApiErrorResponse
  { apiErrorResponseMetadata :: ResponseMetadata,
    apiErrorResponseError :: ApiError
  }
  deriving (Eq, Show)

errorResponseJson :: ApiErrorResponse -> Aeson.Value
errorResponseJson response = Aeson.object
  [ "schema" Aeson..= ("adrai/api/v1" :: Text),
    "metadata" Aeson..= metadataJson (apiErrorResponseMetadata response),
    "error" Aeson..= Aeson.object
      [ "category" Aeson..= errorCategoryText (apiErrorCategory problem),
        "status" Aeson..= apiErrorStatus problem,
        "code" Aeson..= apiErrorCode problem,
        "message" Aeson..= apiErrorMessage problem
      ]
  ]
  where
    problem = apiErrorResponseError response

errorResponseHeaders :: ApiErrorResponse -> [(Text, Text)]
errorResponseHeaders = responseHeaders . apiErrorResponseMetadata

errorCategoryText :: ApiErrorCategory -> Text
errorCategoryText category = case category of
  MalformedInput -> "malformed-input"
  AuthenticationFailure -> "authentication"
  OriginFailure -> "origin"
  MissingRoute -> "missing-route"
  MissingResource -> "missing-resource"
  StaleState -> "stale-state"
  SemanticConflict -> "semantic-conflict"
  ServiceFailure -> "service-failure"

data ApiErrorCategory
  = MalformedInput
  | AuthenticationFailure
  | OriginFailure
  | MissingRoute
  | MissingResource
  | StaleState
  | SemanticConflict
  | ServiceFailure
  deriving (Eq, Show)

data ApiError = ApiError
  { apiErrorCategory :: ApiErrorCategory,
    apiErrorStatus :: Int,
    apiErrorCode :: Text,
    apiErrorMessage :: Text
  }
  deriving (Eq, Show)

badField :: Text -> Text -> ApiError
badField field message = ApiError MalformedInput 400 "invalid-field" (field <> ": " <> message)

serviceFailure :: Text -> ApiError
serviceFailure = ApiError ServiceFailure 500 "service-failure"

staleStateFailure :: Text -> ApiError
staleStateFailure = ApiError StaleState 409 "stale-state"

headBasisText :: RepositoryHeadBasis -> Text
headBasisText (AttachedHead reference) = gitRefText reference
headBasisText DetachedHead = "DETACHED"
