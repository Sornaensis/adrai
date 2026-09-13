{-# LANGUAGE OverloadedStrings #-}

module Adrai.WebContractTest (tests) where

import qualified Adrai.CliTypes as CliTypes
import qualified Adrai.Format
import qualified Adrai.Git as Git
import qualified Adrai.Provenance as Provenance
import qualified Adrai.Service.Mutation as Mutation
import qualified Adrai.Service.PostCommitIndex as PostCommit
import qualified Adrai.Types
import qualified Adrai.Web.Api as Api
import qualified Adrai.Web.Events as Events
import qualified Adrai.Web.Security as Security
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (Pair)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import Data.Either (isLeft, isRight)
import Data.List (isInfixOf)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?), (@?=))

tests :: TestTree
tests = testGroup "web-contracts"
  [ testCase "exact method and route matrix is closed" testRouteMatrix,
    testCase "wrong methods invalid ids and missing routes are distinct" testRouteFailures,
    testCase "query codecs reject duplicates unknown fields and bounds" testQueryStrictness,
    testCase "all mutations require their distinct state preconditions" testMutationPreconditions,
    testCase "amend conversion preserves shared typed request and rejects unknown JSON" testSharedConversion,
    testCase "binding accepts linked worktrees and rejects bare or undiscovered repositories" testRepositoryBinding,
    testCase "response metadata preserves the exact query snapshot and compare operands" testResponseMetadata,
    testCase "repository state tokens are distinct from ADR state tokens" testTokenKinds,
    testCase "events encode adrai events v1 and authenticate before data" testEvents,
    testCase "credentials admit cookie bearer and bootstrap while confining query tokens" testCredentialAdmission,
    testCase "host origin null-origin and bound-port checks fail closed" testAuthorityAdmission,
    testCase "mutations require bearer JSON and accept the matching browser cookie" testMutationAdmission,
    testCase "credentials frames and admission diagnostics are redacted" testRedaction
  ]

adrText, stateText, oidText, repositoryTokenText :: Text
adrText = "A00000000000000000000000000"
stateText = "S0000000000000000000000"
oidText = "0000000000000000000000000000000000000000"
repositoryTokenText = "Raaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

adrId :: Adrai.Types.AdrId
adrId = either (error . show) id (Adrai.Types.mkAdrId adrText)

stateToken :: Adrai.Types.StateToken
stateToken = either (error . show) id (Adrai.Types.mkStateToken stateText)

oid :: Provenance.GitOid
oid = either (error . show) id (Provenance.mkGitOid oidText)

testRouteMatrix :: IO ()
testRouteMatrix = do
  let gets =
        [ ["api", "v1", "repository"], ["api", "v1", "search"], ["api", "v1", "relevant"],
          ["api", "v1", "adrs", adrText], ["api", "v1", "history"], ["api", "v1", "compare"],
          ["api", "v1", "conflicts"], ["api", "v1", "doctor"], ["api", "v1", "events"]
        ]
      posts =
        [ ["api", "v1", "adrs"], ["api", "v1", "adrs", adrText, "amend"],
          ["api", "v1", "adrs", adrText, "scope"], ["api", "v1", "adrs", adrText, "domain"],
          ["api", "v1", "adrs", adrText, "obsolete"], ["api", "v1", "adrs", adrText, "reactivate"]
        ]
  assertBool "all GET routes parse" (all (isRight . Api.parseApiRoute Api.GetMethod) gets)
  assertBool "all POST routes parse" (all (isRight . Api.parseApiRoute Api.PostMethod) posts)
  length gets @?= 9
  length posts @?= 6

testRouteFailures :: IO ()
testRouteFailures = do
  category (Api.parseApiRoute Api.PostMethod ["api", "v1", "search"]) @?= Just Api.MalformedInput
  status (Api.parseApiRoute Api.GetMethod ["api", "v1", "missing"]) @?= Just 404
  code (Api.parseApiRoute Api.GetMethod ["api", "v1", "adrs", "bad"]) @?= Just "invalid-field"
  where
    category = either (Just . Api.apiErrorCategory) (const Nothing)
    status = either (Just . Api.apiErrorStatus) (const Nothing)
    code = either (Just . Api.apiErrorCode) (const Nothing)

testQueryStrictness :: IO ()
testQueryStrictness = do
  failureCode (Api.decodeApiRequest Api.defaultApiLimits Api.SearchRoute [("q", Just "a"), ("q", Just "b")] ByteString.empty) @?= "duplicate-query"
  failureCode (Api.decodeApiRequest Api.defaultApiLimits Api.SearchRoute [("q", Just "a"), ("wat", Just "b")] ByteString.empty) @?= "unknown-query"
  failureCode (Api.decodeApiRequest Api.defaultApiLimits Api.SearchRoute [("q", Just "a"), ("limit", Just "101")] ByteString.empty) @?= "invalid-field"
  let tiny = Api.defaultApiLimits {Api.maximumQueryBytes = 1}
  failureCode (Api.decodeApiRequest tiny Api.SearchRoute [("q", Just "abcd")] ByteString.empty) @?= "query-too-large"
  let utf8Bound = Api.defaultApiLimits {Api.maximumQueryBytes = 5}
  failureCode (Api.decodeApiRequest utf8Bound Api.SearchRoute [("q", Just "ééé")] ByteString.empty) @?= "query-too-large"
  mapM_ (\raw -> failureCode (Api.decodeApiRequest Api.defaultApiLimits Api.SearchRoute [("q", Just "x"), ("at", Just raw)] ByteString.empty) @?= "invalid-field") ["", "-option", "HEAD\NULtail", "HEAD\nother"]
  failureCode (Api.decodeApiRequest Api.defaultApiLimits Api.CompareRoute [("from", Just ""), ("to", Just "HEAD")] ByteString.empty) @?= "invalid-field"
  failureCode (Api.decodeApiRequest Api.defaultApiLimits Api.HistoryRoute [("adr", Just "bad")] ByteString.empty) @?= "invalid-field"

testMutationPreconditions :: IO ()
testMutationPreconditions = do
  assertBool "create rejects a missing repository token" (mentions "repository_state" (Api.decodeApiRequest Api.defaultApiLimits Api.CreateRoute [] (jsonObject ["title" Aeson..= ("t" :: Text), "summary" Aeson..= ("s" :: Text), "body" Aeson..= ("b" :: Text), "actor" Aeson..= actorJson])))
  let cases =
        [ (Api.AmendRoute adrId, ["change_summary" Aeson..= ("why" :: Text), "title" Aeson..= ("t" :: Text), "summary" Aeson..= ("s" :: Text), "body" Aeson..= ("b" :: Text)]),
          (Api.ScopeRoute adrId, ["reason" Aeson..= ("why" :: Text), "mode" Aeson..= ("delta" :: Text)]),
          (Api.DomainRoute adrId, ["reason" Aeson..= ("why" :: Text), "mode" Aeson..= ("delta" :: Text)]),
          (Api.ObsoleteRoute adrId, ["reason" Aeson..= ("why" :: Text)]),
          (Api.ReactivateRoute adrId, ["reason" Aeson..= ("why" :: Text)])
        ]
      body fields = jsonObject (["repository_state" Aeson..= basisJson, "actor" Aeson..= actorJson] <> fields)
  assertBool "every existing ADR mutation rejects missing state_token" (all (\(route, fields) -> mentions "state_token" (Api.decodeApiRequest Api.defaultApiLimits route [] (body fields))) cases)

testSharedConversion :: IO ()
testSharedConversion = do
  let fields =
        [ "repository_state" Aeson..= basisJson, "state_token" Aeson..= stateText,
          "change_summary" Aeson..= ("why" :: Text), "title" Aeson..= ("title" :: Text),
          "summary" Aeson..= ("summary" :: Text), "body" Aeson..= ("body" :: Text), "actor" Aeson..= actorJson
        ]
  case Api.decodeApiRequest Api.defaultApiLimits (Api.AmendRoute adrId) [] (jsonObject fields) of
    Right (Api.ApiAmendRequest mutation) -> do
      Api.existingAdrState mutation @?= stateToken
      CliTypes.amendRequestExpectedState (Api.existingSharedRequest mutation) @?= Just stateToken
      CliTypes.amendRequestAdr (Api.existingSharedRequest mutation) @?= adrId
    other -> assertFailure (show other)
  failureCode (Api.decodeApiRequest Api.defaultApiLimits (Api.AmendRoute adrId) [] (jsonObject (fields <> ["unknown" Aeson..= True]))) @?= "unknown-json-field"
  let statusFields = ["repository_state" Aeson..= basisJson, "state_token" Aeson..= stateText, "reason" Aeson..= ("why" :: Text), "actor" Aeson..= actorJson]
  case Api.decodeApiRequest Api.defaultApiLimits (Api.ObsoleteRoute adrId) [] (jsonObject statusFields) of
    Right (Api.ApiObsoleteRequest mutation) -> Api.existingAdr mutation @?= adrId
    other -> assertFailure (show other)
  case Api.decodeApiRequest Api.defaultApiLimits (Api.ReactivateRoute adrId) [] (jsonObject statusFields) of
    Right (Api.ApiReactivateRequest mutation) -> Api.existingAdr mutation @?= adrId
    other -> assertFailure (show other)
  let mutationBase = ["repository_state" Aeson..= basisJson, "state_token" Aeson..= stateText, "reason" Aeson..= ("why" :: Text), "actor" Aeson..= actorJson]
      domainUnion = mutationBase <> ["mode" Aeson..= ("reviewed" :: Text), "domains" Aeson..= (["a"] :: [Text]), "add" Aeson..= (["b"] :: [Text])]
      domainMissing = mutationBase <> ["mode" Aeson..= ("reviewed" :: Text), "add" Aeson..= (["b"] :: [Text])]
      scopeUnion = mutationBase <> ["mode" Aeson..= ("reviewed" :: Text), "patterns" Aeson..= (["src/**"] :: [Text]), "add" Aeson..= (["test/**"] :: [Text])]
  failureCode (Api.decodeApiRequest Api.defaultApiLimits (Api.DomainRoute adrId) [] (jsonObject domainUnion)) @?= "invalid-field"
  failureCode (Api.decodeApiRequest Api.defaultApiLimits (Api.DomainRoute adrId) [] (jsonObject domainMissing)) @?= "invalid-field"
  failureCode (Api.decodeApiRequest Api.defaultApiLimits (Api.ScopeRoute adrId) [] (jsonObject scopeUnion)) @?= "invalid-field"

testRepositoryBinding :: IO ()
testRepositoryBinding = do
  let linked = repository Git.LinkedWorktree (Just "D:\\worktree") True
      bare = repository Git.BareRepository Nothing True
  assertBool "linked worktree accepted even with bare common store" (isRight (Api.validateRepositoryBinding (Right linked)))
  case Api.validateRepositoryBinding (Right linked) of
    Right bound -> do
      let mainRef = either (error . show) Api.AttachedHead (Adrai.Types.mkGitRef "refs/heads/main")
          otherRef = either (error . show) Api.AttachedHead (Adrai.Types.mkGitRef "refs/heads/other")
      assertBool "same OID on another attached branch has a different token" (Api.mkRepositoryStateToken bound oid mainRef /= Api.mkRepositoryStateToken bound oid otherRef)
      assertBool "detached basis has a different token" (Api.mkRepositoryStateToken bound oid mainRef /= Api.mkRepositoryStateToken bound oid Api.DetachedHead)
    Left problem -> assertFailure (show problem)
  Api.validateRepositoryBinding (Right bare) @?= Left Api.BareRepositoryRejected
  assertBool "discovery failure stays typed" (isLeft (Api.validateRepositoryBinding (Left (Git.GitNotRepository "D:\\missing"))))
  where
    repository layout root commonBare = Git.Repository (Git.GitClient "git") root "D:\\gitdir" "D:\\common" layout commonBare (maybe "D:\\common" id root)

testResponseMetadata :: IO ()
testResponseMetadata = do
  let other = either (error . show) id (Provenance.mkGitOid "1111111111111111111111111111111111111111")
      single = Api.ResponseMetadata (Api.mkGeneration 7) (Api.AsOfCommit oid)
      comparison = Api.ResponseMetadata (Api.mkGeneration 8) (Api.AsOfComparison oid other)
  lookup "X-Adrai-As-Of" (Api.responseHeaders single) @?= Just oidText
  lookup "X-Adrai-As-Of" (Api.responseHeaders comparison) @?= Just (oidText <> "..1111111111111111111111111111111111111111")
  let linked = Git.Repository (Git.GitClient "git") (Just "D:\\worktree") "D:\\gitdir" "D:\\common" Git.LinkedWorktree True "D:\\worktree"
      headBasis = either (error . show) Api.AttachedHead (Adrai.Types.mkGitRef "refs/heads/main")
  bound <- either (assertFailure . show) pure (Api.validateRepositoryBinding (Right linked))
  let repositoryPayload = Api.ApiRepositoryResult (Api.repositoryResult bound oid headBasis)
      repositoryWire = encoded (Api.responseJson (Api.ApiResponse single repositoryPayload))
  repositoryWire `contains` oidText @? "exact resolved OID missing"
  repositoryWire `contains` (Api.repositoryStateTokenText (Api.mkRepositoryStateToken bound oid headBasis)) @? "repository state token missing"
  let created = Mutation.CreateResult "operation" adrId (typedRecord "R00000000000000000000000000") (typedConnection "C00000000000000000000000000") (typedConnection "C11111111111111111111111111") (typedConnection "C22222222222222222222222222") oid [Adrai.Types.RepoPath "architecture/adrai/decisions/a.md"] True Nothing
      indexed = PostCommit.PostCommitIndexResult True (Just "cache.db") (Just oid) [PostCommit.IndexWarning "warning" "retained"] Nothing
      mutationWire = encoded (Api.apiResultPayload (Api.ApiCreateResult created [] indexed Nothing))
  mutationWire `contains` "\"committed\":true" @? "durable commit result lost"
  mutationWire `contains` "\"index_warnings\":1" @? "post-commit index warning lost"
  let unavailable = Api.ResponseMetadata (Api.mkGeneration 9) (Api.AsOfUnavailable "pre-authentication")
      errorWire = encoded (Api.errorResponseJson (Api.ApiErrorResponse unavailable (Api.serviceFailure "failed")))
  errorWire `contains` "pre-authentication" @? "error metadata missing"
  lookup "X-Adrai-Generation" (Api.errorResponseHeaders (Api.ApiErrorResponse unavailable (Api.serviceFailure "failed"))) @?= Just "9"

testTokenKinds :: IO ()
testTokenKinds = do
  assertBool "repository token parses" (isRight (Api.parseRepositoryStateToken repositoryTokenText))
  assertBool "ADR token cannot parse as repository token" (isLeft (Api.parseRepositoryStateToken stateText))
  assertBool "repository token cannot parse as ADR token" (isLeft (Adrai.Format.parseStateToken repositoryTokenText))

testEvents :: IO ()
testEvents = do
  let envelope = Events.EventEnvelope 4 (Events.EventAt oid) (Events.RepositoryInvalidated [Events.HeadChanged, Events.IndexChanged])
      wire = encoded (Events.eventEnvelopeJson envelope)
  wire `contains` Events.eventsSchema @? "event schema missing"
  wire `contains` oidText @? "event exact as_of missing"
  let frame = Events.decodeClientFrame 4096 (jsonObject ["type" Aeson..= ("authenticate" :: Text), "credential" Aeson..= ("secret" :: Text)])
  Events.acceptClientFrame (== "secret") Events.AwaitingAuthentication frame @?= Events.SocketAuthenticated
  Events.acceptClientFrame (const True) Events.SocketAuthenticated frame @?= Events.SocketClosed Events.AuthenticationRepeated
  Events.acceptClientFrame (const True) Events.AwaitingAuthentication (Left Events.MalformedFrame) @?= Events.SocketClosed Events.MalformedAuthentication
  Events.decodeClientFrame 1 "xx" @?= Left Events.FrameTooLarge
  Events.acceptClientFrame (== "secret") Events.AwaitingAuthentication (Right (Events.AuthenticateFrame "wrong")) @?= Events.SocketClosed Events.AuthenticationRejected
  let nonAuth = Events.decodeClientFrame 4096 (jsonObject ["type" Aeson..= ("event" :: Text), "credential" Aeson..= ("secret" :: Text)])
  Events.acceptClientFrame (const True) Events.AwaitingAuthentication nonAuth @?= Events.SocketClosed Events.MalformedAuthentication

testCredentialAdmission :: IO ()
testCredentialAdmission = do
  (authority, secret, token) <- securityFixture
  let bearer = (baseSecurity authority Security.ApiReadTarget) {Security.securityAuthorization = ["Bearer " <> token], Security.securityQuery = [("q", Just "search terms")]}
      cookie = (baseSecurity authority Security.ApiReadTarget) {Security.securityCookies = [(Security.processCookieName authority secret, token)]}
      bootstrap = (baseSecurity authority Security.BootstrapTarget) {Security.securityQuery = [("token", Just token)]}
      leaked = bearer {Security.securityQuery = [("token", Just token)]}
  assertBool "ordinary bearer with route query admitted" (isRight (Security.admitRequest authority secret bearer))
  assertBool "matching process cookie admitted" (isRight (Security.admitRequest authority secret cookie))
  assertBool "bootstrap query admitted" (isRight (Security.admitRequest authority secret bootstrap))
  Security.admitRequest authority secret leaked @?= Left Security.QueryCredentialForbidden
  Security.admitRequest authority secret (baseSecurity authority Security.ApiReadTarget) @?= Left Security.MissingCredential
  Security.admitRequest authority secret (bearer {Security.securityAuthorization = ["Bearer wrong"]}) @?= Left Security.InvalidCredential
  Security.admitRequest authority secret (cookie {Security.securityCookies = [(Security.processCookieName authority secret, token), (Security.processCookieName authority secret, token)]}) @?= Left Security.AmbiguousCredential
  Security.admitRequest authority secret (bootstrap {Security.securityQuery = [("token", Just token), ("token", Just token)]}) @?= Left Security.AmbiguousCredential
  Security.admitRequest authority secret (bootstrap {Security.securityMethod = Security.SecurityPost}) @?= Left Security.InvalidMethodForTarget

testAuthorityAdmission :: IO ()
testAuthorityAdmission = do
  (authority, secret, token) <- securityFixture
  let valid = (baseSecurity authority Security.ApiReadTarget) {Security.securityAuthorization = ["Bearer " <> token]}
      post = valid {Security.securityMethod = Security.SecurityPost, Security.securityTarget = Security.MutationTarget, Security.securityOrigins = [Security.authorityOrigin authority], Security.securityContentTypes = ["application/json"]}
  Security.admitRequest authority secret (valid {Security.securityHosts = ["localhost:4400"]}) @?= Left Security.InvalidHost
  Security.admitRequest authority secret (post {Security.securityOrigins = ["null"]}) @?= Left Security.InvalidOrigin
  Security.admitRequest authority secret (post {Security.securityOrigins = []}) @?= Left Security.MissingOrigin
  Security.admitRequest authority secret (post {Security.securityOrigins = ["http://127.0.0.1:4401"]}) @?= Left Security.InvalidOrigin

testMutationAdmission :: IO ()
testMutationAdmission = do
  (authority, secret, token) <- securityFixture
  let valid = (baseSecurity authority Security.MutationTarget)
        { Security.securityMethod = Security.SecurityPost,
          Security.securityOrigins = [Security.authorityOrigin authority],
          Security.securityAuthorization = ["Bearer " <> token],
          Security.securityCookies = [(Security.processCookieName authority secret, token)],
          Security.securityContentTypes = ["application/json"]
        }
  assertBool "normal browser cookie plus explicit bearer accepted" (isRight (Security.admitRequest authority secret valid))
  Security.admitRequest authority secret (valid {Security.securityAuthorization = []}) @?= Left Security.BearerRequired
  Security.admitRequest authority secret (valid {Security.securityContentTypes = ["text/plain"]}) @?= Left Security.JsonContentTypeRequired
  Security.admitRequest authority secret (valid {Security.securityAuthorization = ["Basic nope"]}) @?= Left Security.InvalidCredential
  Security.admitRequest authority secret (valid {Security.securityAuthorization = ["Bearer " <> token, "Bearer " <> token]}) @?= Left Security.AmbiguousCredential

testRedaction :: IO ()
testRedaction = do
  (authority, secret, token) <- securityFixture
  let request = (baseSecurity authority Security.ApiReadTarget) {Security.securityAuthorization = ["Bearer " <> token]}
      frame = Events.AuthenticateFrame token
  assertBool "secret show is redacted" (not (Text.unpack token `isInfixOf` show secret))
  assertBool "request show is redacted" (not (Text.unpack token `isInfixOf` show request))
  assertBool "frame show is redacted" (not (Text.unpack token `isInfixOf` show frame))

securityFixture :: IO (Security.BoundAuthority, Security.ProcessSecret, Text)
securityFixture = do
  authority <- either (assertFailure . show) pure (Security.mkBoundAuthority 4400 "process-a")
  secret <- either (assertFailure . show) pure (Security.mkProcessSecret (ByteString.replicate 32 97))
  pure (authority, secret, Security.processSecretText secret)

baseSecurity :: Security.BoundAuthority -> Security.AdmissionTarget -> Security.SecurityRequest
baseSecurity authority target = Security.SecurityRequest
  { Security.securityMethod = Security.SecurityGet,
    Security.securityTarget = target,
    Security.securityHosts = [Security.authorityHost authority],
    Security.securityOrigins = [],
    Security.securityAuthorization = [],
    Security.securityCookies = [],
    Security.securityQuery = [],
    Security.securityContentTypes = []
  }

basisJson :: Aeson.Value
basisJson = Aeson.object ["kind" Aeson..= ("repository" :: Text), "token" Aeson..= repositoryTokenText, "head" Aeson..= oidText, "head_ref" Aeson..= ("refs/heads/main" :: Text)]

actorJson :: Aeson.Value
actorJson = Aeson.object ["kind" Aeson..= ("human" :: Text), "id" Aeson..= ("web-user" :: Text)]

jsonObject :: [Pair] -> ByteString.ByteString
jsonObject = LazyByteString.toStrict . Aeson.encode . Aeson.object

failureCode :: Either Api.ApiError value -> Text
failureCode = either Api.apiErrorCode (const "success")

mentions :: Text -> Either Api.ApiError value -> Bool
mentions needle = either (Text.isInfixOf needle . Api.apiErrorMessage) (const False)

encoded :: Aeson.Value -> Text
encoded = Data.Text.Encoding.decodeUtf8 . LazyByteString.toStrict . Aeson.encode

contains :: Text -> Text -> Bool
contains haystack needle = needle `Text.isInfixOf` haystack

typedRecord :: Text -> Adrai.Types.RecordId
typedRecord = either (error . show) id . Adrai.Types.mkRecordId

typedConnection :: Text -> Adrai.Types.ConnectionId
typedConnection = either (error . show) id . Adrai.Types.mkConnectionId
