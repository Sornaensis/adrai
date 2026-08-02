{-# LANGUAGE OverloadedStrings #-}

module Adrai.ManagedPathContractTest (tests) where

import Adrai.Format.Document
import Adrai.ManagedPath
import Adrai.Provenance
import Adrai.Types
import qualified Data.ByteString as BS
import Data.Either (isLeft)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TextEncoding
import System.Directory
  ( canonicalizePath,
    createDirectory,
    createDirectoryLink,
    doesFileExist,
  )
import System.FilePath ((</>), normalise)
import qualified System.Exit as Exit
import System.Info (os)
import System.IO.Error (tryIOError)
import System.IO.Temp (withSystemTempDirectory)
import System.Process.Typed (runProcess, shell)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit ((@?=), assertBool, assertFailure, testCase)

tests :: TestTree
tests =
  testGroup
    "managed path and location contract"
    [ testCase "RepoPath rejects lexical write escapes before resolution" lexicalInvalids,
      testCase "Unicode and a nonexistent leaf remain below verified parents" unicodeNonexistentLeaf,
      testCase "managed documents require their exact canonical path" canonicalLocationMismatches,
      testCase "parsed semantics must use the canonical serializer shape" canonicalSemantic,
      testCase "parsedManagedBytes retains exact append-only source bytes" exactSourceBytes,
      testCase "an immutable destination must not already exist" existingLeafRejected,
      testCase "configured managed roots are physically distinct" physicalManagedRoots,
      testCase "a linked parent cannot redirect a managed write" linkedParentEscape
    ]

lexicalInvalids :: IO ()
lexicalInvalids = do
  assertBool "parent segment" (isLeft (mkRepoPath "architecture/../outside.md"))
  assertBool "absolute path" (isLeft (mkRepoPath "/outside.md"))
  assertBool "drive-qualified path" (isLeft (mkRepoPath "C:/outside.md"))
  assertBool "UNC path" (isLeft (mkRepoPath "//server/share/outside.md"))
  assertBool "backslash path" (isLeft (mkRepoPath "architecture\\outside.md"))

unicodeNonexistentLeaf :: IO ()
unicodeNonexistentLeaf =
  withSystemTempDirectory "adrai-managed-path" $ \temporary -> do
    let root = temporary </> "r\233pertoire"
        existingParent = root </> "architecture"
    createDirectory root
    createDirectory existingParent
    repositoryPath <- pathOrFail "architecture/\916\959\954\953\956\942/\26410\20316\25104.md"
    resolved <- resolveManagedWritePath root repositoryPath >>= assertRight
    canonicalRoot <- canonicalizePath root
    resolved
      @?= normalise
        ( canonicalRoot
            </> "architecture"
            </> "\916\959\954\953\956\942"
            </> "\26410\20316\25104.md"
        )
    exists <- doesFileExist resolved
    assertBool "resolution must not create the leaf" (not exists)

canonicalLocationMismatches :: IO ()
canonicalLocationMismatches = do
  decisionBytes <- BS.readFile decisionFixturePath
  let paths = configManagedPaths defaultConfig
  expectedDecision <- pathOrFail canonicalDecisionPath
  mapM_
    (assertDecisionMismatch paths decisionBytes expectedDecision)
    [ "elsewhere/R000/R00000000000000000000000000--stable-cache-identity.decision.md",
      "architecture/adrai/decisions/R999/R00000000000000000000000000--stable-cache-identity.decision.md",
      "architecture/adrai/decisions/R000/R00000000000000000000000000--wrong-slug.decision.md"
    ]

  managedConnection <- connectionFixture
  connectionBytes <- sealFixture managedConnection
  expectedConnection <- assertRight (canonicalManagedPath paths managedConnection)
  wrongRelation <-
    pathOrFail
      "architecture/adrai/connections/C000/C00000000000000000000000000--domains.connection.md"
  parsedConnection <- assertRight (parseManagedDocument wrongRelation connectionBytes)
  validateManagedLocation paths parsedConnection
    @?= Left (DocumentNonCanonicalPath wrongRelation expectedConnection)

assertDecisionMismatch :: ManagedPaths -> BS.ByteString -> RepoPath -> Text -> IO ()
assertDecisionMismatch paths bytes expected actualText = do
  actual <- pathOrFail actualText
  parsed <- assertRight (parseManagedDocument actual bytes)
  validateManagedLocation paths parsed
    @?= Left (DocumentNonCanonicalPath actual expected)

canonicalSemantic :: IO ()
canonicalSemantic = do
  canonical <- BS.readFile decisionFixturePath
  path <- pathOrFail canonicalDecisionPath
  parsed <- assertRight (parseManagedDocument path canonical)
  validateManagedLocation (configManagedPaths defaultConfig) parsed @?= Right ()
  let canonicalText =
        Text.replace "\r" "\n" (Text.replace "\r\n" "\n" (TextEncoding.decodeUtf8 canonical))
      reordered =
        Text.replace
          ( "title = \"Stable cache identity\"\n"
              <> "summary = \"Cache keys derive from semantic inputs.\""
          )
          ( "summary = \"Cache keys derive from semantic inputs.\"\n"
              <> "title = \"Stable cache identity\""
          )
          canonicalText
  parseManagedDocument path (TextEncoding.encodeUtf8 reordered)
    @?= Left DocumentNonCanonicalSemantic

exactSourceBytes :: IO ()
exactSourceBytes = do
  canonical <- BS.readFile decisionFixturePath
  path <- pathOrFail canonicalDecisionPath
  let crlf =
        TextEncoding.encodeUtf8
          (Text.replace "\n" "\r\n" (TextEncoding.decodeUtf8 canonical))
  parsed <- assertRight (parseManagedDocument path crlf)
  parsedManagedBytes parsed @?= crlf
  assertBool "the fixture exercises a byte-distinct encoding" (crlf /= canonical)

existingLeafRejected :: IO ()
existingLeafRejected =
  withSystemTempDirectory "adrai-managed-existing" $ \root -> do
    let leaf = root </> "existing.md"
    BS.writeFile leaf "already immutable"
    repositoryPath <- pathOrFail "existing.md"
    resolved <- resolveManagedWritePath root repositoryPath
    resolved @?= Left (ManagedPathLeafExists (normalise leaf))

physicalManagedRoots :: IO ()
physicalManagedRoots =
  withSystemTempDirectory "adrai-managed-roots" $ \root -> do
    decisions <- pathOrFail "architecture/decisions"
    connections <- pathOrFail "architecture/connections"
    paths <- assertRight (mkManagedPaths decisions connections)
    result <- validateManagedRoots root paths
    result @?= Right ()

linkedParentEscape :: IO ()
linkedParentEscape =
  withSystemTempDirectory "adrai-managed-link" $ \temporary -> do
    let root = temporary </> "repository"
        outside = temporary </> "outside"
        linkedParent = root </> "managed"
        outsideLeaf = outside </> "escaped.md"
    createDirectory root
    createDirectory outside
    createDirectoryRedirect outside linkedParent
    repositoryPath <- pathOrFail "managed/escaped.md"
    result <- resolveManagedWritePath root repositoryPath
    assertBool ("expected redirect rejection, got " <> show result) (isRedirectError result)
    escaped <- doesFileExist outsideLeaf
    assertBool "resolution must never write through the linked parent" (not escaped)

createDirectoryRedirect :: FilePath -> FilePath -> IO ()
createDirectoryRedirect target link
  | os == "mingw32" = do
      let command = "mklink /J \"" <> link <> "\" \"" <> target <> "\""
      result <- runProcess (shell command)
      case result of
        Exit.ExitSuccess -> pure ()
        Exit.ExitFailure code -> assertFailure ("failed to create Windows junction, exit " <> show code)
  | otherwise = do
      result <- tryIOError (createDirectoryLink target link)
      case result of
        Right () -> pure ()
        Left problem -> assertFailure ("failed to create directory symlink: " <> show problem)

isRedirectError :: Either ManagedPathError FilePath -> Bool
isRedirectError result =
  case result of
    Left (ManagedPathRedirected _) -> True
    Left (ManagedPathEscapesRoot _ _) -> True
    _ -> False

connectionFixture :: IO ManagedRecord
connectionFixture = do
  identifier <- assertRight (mkConnectionId "C00000000000000000000000000")
  subject <- assertRight (mkAdrId "A00000000000000000000000000")
  fromRecord <- assertRight (mkRecordId "R00000000000000000000000001")
  toRecord <- assertRight (mkRecordId "R00000000000000000000000000")
  pure
    ( ManagedConnection
        ( ConnectionRecord
            identifier
            (AmendsConnection (AmendsPayload subject fromRecord [toRecord]))
            "Replace the previous effective record.\n"
        )
    )

sealFixture :: ManagedRecord -> IO BS.ByteString
sealFixture managed = do
  semantic <- assertRight (renderManagedSemantic managed)
  operation <- assertRight (mkOperationId "O00000000000000000000000000")
  actor <- assertRight (mkActor HumanActor "architect" Nothing)
  basis <- assertRight (mkGitOid (Text.replicate 40 "0"))
  event <- assertRight (mkEventKind "connection.create")
  capsule <-
    assertRight . mkProvenanceCapsule $
      ProvenanceCapsuleInput
        { capsuleInputOperationId = operation,
          capsuleInputObjectId = ProvenanceConnection (connectionIdentifier managed),
          capsuleInputEventKind = event,
          capsuleInputActor = actor,
          capsuleInputTimestampMs = 1700000000000,
          capsuleInputBasis = basis,
          capsuleInputParents = [],
          capsuleInputBranchHint = Nothing,
          capsuleInputUpstreamHint = Nothing,
          capsuleInputLineAnchors = [],
          capsuleInputSemanticDigest = semanticDigest semantic,
          capsuleInputToolVersion = "adrai/1.0.0",
          capsuleInputDigests = ProvenanceInputs Nothing Nothing Nothing
        }
  assertRight (sealManagedDocument managed capsule)

connectionIdentifier :: ManagedRecord -> ConnectionId
connectionIdentifier managed =
  case managed of
    ManagedConnection record -> connectionRecordId record
    ManagedDecision _ -> error "connection fixture unexpectedly contained a decision"

pathOrFail :: Text -> IO RepoPath
pathOrFail = assertRight . mkRepoPath

assertRight :: (Show problem) => Either problem value -> IO value
assertRight result =
  case result of
    Left problem -> assertFailure (show problem) >> fail "unreachable"
    Right value -> pure value

decisionFixturePath :: FilePath
decisionFixturePath = "test/fixtures/contracts/v1/documents/decision-create.sealed.md"

canonicalDecisionPath :: Text
canonicalDecisionPath =
  "architecture/adrai/decisions/R000/R00000000000000000000000000--stable-cache-identity.decision.md"
