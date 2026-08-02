{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

-- | Physical containment checks for paths that are about to be written.
--
-- 'RepoPath' supplies the lexical contract (relative, no @.@ or @..@ segments,
-- no drive/UNC spelling).  This module adds the filesystem contract: every
-- existing component below the canonical repository root must be an ordinary
-- directory component, never a symbolic link, junction, or other reparse-point
-- redirection.  The result is suitable for a later write, although the caller
-- must still avoid introducing a check/write race of its own.
module Adrai.ManagedPath
  ( ManagedPathError (..),
    resolveManagedWritePath,
    validateManagedRoots,
  )
where

import Adrai.Types
  ( ManagedPaths,
    RepoPath,
    managedConnectionPath,
    managedDecisionPath,
    repoPathText,
  )
import qualified Data.Text as Text
import System.Directory
  ( canonicalizePath,
    doesDirectoryExist,
    doesPathExist,
    pathIsSymbolicLink,
  )
import System.FilePath
  ( (</>),
    isAbsolute,
    makeRelative,
    normalise,
    splitDirectories,
  )
import System.IO.Error (catchIOError, isDoesNotExistError, tryIOError)

-- | A failure to turn a lexical repository path into a physically contained
-- write destination.
data ManagedPathError
  = ManagedPathRootMissing FilePath
  | ManagedPathRootNotDirectory FilePath
  | ManagedPathRedirected FilePath
  | ManagedPathEscapesRoot FilePath FilePath
  | ManagedPathAncestorNotDirectory FilePath
  | ManagedPathLeafExists FilePath
  | ManagedPathLeafIsDirectory FilePath
  | ManagedPathRootsOverlap FilePath FilePath
  | ManagedPathIoError FilePath String
  deriving (Eq, Show)

-- | Resolve a typed repository-relative path below a repository root.
--
-- The root itself is canonicalized first, making that resolved directory the
-- containment boundary.  Each existing descendant is then checked before it
-- is traversed.  On Windows, @directory@ implements 'pathIsSymbolicLink' using
-- the reparse-point attribute, so directory junctions are rejected along with
-- ordinary symbolic links.  Once the first nonexistent component is reached,
-- all remaining components are returned lexically below the last verified
-- parent; a descendant cannot already exist below a nonexistent parent.
resolveManagedWritePath :: FilePath -> RepoPath -> IO (Either ManagedPathError FilePath)
resolveManagedWritePath repositoryRoot repositoryPath = do
  attempted <- tryIOError (resolve repositoryRoot repositoryPath)
  pure $
    case attempted of
      Left problem -> Left (ManagedPathIoError repositoryRoot (show problem))
      Right result -> result

resolve :: FilePath -> RepoPath -> IO (Either ManagedPathError FilePath)
resolve repositoryRoot repositoryPath = do
  rootExists <- doesPathExist repositoryRoot
  if not rootExists
    then pure (Left (ManagedPathRootMissing repositoryRoot))
    else do
      rootIsDirectory <- doesDirectoryExist repositoryRoot
      if not rootIsDirectory
        then pure (Left (ManagedPathRootNotDirectory repositoryRoot))
        else do
          canonicalRoot <- canonicalizePath repositoryRoot
          walk True canonicalRoot canonicalRoot (pathSegments repositoryPath)

-- | Resolve both configured managed roots against the physical repository and
-- reject equality or ancestry after platform canonicalization (including an
-- existing Windows 8.3 alias). This complements the portable lexical overlap
-- check performed by 'Adrai.Types.mkManagedPaths'.
validateManagedRoots :: FilePath -> ManagedPaths -> IO (Either ManagedPathError ())
validateManagedRoots repositoryRoot paths = do
  decisionsResult <- resolveManagedRoot repositoryRoot (managedDecisionPath paths)
  connectionsResult <- resolveManagedRoot repositoryRoot (managedConnectionPath paths)
  pure $ do
    decisions <- decisionsResult
    connections <- connectionsResult
    if physicalPathsOverlap decisions connections
      then Left (ManagedPathRootsOverlap decisions connections)
      else Right ()

resolveManagedRoot :: FilePath -> RepoPath -> IO (Either ManagedPathError FilePath)
resolveManagedRoot repositoryRoot repositoryPath = do
  attempted <- tryIOError $ do
    rootExists <- doesPathExist repositoryRoot
    if not rootExists
      then pure (Left (ManagedPathRootMissing repositoryRoot))
      else do
        rootIsDirectory <- doesDirectoryExist repositoryRoot
        if not rootIsDirectory
          then pure (Left (ManagedPathRootNotDirectory repositoryRoot))
          else do
            canonicalRoot <- canonicalizePath repositoryRoot
            walk False canonicalRoot canonicalRoot (pathSegments repositoryPath)
  pure $
    case attempted of
      Left problem -> Left (ManagedPathIoError repositoryRoot (show problem))
      Right result -> result

walk :: Bool -> FilePath -> FilePath -> [FilePath] -> IO (Either ManagedPathError FilePath)
walk requireFreshLeaf root current remaining =
  case remaining of
    [] -> pure (Right current)
    segment : rest -> do
      let candidate = normalise (current </> segment)
      redirected <- isRedirect candidate
      if redirected
        then pure (Left (ManagedPathRedirected candidate))
        else do
          exists <- doesPathExist candidate
          if not exists
            then
              let unresolved = normalise (foldl (</>) candidate rest)
               in pure
                    ( if isContainedBy root unresolved
                        then Right unresolved
                        else Left (ManagedPathEscapesRoot root unresolved)
                    )
            else do
              isDirectory <- doesDirectoryExist candidate
              if not (null rest) && not isDirectory
                then pure (Left (ManagedPathAncestorNotDirectory candidate))
                else
                  if null rest && isDirectory
                    then
                      if requireFreshLeaf
                        then pure (Left (ManagedPathLeafIsDirectory candidate))
                        else containedCanonical root candidate
                    else
                      if null rest
                        then
                          if requireFreshLeaf
                            then pure (Left (ManagedPathLeafExists candidate))
                            else pure (Left (ManagedPathAncestorNotDirectory candidate))
                        else do
                          physical <- canonicalizePath candidate
                          if isContainedBy root physical
                            then walk requireFreshLeaf root physical rest
                            else pure (Left (ManagedPathEscapesRoot root physical))

containedCanonical :: FilePath -> FilePath -> IO (Either ManagedPathError FilePath)
containedCanonical root candidate = do
  physical <- canonicalizePath candidate
  pure
    ( if isContainedBy root physical
        then Right physical
        else Left (ManagedPathEscapesRoot root physical)
    )

-- A dangling link may report nonexistent through 'doesPathExist', so the link
-- check deliberately comes first.  Missing ordinary paths are not errors here.
isRedirect :: FilePath -> IO Bool
isRedirect path =
  catchIOError
    (pathIsSymbolicLink path)
    (\problem -> if isDoesNotExistError problem then pure False else ioError problem)

isContainedBy :: FilePath -> FilePath -> Bool
isContainedBy root candidate =
  let relative = makeRelative (normalise root) (normalise candidate)
      components = splitDirectories relative
   in not (isAbsolute relative)
        && all (/= "..") components

physicalPathsOverlap :: FilePath -> FilePath -> Bool
physicalPathsOverlap first second = isContainedBy first second || isContainedBy second first

pathSegments :: RepoPath -> [FilePath]
pathSegments = map Text.unpack . Text.splitOn "/" . repoPathText
