module Main (main) where

import Control.Monad (filterM)
import System.Directory (doesFileExist)
import System.Exit (exitFailure)

expectedDocs :: [FilePath]
expectedDocs =
  [ "docs/README.md",
    "docs/INSTALL.md",
    "docs/USAGE.md",
    "docs/WEB.md",
    "docs/CONFLICTS.md",
    "docs/FORMAT.md",
    "docs/CACHE.md",
    "docs/SEARCH.md",
    "docs/DEVELOPMENT.md"
  ]

main :: IO ()
main = do
  missing <- filterM (fmap not . doesFileExist) expectedDocs
  if null missing
    then putStrLn "All documentation files are present."
    else do
      putStrLn "Missing documentation files:"
      mapM_ putStrLn missing
      exitFailure
