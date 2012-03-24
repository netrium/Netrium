#!/usr/bin/runghc

-- On Unix/MacOSX run as:
--   ./generate-docs.hs
--
-- On Windows:
--    runghc generate-docs.hs
--
-- Then point your browser at ./doc/index.html

import System.Process
import System.Directory
import System.FilePath
import System.Exit
import Control.Monad
import Data.Char

outDir   = "doc"
shareDir = "share"
srcDir   = "src"

main = do
  libmodules <- getLibModules
  exitcode <- rawSystem "haddock" (haddockArgs libmodules)
  when (exitcode /= ExitSuccess) $
    putStrLn "Generating 'haddock' documentation failed"
  exitWith exitcode

getLibModules :: IO [FilePath]
getLibModules = do
  files <- getDirectoryContents shareDir
  return [ shareDir </> file
         | file <- files
         , takeExtension file == ".hs", isUpper (head file) ]

haddockArgs libmodules =
  [ "--html"
  , "-o", outDir
  , "--optghc=-i" ++ srcDir ]
  ++ libmodules
