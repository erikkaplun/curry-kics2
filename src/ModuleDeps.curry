--- --------------------------------------------------------------------------
--- Computation of dependendies between Curry modules.
---
--- This module implements the functions to compute the dependency
--- information between Curry modules.
---
--- @author  Björn Peemöller, Fabian Skrlac
--- @version April 2014
--- --------------------------------------------------------------------------

module ModuleDeps (ModuleIdent, Source, deps) where

import Directory
import Distribution
import FileGoodies (lookupFileInPath)
import FilePath    ( FilePath, dropExtension, dropTrailingPathSeparator
                   , takeBaseName)
import FiniteMap   (FM, emptyFM, addToFM, fmToList, lookupFM)
import FlatCurry   (readFlatCurryWithParseOptions, Prog (..),flatCurryFileName)
import Function    (second)
import List        (intercalate, partition)
import Maybe       (fromJust, isJust)

import CompilerOpts
import Files
import SCC

type ModuleIdent = String
type Source      = (FilePath, Prog) -- file name, code
type SourceEnv   = FM ModuleIdent (Maybe Source)

--- Compute all dependendies for a given module
--- @param opts - compiler options
--- @param fn   - file name of the module
--- @return     - topologically sorted list of all dependendies
deps :: Options -> String -> IO ([(ModuleIdent, Source)], [String])
deps opts fn = do
  mEnv <- sourceDeps opts (dropExtension $ takeBaseName fn) fn (emptyFM (<))
  fcyvalid <- isFlatCurryValid fn
  let (mods1, errs1) = filterMissing mEnv -- handle missing modules
      (mods2, errs2) = flattenDeps mods1  -- check for cyclic imports
                                          -- and sort topologically
  return (mods2, if fcyvalid then errs1 ++ errs2 else ["Compilation aborted"])

-- Has the given program name a valid FlatCurry file?
-- Used to check the result of the front end.
isFlatCurryValid :: String -> IO Bool
isFlatCurryValid fname = do
  let fcyname = flatCurryFileName fname
  existcy  <- doesFileExist fname
  existfcy <- doesFileExist fcyname
  if existcy && existfcy
   then do cymtime  <- getModificationTime fname
           fcymtime <- getModificationTime fcyname
           return (fcymtime >= cymtime)
   else return False

moduleDeps :: Options -> SourceEnv -> ModuleIdent -> IO SourceEnv
moduleDeps opts mEnv m = case lookupFM mEnv m of
  Just _  -> return mEnv
  Nothing -> do
    mbFile <- lookupModule opts m
    case mbFile of
      Nothing -> return $ addToFM mEnv m Nothing
      Just fn -> sourceDeps opts m fn mEnv

lookupModule :: Options -> String -> IO (Maybe String)
lookupModule opts mod = lookupFileInPath mod [".curry", ".lcurry"]
                        (map dropTrailingPathSeparator importPaths)
  where importPaths = "." : opts :> optImportPaths

sourceDeps :: Options -> ModuleIdent -> String -> SourceEnv -> IO SourceEnv
sourceDeps opts m fn mEnv = do
  fcy@(Prog _ is _ _ _) <- readFlatCurryWithParseOptions (dropExtension fn)
                         $ setFullPath importPaths
                         $ setQuiet True
                         $ setSpecials (opts :> optParser)
                           defaultParams
  foldIO (moduleDeps opts) (addToFM mEnv m (Just (fn, fcy))) is
 where importPaths = "." : opts :> optImportPaths

filterMissing :: SourceEnv -> ([(ModuleIdent, Source)], [String])
filterMissing env = (map (second fromJust) present, errs) where
  errs = map (\(m, _) -> "Module " ++ m ++ " could not be found") missing
  (present, missing) = partition (isJust . snd) $ fmToList env

--- Convert the dependency map into a topologically sorted dependency list
--- and a list of errors for cyclic imports.
flattenDeps :: [(ModuleIdent, Source)] -> ([(ModuleIdent, Source)], [String])
flattenDeps = fdeps . sortDeps where

  sortDeps :: [(ModuleIdent, Source)] -> [[(ModuleIdent, Source)]]
  sortDeps = scc modules imports where
    -- extract the module ident
    modules (m, _) = [m]
    -- extract the imports
    imports (_, (_, (Prog _ imps _ _ _))) = imps

  fdeps :: [[(ModuleIdent, Source)]] -> ([(ModuleIdent, Source)], [String])
  fdeps = foldr checkdep ([], [])

  checkdep []          (ms', errs) = (ms'  , errs)
  checkdep [m]         (ms', errs) = (m:ms', errs)
  checkdep dep@(_:_:_) (ms', errs) = (ms'  , cyclicError (map fst dep) : errs)

  cyclicError :: [ModuleIdent] -> String
  cyclicError ms = "Cylic import dependency between modules " ++
                   intercalate ", " inits ++ " and " ++ last where
    (inits, last)      = splitLast ms
    splitLast []       = error "ModuleDeps.splitLast: empty list"
    splitLast (x:[])   = ([]  , x)
    splitLast (x:y:ys) = (x:xs, z) where (xs, z) = splitLast (y:ys)
