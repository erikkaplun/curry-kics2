{-  Computation of dependendies between Curry modules

    This module implements the functions to compute the dependency
    information between Curry modules.

    Copyright (c) 2002-2004, Wolfgang Lux
    See LICENSE for the full license.

    Modified by Martin Engelke (men@informatik.uni-kiel.de)
    Extended by Sebastian Fischer (sebf@informatik.uni-kiel.de)
-}

module ModuleDeps (ModuleIdent, Source, deps) where

import FileGoodies
import FiniteMap (FM, emptyFM, addToFM, fmToList, lookupFM)
import FlatCurry (readFlatCurry, Prog (..))
import Utils (foldIO, intercalate)

import Files
import SCC

type ModuleIdent = String
type Source = (String, Prog) -- filename, code
type SourceEnv = FM ModuleIdent Source

deps :: [String] -> String -> IO ([(ModuleIdent, Source)], [String])
deps importPaths fn = do
  dps <- sourceDeps ("." : importPaths) ident fn (emptyFM (<))
  return $ flattenDeps dps
    where ident = stripSuffix $ baseName fn

sourceDeps :: [String] -> ModuleIdent -> String -> SourceEnv -> IO SourceEnv
sourceDeps importPaths m fn mEnv = do
  fcy@(Prog _ imps _ _ _) <- readFlatCurry (stripSuffix fn)
  foldIO (moduleDeps importPaths) (addToFM mEnv m (fn, fcy)) imps

moduleDeps :: [String] -> SourceEnv -> ModuleIdent -> IO SourceEnv
moduleDeps importPaths mEnv m = case lookupFM mEnv m of
  Just _  -> return mEnv
  Nothing -> do
    mbFile <- lookupModule importPaths m
    case mbFile of
      -- TODO: Could be improved by inserting it into the error messages
      Nothing -> error $ unlines $ ("Module " ++ m ++ " could not be found in:") : importPaths
      Just fn -> sourceDeps importPaths m fn mEnv

lookupModule :: [String] -> String -> IO (Maybe String)
lookupModule importPaths mod = lookupFileInPath mod [".curry", ".lcurry"]
                               (map dropTrailingPathSeparator importPaths)

{-  Convert the dependency map into a topologically sorted dependency list
    and a list of errors.
-}
flattenDeps :: SourceEnv -> ([(ModuleIdent, Source)], [String])
flattenDeps = fdeps . sortDeps where

  sortDeps :: SourceEnv -> [[(ModuleIdent, Source)]]
  sortDeps = scc modules imports . fmToList

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
    splitLast (x:[])   = ([]  , x)
    splitLast (x:y:ys) = (x:xs, z) where (xs, z) = splitLast (y:ys)
