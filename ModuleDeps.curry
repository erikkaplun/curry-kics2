{-  Computation of dependendies between Curry modules

    This module implements the functions to compute the dependency
    information between Curry modules.

    Copyright (c) 2002-2004, Wolfgang Lux
    See LICENSE for the full license.

    Modified by Martin Engelke (men@informatik.uni-kiel.de)
    Extended by Sebastian Fischer (sebf@informatik.uni-kiel.de)
-}

module ModuleDeps ( deps, flattenDeps, sourceDeps, moduleDeps ) where

import FileGoodies
import FiniteMap (FM, emptyFM, addToFM, fmToList, lookupFM)
import FlatCurry (readFlatCurry, Prog (..))

import SCC

type ModuleIdent = String
type SourceEnv = FM ModuleIdent Prog

deps :: String -> IO ([(ModuleIdent, Prog)], [String])
deps fn = do
  dps <- sourceDeps ident (emptyFM (<))
  return $ flattenDeps dps
    where ident = stripSuffix $ baseName fn

sourceDeps :: ModuleIdent -> SourceEnv -> IO SourceEnv
sourceDeps m mEnv = do
  fn <- lookupModule m
  fcy@(Prog _ imps _ _ _) <- readFlatCurry fn
  foldIO moduleDeps (addToFM mEnv m fcy) imps

lookupModule :: String -> IO String
lookupModule mod = return $ mod ++ ".curry"

moduleDeps :: SourceEnv -> ModuleIdent -> IO SourceEnv
moduleDeps mEnv m = case lookupFM mEnv m of
  Just _  -> return mEnv
  Nothing -> sourceDeps m mEnv

{-  Convert the dependency map into a topologically sorted dependency list
    and a list of errors.
-}
flattenDeps :: SourceEnv -> ([(ModuleIdent, Prog)], [String])
flattenDeps = fdeps . sortDeps where
  sortDeps :: SourceEnv -> [[(ModuleIdent, Prog)]]
  sortDeps = scc modules imports . fmToList

  -- extract the module ident
  modules (m, _) = [m]

  -- extract the imports
  imports (_, (Prog _ imps _ _ _)) = imps

  fdeps :: [[(ModuleIdent, Prog)]] -> ([(ModuleIdent, Prog)], [String])
  fdeps = foldr checkdep ([], [])

  checkdep []          (ms', errs) = (ms'  , errs)
  checkdep [m]         (ms', errs) = (m:ms', errs)
  checkdep dep@(_:_:_) (ms', errs) = (ms'  , cyclicError (map fst dep) : errs)

  cyclicError :: [ModuleIdent] -> String
  cyclicError []     = ""
  cyclicError (m:ms) =
    "Cylic import dependency between modules " ++ show m ++ rest ms

  rest [m] = " and " ++ show m
  rest ms@(_:_:_)  = rest' ms
  rest' [] = ""
  rest' [m] = ", and " ++ show m
  rest' (m:m2:ms) = ", " ++ show m ++ rest' (m2:ms)

foldIO :: (a -> b -> IO a) -> a -> [b] -> IO a
foldIO _ a []      =  return a
foldIO f a (x:xs)  =  f a x >>= \fax -> foldIO f fax xs
