--- --------------------------------------------------------------------------
--- CallGraph analysis

--- This module analyzes the call graph of the defined functions to classify
--- them respective to their (non)determinism.

--- @author Bernd Brassel, Bjoern Peemoeller, Fabian Reck
--- @version March 2011
--- --------------------------------------------------------------------------
module CallGraph (NDClass (..), initNDResult, analyseNd, NDResult) where

import Char (isDigit)
import Maybe (fromJust)
import List (findIndex, nub)
import FiniteMap
import FlatCurry
import FlatCurryGoodies
import GraphAlgorithms
import GraphInductive

import Base
import Names

type NDResult = FM QName NDClass

data QN = QN QName

showQN :: QN -> String
showQN (QN (q,m)) = q ++ "." ++ m

initNDResult :: NDResult
initNDResult = listToFM (<) [(qmark, ND), (apply, DHO)]

analyseNd :: Prog -> NDResult -> NDResult
analyseNd p nds = plusFM nds (listToFM (<) (map (\ f -> (f, ndClass f)) funs))
  where
  (funs, graph) = callGraph p
--   qmarkNode    = find qmark funs
--   applyNode    = find apply funs
  ndClass f
      -- function depending on (?) are non-deterministic
    | isNonDeterministic    = ND
      -- set functions are non-deterministic
    | isSetFunction (snd f) = ND
      -- TODO
    | isHigherOrder         = DHO
    | otherwise             = DFO
      where
        isSetFunction g    = take 4 g `elem` map (\n -> "set" ++ show n) [0 .. 9]
        isNonDeterministic = any (hasNDClass  ND) deps
        isHigherOrder      = any (hasNDClass DHO) deps
        deps               = reachable [find f funs] graph
        hasNDClass cl node = let (QN n) = fromJust (lab graph node)
                             in lookupFM nds n == Just cl

find :: a -> [a] -> Int
find x xs = fromJust (findIndex (x==) xs)

-- create a call graph from a program
callGraph :: Prog -> ([QName], Graph QN ())
callGraph p =
  let calls = map funs2graph (progFuncs p)
      funs  = nub (QN qmark : QN apply : map fst calls ++ concatMap snd calls)
  in  ( map (\ (QN x) -> x ) funs
      , mkGraph (zip [0..] funs) (concatMap (toEdges funs) calls)
      )

toEdges :: [QN] -> (QN, [QN]) -> [(Int, Int, ())]
toEdges funs (f, fs) = map (\ f' -> (i, find f' funs, ())) fs
  where i = find f funs

-- Create a tuple of the function name and a lisf of the called functions
funs2graph :: FuncDecl -> (QN, [QN])
funs2graph f = (QN (funcName f), nub called)
  where
    called
      | isRuleExternal rule = if funcName f `elem` externalHOFuncs
                              then [QN apply] else []
      | otherwise = trExpr var lit comb leT freE oR casE branch (ruleBody rule)
    rule = funcRule f

    var _ = []
    lit _ = []

    comb FuncCall qn es         = QN qn : concat es
    comb (FuncPartCall _) qn es = QN qn : concat es
    comb ConsCall _ es          = concat es
    comb (ConsPartCall _) _  es = concat es

    leT xs e    = concatMap snd xs ++ e
    freE _ x    = QN qmark : x
    oR es1 es2  = QN qmark : es1 ++ es2
    casE _ e bs = e ++ concat bs
    branch _ e  = e

externalHOFuncs :: [QName]
externalHOFuncs = map renameQName $ zip (repeat prelude)
  [">>=", "apply", "catch", "try"]

qmark :: QName
qmark = renameQName (prelude, "?")

apply :: QName
apply = renameQName (prelude, "apply")

prelude :: String
prelude = "Prelude"