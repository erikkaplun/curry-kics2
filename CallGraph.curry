module CallGraph (NDClass (..), analyseNd, NDResult) where

import Char (isDigit)
import Maybe (fromJust)
import List (findIndex, nub)
import FiniteMap
import FlatCurry
import FlatCurryGoodies

import GraphAlgorithms
import GraphInductive

data NDClass
  = ND  -- non-deterministic function
  | DHO -- deterministic higher-order function
  | DFO -- deterministic first-order function

type NDResult = FM QName NDClass

data QN = QN QName

showQN :: QN -> String
showQN (QN (q,m)) = q ++ "." ++ m

analyseNd :: Prog -> NDResult
analyseNd p = listToFM (<) (map (\ f -> (f, ndClass f)) funs)
  where
   (funs,graph) = callGraph p
   qmarkNode    = find qmark funs
   applyNode    = find apply funs
   ndClass f    | elem qmarkNode deps = ND
--                | take 3 (snd f) == "set" && (isDigit $ snd f !! 3) = ND -- TODO hack?
                | elem applyNode deps = DHO
                | otherwise           = DFO
     where
       deps = reachable [find f funs] graph

find :: a -> [a] -> Int
find x xs = fromJust (findIndex (x==) xs)

callGraph :: Prog -> ([QName],Graph QN ())
callGraph p = let calls = map funs2graph (progFuncs p)
                  funs  = nub (QN qmark:QN apply:map fst calls ++ concatMap snd calls)
               in (map (\ (QN x) -> x ) funs,
                   mkGraph (zip [0..] funs)
                           (concatMap (toEdges funs) calls))

toEdges :: [QN] -> (QN,[QN]) -> [(Int,Int,())]
toEdges funs (f,fs) = map (\ f' -> (i,find f' funs,())) fs
  where i = find f funs

funs2graph :: FuncDecl -> (QN,[QN])
funs2graph f = (QN (funcName f),nub called)
  where
    called = if isRuleExternal rule
      then []
      else trExpr var lit comb leT freE oR casE branch (ruleBody rule)
    rule = funcRule f

    var _ = []
    lit _ = []

    comb FuncCall qn es         = QN qn : concat es
    comb (FuncPartCall _) qn es = QN qn : concat es
    comb ConsCall _ es          = concat es
    comb (ConsPartCall _) _  es = concat es

    leT xs e = concatMap snd xs ++ e
    freE _ x = QN qmark : x
    oR es1 es2 = QN qmark : es1 ++ es2
    casE _ e bs = e ++ concat bs
    branch _ e = e

prelude = "Prelude"
qmark = (prelude, "?")
apply = (prelude, "apply")
meta  = "Cover"
cover = (meta, "cover")
