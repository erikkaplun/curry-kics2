module Analysis (module Base, module Analysis) where

import FiniteMap
import FlatCurry
import FlatCurryGoodies
import Maybe (fromJust, fromMaybe)
import List (partition)

import Base
import Dependency2
import Names

type Map a = FM QName a

-- from AnalysisSolver
data Declaration
  = F FuncDecl
  | T TypeDecl

type AnalysisFunction t a = Map a -> (t, [QName]) -> (QName, a)

getDeclName :: Declaration -> QName
getDeclName decl = case decl of
  F f -> funcName f
  T t -> typeName t

getFunctionCalls :: [FuncDecl] -> [(FuncDecl, [QName])]
getFunctionCalls fs = map (\ func -> (func, callsDirectly func)) fs

getCalls :: [Declaration] -> [(Declaration, [QName])]
getCalls ds = map (\ decl -> (decl, callHelp decl)) ds
  where
    callHelp decl = case decl of
      F f -> callsDirectly f
      T t -> callsDirectly2 t

fullIteration :: AnalysisFunction t a -> [(t, [QName])] -> Map a -> Map a
              -> Map a
fullIteration analyze calls env start =
  let after = listToFM (<) $ map (analyze (env `plusFM` start)) calls
  in if (start `eqFM` after)
         then start
         else fullIteration analyze calls env after

-- ---------------------------------------------------------------------------
-- (Non)Determinism analysis
-- ---------------------------------------------------------------------------
type NDResult = Map NDClass

initNDResult :: NDResult
initNDResult = listToFM (<) [(qmark, ND)]

analyseND :: Prog -> NDResult -> NDResult
analyseND p preRes =
  let funcs = progFuncs p
      start = listToFM (<) $ map initValue funcs
  in  fullIteration ndFunc (getFunctionCalls funcs) preRes start
  where initValue f = let name = funcName f
                      in (name, if name == qmark then ND else D)

ndFunc:: AnalysisFunction FuncDecl NDClass
ndFunc ndmap (f, called)
  | isRuleExternal rule      = default
  | isNDExpr (ruleBody rule) = (name, ND)
  | callsND                  = (name, ND)
  | otherwise                = default
  where
    name    = funcName f
    rule    = funcRule f
    callsND = any (== Just ND) $ map (lookupFM ndmap) called
    default = (name, fromJust $ lookupFM ndmap name)

-- Check whether an expression is non-deterministic, i.e. whether it uses
-- an OR or FREE
isNDExpr :: Expr -> Bool
isNDExpr = trExpr cf cf combf letf freef orf casef branchf typedf
  where
    cf x = const False x
    combf _ _ isNDs = or isNDs
    letf ndBinds ndExp = or $ ndExp : map snd ndBinds
    freef _ _ = True
    orf _ _ = True
    casef _ e bs = or (e:bs)
    branchf _ e = e
    typedf e _ = e

qmark :: QName
qmark = renameQName (prelude, "?")

-- ---------------------------------------------------------------------------
-- (first/higher)-order analysis
-- ---------------------------------------------------------------------------
type HOResult = FM QName HOClass

initHOResult :: HOResult
initHOResult = emptyFM (<) -- listToFM (<) [(f, HO) | f <- externalHOFuncs]

--externalHOFuncs :: [QName]
--externalHOFuncs = map renameQName $ zip (repeat prelude)
--  [">>=", "apply", "catch", "try", "$!", "$!!", "$##"]

analyseHOFunc :: Prog -> HOResult -> HOResult
analyseHOFunc p preRes =
  let funcs = map F $ progFuncs p
      types = map T $ progTypes p
      decls = funcs ++ types
      start = listToFM (<) $ map (\d ->(getDeclName d, FO)) decls
  in  fullIteration ordFunc (getCalls decls) preRes start

analyseHOCons :: Prog -> HOResult
analyseHOCons p = listToFM (<)
                $ ((("Curry_Prelude","C_Success"), FO) :)
                $ map consOrder
                $ concatMap typeConsDecls
                $ filter (not . isTypeSyn)
                $ progTypes p

consOrder (Cons name _ _ texps) = (name, consOrder' texps)
  where
    consOrder' [] = FO
    consOrder' (typeExpr:typeExprs) = case typeExpr of
      FuncType _ _ -> HO
      TCons _ typeExprs2 -> consOrder' (typeExprs2 ++ typeExprs)
      TVar _ -> consOrder' typeExprs

hoOr::HOClass->HOClass->HOClass
hoOr HO _ = HO
hoOr FO x = x

ordFunc :: AnalysisFunction Declaration HOClass
ordFunc orderMap (T (Type    qName _ _ conDecls),_)
  = (qName,(goThroughConsList orderMap conDecls))
ordFunc orderMap (T (TypeSyn qName _ _ typeExpr),_)
  = (qName,(ordHelp1 orderMap typeExpr))
ordFunc orderMap (F func                        ,_)
  = (funcName func,(ordHelp2 (funcType func) (funcArity func) orderMap))

goThroughConsList _        [] = FO
goThroughConsList orderMap (Cons _ _ _ tys : conDecls)
  = hoOr (foldr (\ ty order -> hoOr order (ordHelp1 orderMap ty)) FO tys)
         (goThroughConsList orderMap conDecls)

ordHelp1 _        (TVar          _) = FO
ordHelp1 _        (FuncType    _ _) = HO
ordHelp1 orderMap (TCons qName tys)
  = hoOr (fromMaybe FO (lookupFM orderMap qName))
         (foldr (\ty order -> hoOr order (ordHelp1 orderMap ty)) FO tys)

ordHelp2 functype arity orderMap = if arity == 0
  then case functype of
        FuncType  _ _ -> HO
        TCons x (y:ys)->
          let cons1=(ordHelp2 y 0 orderMap)
              consRest=(ordHelp2 (TCons x ys) 0 orderMap)
          in (hoOr cons1 consRest)
        TCons (modName,consName) [] ->
          fromMaybe FO (lookupFM orderMap (modName,consName))
        _ -> FO
  else case functype of
        FuncType x y ->
            let func1 = (ordHelp2 x 0 orderMap)
                funcRest = (ordHelp2 y (arity-1) orderMap)
            in (hoOr func1 funcRest)

--------------------------------------------------------------------------------
-- Visibility analysis
--------------------------------------------------------------------------------

data Visibilities = Vis ([QName],[QName]) ([QName],[QName]) ([QName],[QName])

getPrivateFunc (Vis (_  ,priv) _                   _) = priv
getPublicFunc  (Vis (pub,   _) _                   _) = pub

getPrivateType (Vis _          (_  ,priv)          _) = priv
getPublicType  (Vis _          (pub,   _)          _) = pub

getPrivateCons (Vis _          _          (_  ,priv)) = priv
getPublicCons  (Vis _          _          (pub,   _)) = pub

analyzeVisibility :: Prog -> Visibilities
analyzeVisibility p =
  Vis (splitVisibleFuncs (progFuncs p))
      (splitVisibleTypes types)
      (splitVisibleCons  (concatMap typeConsDecls
                                    (filter (not . isTypeSyn) types)))
 where
  types = progTypes p

splitVisibleFuncs funcs =
   let (pubs,privs) =  partition (\f -> funcVisibility f == Public) funcs in
   (map funcName pubs, map funcName privs)

splitVisibleTypes types =
   let (pubs,privs) = partition (\t -> typeVisibility t == Public) types in
    (map typeName pubs, map typeName privs)

splitVisibleCons cons =
  let (pubs,privs) = partition (\c -> consVisibility c == Public) cons in
   (map consName pubs, map consName privs)

(|++|) :: ([a], [b]) -> ([a], [b]) -> ([a], [b])
(xs1, xs2) |++| (ys1, ys2) = (xs1 ++ ys1 , xs2 ++ ys2)
