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
    cf               x = const False x
    combf    _ _ isNDs = or isNDs
    letf ndBinds ndExp = or (ndExp : map snd ndBinds)
    freef          _ _ = True
    orf            _ _ = True
    casef       _ e bs = or (e : bs)
    branchf        _ e = e
    typedf         e _ = e

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
      start = listToFM (<) $ map (\d -> (getDeclName d, FO)) decls
  in  fullIteration ordFunc (getCalls decls) preRes start

analyseHOCons :: Prog -> HOResult
analyseHOCons p = listToFM (<)
                $ ((("Curry_Prelude","C_Success"), FO) :)
                $ map consOrder
                $ concatMap typeConsDecls
                $ filter (not . isTypeSyn)
                $ progTypes p

consOrder (Cons name _ _ texps) = (name, foldr hoOr FO (map consHOType texps))
  where
    consHOType (TVar       _) = FO
    consHOType (FuncType _ _) = HO
    consHOType (TCons  _ tys) = foldr hoOr FO (map consHOType tys)

hoOr :: HOClass -> HOClass -> HOClass
hoOr HO _ = HO
hoOr FO x = x
hoOr (HORes _) HO = HO
hoOr x@(HORes _) FO = x
hoOr (HORes _) (HORes _) = error "Analysis.hoOr"

ordFunc :: AnalysisFunction Declaration HOClass
ordFunc orderMap (T (Type    qn _ _ conDecls),_)
  = (qn,(goThroughConsList orderMap conDecls))
ordFunc orderMap (T (TypeSyn qn _ _ ty),_)
  = (qn,(ordHelp1 orderMap ty))
ordFunc orderMap (F func                        ,_)
  = (funcName func, isHOFunc orderMap (funcArity func) (funcType func))

goThroughConsList _        [] = FO
goThroughConsList orderMap (Cons _ _ _ tys : conDecls)
  = hoOr (foldr (\ ty order -> hoOr order (ordHelp1 orderMap ty)) FO tys)
         (goThroughConsList orderMap conDecls)

ordHelp1 _        (TVar       _) = FO
ordHelp1 _        (FuncType _ _) = HO
ordHelp1 orderMap (TCons qn tys)
  = hoOr (fromMaybe FO (lookupFM orderMap qn))
         (foldr (\ty order -> hoOr order (ordHelp1 orderMap ty)) FO tys)


-- Determines, if a Function is a higher order function.
-- In our context, a function with arity n is higher order (HO),
-- if one of its arguments has a higher order type. 
-- If the result is a m-ary function type and neither
-- the argument types nor the result type is a higher order type,
-- then the function has a higher order result with arity m (HORes m)
-- Otherwise, it is first order (FO)
isHOFunc orderMap arity ty
  | arity == 0 = foldr (\ty order -> hoOr order (isHOType orderMap ty))
                       (if numArgs > 0 then HORes numArgs  else FO)
                       types  --isHOType orderMap ty  -- FO 
  | otherwise  = case ty of
      FuncType x y -> isHOType orderMap x `hoOr` isHOFunc orderMap (arity - 1) y
      _            -> error "Analysis.isHOFunc"
 where types = splitFuncType ty
       numArgs = length types - 1

-- Determines, if a type expression involves a function type (->)
-- or a type that has a constructor which involves a function type. 
isHOType orderMap ty = case ty of
  TVar _          -> FO
  FuncType   _ _  -> HO
  TCons    qn tys -> foldr hoOr (fromMaybe FO (lookupFM orderMap qn))
                                (map (isHOType orderMap) tys)

-- splits a function Type in the type-expressions of the arguments and the result
splitFuncType :: TypeExpr -> [TypeExpr]
splitFuncType t@(TVar        _) = [t]
splitFuncType (FuncType at rt) = at : splitFuncType rt
splitFuncType t@(TCons     _ _) = [t]

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
