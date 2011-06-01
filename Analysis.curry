module Analysis (module Base, module Analysis) where

import FiniteMap
import FlatCurry
import FlatCurryGoodies
import Maybe (fromJust, fromMaybe)

import Base
import Dependency2
import Names
import ReadShowTerm (readQTerm, showQTerm)


type Map a = FM QName a

showMap :: Map a -> String
--showMap m = showQTerm $ fmToList m
showMap = showFM

readMap :: String -> Map a
--readMap m = listToFM (<) $ readQTerm m
readMap m = readFM (<) m

-- from AnalysisSolver
data Declaration = F FuncDecl|T TypeDecl

type AnalysisFunction  a = (FuncDecl,[QName])->Map a->(QName,a)
type AnalysisFunction3 a = (Declaration,[QName])->Map a->(QName,a)

getDeclName decl = case decl of
  F f->funcName f
  T t->typeName t

getFunctionCalls::[FuncDecl]->[(FuncDecl,[QName])]
getFunctionCalls [] = []
getFunctionCalls (func:funcs) = (func,(callsDirectly func)):(getFunctionCalls funcs)

getCalls::[Declaration]->[(Declaration,[QName])]
getCalls [] = []
getCalls (decl:decls) = (decl,(callHelp decl)):(getCalls decls)

callHelp decl = case decl of
  F f->callsDirectly f
  T t->callsDirectly2 t

fullIteration::(AnalysisFunction a)->[(FuncDecl,[QName])]->Map a->Map a->Map a
fullIteration analysis funcsWithCalls importsWithValues startMap =
  let afterMap = listToFM (<) $ (map (\func->(analysis func (startMap `plusFM` importsWithValues))) funcsWithCalls)
  in if (startMap `eqFM` afterMap)
         then startMap
         else fullIteration analysis funcsWithCalls importsWithValues afterMap

fullIteration3::(AnalysisFunction3 a)->[(Declaration,[QName])]->Map a->Map a->Map a
fullIteration3 analysis declsWithCalls importsWithValues startMap =
  let afterMap = listToFM (<) $ (map (\decl->(analysis decl (startMap `plusFM` importsWithValues))) declsWithCalls)
  in if (startMap `eqFM` afterMap)
         then startMap
         else fullIteration3 analysis declsWithCalls importsWithValues afterMap

-- ---------------------------------------------------------------------------
-- ND
-- ---------------------------------------------------------------------------
type NDResult = FM QName NDClass

initNDResult :: NDResult
initNDResult = listToFM (<) [(qmark, ND)]

analyseND :: Prog -> NDResult -> NDResult
analyseND p preRes =
  let funcs = progFuncs p
      start = listToFM (<) $ map (\f -> let name = funcName f in (name, if name == qmark then ND else D)) funcs
  in
  preRes `plusFM` fullIteration ndFunc (getFunctionCalls funcs) preRes start

ndFunc:: (FuncDecl, [QName]) -> Map NDClass -> (QName, NDClass)
ndFunc (f, called) ndmap
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
isNDExpr = trExpr cf cf combf letf freef orf casef branchf
  where
    cf x = const False x
    combf _ _ isNDs = or isNDs
    letf ndBinds ndExp = or $ ndExp : map snd ndBinds
    freef _ _ = True
    orf _ _ = True
    casef _ e bs = or (e:bs)
    branchf _ e = e

qmark :: QName
qmark = renameQName (prelude, "?")

-- ---------------------------------------------------------------------------
-- HO
-- ---------------------------------------------------------------------------
type HOResult = FM QName HOClass

initHOResult :: HOResult
initHOResult = emptyFM (<) -- listToFM (<) [(f, HO) | f <- externalHOFuncs]

analyseHOFunc :: Prog -> HOResult -> HOResult
analyseHOFunc p preRes =
  let funcs = map F $ progFuncs p
      types = map T $ progTypes p
      decls = funcs ++ types
      start = listToFM (<) $ map (\d ->(getDeclName d, FO)) decls
  in
  preRes `plusFM` fullIteration3 ordFunc (getCalls decls) preRes start

analyseHOCons :: Prog -> HOResult -> HOResult
analyseHOCons p preRes =
  let constructors = concatMap typeConsDecls $ filter (not . isTypeSyn) $ (progTypes p)
      result = map consOrder constructors
  in
  preRes `plusFM` listToFM (<) result


consOrder (Cons name _ _ texps) = (name, consOrder' texps)
  where
    consOrder' [] = FO
    consOrder' (typeExpr:typeExprs) = case typeExpr of
      FuncType _ _ -> HO
      TCons _ typeExprs2 -> consOrder' (typeExprs2 ++ typeExprs)
      TVar _ -> consOrder' typeExprs

externalHOFuncs :: [QName]
externalHOFuncs = map renameQName $ zip (repeat prelude)
  [">>=", "apply", "catch", "try", "$!", "$!!", "$##"]

prelude :: String
prelude = "Prelude"


hoOr::HOClass->HOClass->HOClass
hoOr HO _ = HO
hoOr FO x = x

ordFunc::(Declaration,[QName])-> Map HOClass -> (QName,HOClass)
ordFunc (T (Type qName _ _ conDecls),_) orderMap = (qName,(goThroughConsList orderMap conDecls))
ordFunc (T (TypeSyn qName _ _ typeExpr),_) orderMap = (qName,(ordHelp1 orderMap typeExpr))
ordFunc ((F func),_)  orderMap =
    (funcName func,(ordHelp2 (funcType func) (funcArity func) orderMap))

goThroughConsList _ [] = FO
goThroughConsList orderMap (conDecl:conDecls) = let (Cons _ _ _ typeExprs)= conDecl in
  hoOr (foldr (\typeExpr order->hoOr order (ordHelp1 orderMap typeExpr)) FO typeExprs)
        (goThroughConsList orderMap conDecls)

ordHelp1 _ (TVar _) = FO
ordHelp1 _ (FuncType _ _) = HO
ordHelp1 orderMap (TCons qName typeExprs) = hoOr (fromMaybe FO (lookupFM orderMap qName))
  (foldr (\typeExpr order->hoOr order (ordHelp1 orderMap typeExpr)) FO typeExprs)

ordHelp2 functype arity orderMap =
  if (arity==0)
    then case functype of
         FuncType _ _ -> HO
         TVar (-42) -> HO
         TCons x (y:ys)->
           let cons1=(ordHelp2 y 0 orderMap)
               consRest=(ordHelp2 (TCons x ys) 0 orderMap)
           in (hoOr cons1 consRest)
         TCons (modName,consName) []-> fromMaybe FO (lookupFM orderMap (modName,consName))
         _ -> FO
    else case functype of
         TVar (-42) -> HO
         FuncType x y->
             let func1 = (ordHelp2 x 0 orderMap)
                 funcRest = (ordHelp2 y (arity-1) orderMap)
             in (hoOr func1 funcRest)
