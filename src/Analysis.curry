--- ----------------------------------------------------------------------------
--- This module contains the analysis of types, constructors and functions
---- for (non)determinism and first order/higher order.
---
--- @author  Michael Hanus, Björn Peemöller, Fabian Skrlac
--- @version May 2014
--- ----------------------------------------------------------------------------
module Analysis
  ( AnalysisResult, showAnalysisResult, readAnalysisResult
  , TypeMap, initTypeMap, getTypeMap
  , NDResult, NDClass (..), initNDResult, analyseND
  , initHOResult
  , TypeHOResult, TypeHOClass (..), initTypeHOResult, analyseHOType
  , ConsHOResult, ConsHOClass (..), analyseHOCons
  , FuncHOResult, FuncHOClass (..), analyseHOFunc
  , Visibilities, analyzeVisibility
  , getPrivateType, getPrivateFunc, getPrivateCons
  ) where

import FiniteMap
import FlatCurry
import FlatCurryGoodies
import Maybe            (fromJust, fromMaybe)
import List             (partition)
import SetRBT

import Classification
import Names

type AnalysisResult = (TypeMap, NDResult, TypeHOResult, ConsHOResult, FuncHOResult)

showAnalysisResult :: AnalysisResult -> (String, String, String, String, String)
showAnalysisResult (types, ndAna, hoType, hoCons, hoFunc)
  = (showFM types, showFM ndAna, showFM hoType, showFM hoCons, showFM hoFunc)

readAnalysisResult :: (String, String, String, String, String) -> AnalysisResult
readAnalysisResult (types, ndAna, hoType, hoCons, hoFunc)
  = ( readFM (<) types , readFM (<) ndAna , readFM (<) hoType
    , readFM (<) hoCons, readFM (<) hoFunc)

type Map a = FM QName a

-- -----------------------------------------------------------------------------
-- Mapping from constructor names to the defining types
-- -----------------------------------------------------------------------------

-- The type map is used to lookup the type name for a given constructor
-- name to be able to add missing pattern matching alternatives like
-- Choice_<TypeName> etc.
-- This could also be done by inspecting the type signature of the respective
-- function, but it may not be accurate for various reasons.

type TypeMap = Map QName

initTypeMap :: TypeMap
initTypeMap = listToFM (<) primTypes

--- List of constructors of known primitive types.
primTypes :: [(QName, QName)]
primTypes = map (\ (x, y) -> ( renameQName (prelude, x)
                             , renameQName (prelude, y))) $
  [ ("Success","Success"), ("True", "Bool"), ("False", "Bool")
  , ("Int", "Int")  , ("Float", "Float"), ("Char", "Char")
  ]

--- Register the types names of constructors to be able to retrieve
--- the types for constructors used in pattern matching.
--- May be needless now because the case lifting now also creates correct types.
getTypeMap :: [TypeDecl] -> TypeMap
getTypeMap ts = listToFM (<)
              $ concatMap (\(Type qn _ _ cs) -> map (\c -> (consName c, qn)) cs)
              $ filter (not . isTypeSyn) ts

-- -----------------------------------------------------------------------------
-- Analysis using fix-point iteration
-- -----------------------------------------------------------------------------

type Analysis t a = Map a -> (t, [QName]) -> (QName, a)

fullIteration :: Analysis t a -> [(t, [QName])] -> Map a -> Map a -> Map a
fullIteration analyze calls env start =
  let after = listToFM (<) $ map (analyze (env `plusFM` start)) calls
  in if (start `eqFM` after)
         then start
         else fullIteration analyze calls env after

-- -----------------------------------------------------------------------------
-- (Non-)Determinism analysis for functions.
-- (Note that Type constructors and data constructors are deterministic.)
-- -----------------------------------------------------------------------------

type NDResult = Map NDClass

--- Initial start value for the non-determinism analysis.
initNDResult :: NDResult
initNDResult = listToFM (<) [(qmark, ND)]

--- Analyse a module for non-determinism using the information
--- for imported modules analysed before.
analyseND :: Prog -> NDResult -> NDResult
analyseND p importedInfo =
  let fs = progFuncs p
      start = listToFM (<) $ map initValue fs
  in  fullIteration ndFunc (map getFunctionCalls fs) importedInfo start
  where
    initValue f = let name = funcName f
                  in (name, if name == qmark then ND else D)
    getFunctionCalls f = (f, funcCalls f)

--- Analysis function for non-determinism analysis.
ndFunc:: Analysis FuncDecl NDClass
ndFunc ndInfo (f, called)
  | isRuleExternal rule      = default
  | isNDExpr (ruleBody rule) = (name, ND)
  | callsND                  = (name, ND)
  | otherwise                = default
  where
    name    = funcName f
    rule    = funcRule f
    callsND = any (== Just ND) $ map (lookupFM ndInfo) called
    default = (name, fromJust $ lookupFM ndInfo name)

--- Check whether an expression is non-deterministic, i.e.,
--- whether it uses an OR (overlapping rule) or FREE (free variables).
isNDExpr :: Expr -> Bool
isNDExpr = trExpr cf cf combf letf freef orf casef branchf typedf
  where
    cf               x = const False x -- (variable / literal)
    combf    _ _ isNDs = or isNDs
    letf ndBinds ndExp = or (ndExp : map snd ndBinds)
    freef         vs _ = not (null vs)
    orf            _ _ = True
    casef       _ e bs = or (e : bs)
    branchf        _ e = e
    typedf         e _ = e

--- list of direct dependencies for a function
funcCalls :: FuncDecl -> [QName]
funcCalls (Func _ _ _ _ (Rule   _ e)) = toList $ funcsInExp e
funcCalls (Func _ _ _ _ (External _)) = []

--- Gets the set of all functions (including partially applied functions)
--- directly called in an expression.
funcsInExp :: Expr -> SetRBT QName
funcsInExp (Var        _) = empty
funcsInExp (Lit        _) = empty
funcsInExp (Comb ct f es)
  | isFuncCall ct = f `insertRBT` unionMap funcsInExp es
  | otherwise     =               unionMap funcsInExp es
funcsInExp (Free     _ e) = funcsInExp e
funcsInExp (Let     bs e) = unionMap funcsInExp (e : map snd bs)
funcsInExp (Or     e1 e2) = funcsInExp e1 `unionRBT` funcsInExp e2
funcsInExp (Case  _ e bs) = unionMap funcsInExp (e : map branchExpr bs)
funcsInExp (Typed    e _) = funcsInExp e

--- Is a combination a function call?
isFuncCall :: CombType -> Bool
isFuncCall ct = case ct of
  FuncCall       -> True
  FuncPartCall _ -> True
  _              -> False

-- -----------------------------------------------------------------------------
-- (first/higher)-order analysis of types and type constructors
-- -----------------------------------------------------------------------------

type TypeHOResult = Map TypeHOClass

initTypeHOResult :: TypeHOResult
initTypeHOResult = listToFM (<) externalTypes

externalTypes :: [(QName, TypeHOClass)]
externalTypes = [(ioType, TypeIO), (successType, TypeFO)]

getHOResult :: QName -> TypeHOResult -> TypeHOClass
getHOResult qn hoResult = fromMaybe TypeFO (lookupFM hoResult qn)

--- Analyse a module for the higher-order classification of types and type
--- constructors using the information for imported modules analysed before.
analyseHOType :: Prog -> TypeHOResult -> TypeHOResult
analyseHOType p importedInfo =
  let types = progTypes p
      start = listToFM (<) $ map initValue types
  in  fullIteration hoType (map getUsedTypes types) importedInfo start
  where
    initValue    t = (typeName t, classifyHOTypeDecl t)
    getUsedTypes t = (t, usedTypes t)

hoType :: Analysis TypeDecl TypeHOClass
hoType typeInfo (t, deps) = (typeName t, maximumTypeHOClass depClasses)
  where depClasses = map (`getHOResult` typeInfo) (typeName t : deps)

classifyHOTypeDecl :: TypeDecl -> TypeHOClass
classifyHOTypeDecl (Type   qn _ _ cs)
  | qn == ioType = TypeIO
  | otherwise    = maximumTypeHOClass
                 $ map classifyHOType
                 $ concatMap consArgs cs
classifyHOTypeDecl (TypeSyn _ _ _ ty) = classifyHOType ty

classifyHOType :: TypeExpr -> TypeHOClass
classifyHOType (TVar       _) = TypeFO
classifyHOType (FuncType _ _) = TypeHO
classifyHOType (TCons qn tys)
  | qn == ioType = maximumTypeHOClass (TypeIO : map classifyHOType tys)
  | otherwise    = maximumTypeHOClass (map classifyHOType tys)

usedTypes :: TypeDecl -> [QName]
usedTypes (Type    _ _ _ cs) = toList $ unionMap  typeCons
                                      $ concatMap consArgs cs
usedTypes (TypeSyn _ _ _ ty) = toList $ typeCons  ty

typeCons :: TypeExpr -> SetRBT QName
typeCons (TVar       _) = empty
typeCons (FuncType a b) = typeCons a `unionRBT` typeCons b
typeCons (TCons qn tys) = qn `insertRBT` unionMap typeCons tys

-- -----------------------------------------------------------------------------
-- (first/higher)-order analysis of data constructors
-- -----------------------------------------------------------------------------

type ConsHOResult = Map ConsHOClass

initHOResult :: Map a
initHOResult = emptyFM (<)

analyseHOCons :: Prog -> ConsHOResult
analyseHOCons p = listToFM (<) $ externals ++ internals
  where
  externals = filter ((== progName p) . fst . fst) externalCons
  internals = map consOrder $ concatMap typeConsDecls
            $ filter (not . isTypeSyn) -- filter isDataDecl
            $ progTypes p

externalCons :: [(QName, ConsHOClass)]
externalCons = [(successType, ConsFO)]

consOrder :: ConsDecl -> (QName, ConsHOClass)
consOrder (Cons qn _ _ tys) = (qn, class)
  where class = typeToConsHOClass $ maximumTypeHOClass (map classifyHOType tys)

-- -----------------------------------------------------------------------------
-- (first/higher)-order analysis of functions
-- -----------------------------------------------------------------------------

type FuncHOResult = Map FuncHOClass

analyseHOFunc :: Prog -> TypeHOResult -> FuncHOResult
analyseHOFunc p typeInfo = listToFM (<) $ map analyse (progFuncs p)
  where analyse f = (funcName f, isHOFunc typeInfo (funcArity f) (funcType f))

-- Determines if a function is higher order.
-- In our context, a function with arity n is higher order (HO),
-- if one of its arguments has a higher order type.
-- If the result is a m-ary function type and neither
-- the argument types nor the result type is a higher order type,
-- then the function has a higher order result with arity m (HORes m)
-- Otherwise, it is first order (FO)
isHOFunc :: TypeHOResult -> Int -> TypeExpr -> FuncHOClass
isHOFunc typeInfo arity ty
  | arity == 0 = case reverse (splitFuncType ty) of
      []        -> error "Analysis.isHOFunc: no type"
      (lty:tys) -> maximumFuncHOClass (initVal : map (isHOType True typeInfo) tys)
        where initVal = maxFuncHOClass
                          (if null tys then FuncFO else FuncHORes (length tys))
                          (isHOType False typeInfo lty)
  | otherwise  = case ty of
      FuncType x y -> maxFuncHOClass (isHOType True typeInfo x)
                                     (isHOFunc typeInfo (arity - 1) y)
      _            -> error "Analysis.isHOFunc"

--- Determines if a type expression involves a function type (->)
--- or a type that has a constructor which involves a function type.
isHOType :: Bool -> TypeHOResult -> TypeExpr -> FuncHOClass
isHOType ioAsHo typeInfo ty = case ty of
  TVar _          -> FuncFO
  FuncType   _ _  -> FuncHO
  TCons    qn tys -> maximumFuncHOClass $
                        typeToFuncHOClass ioAsHo (getHOResult qn typeInfo) :
                          map (isHOType ioAsHo typeInfo) tys

--- splits a function type into the type expressions
--- of the arguments and the result.
splitFuncType :: TypeExpr -> [TypeExpr]
splitFuncType t@(TVar        _) = [t]
splitFuncType (FuncType  at rt) = at : splitFuncType rt
splitFuncType t@(TCons     _ _) = [t]

-- -----------------------------------------------------------------------------
-- Visibility analysis
-- -----------------------------------------------------------------------------

data Visibilities = Vis ([QName],[QName]) ([QName],[QName]) ([QName],[QName])

getPrivateFunc (Vis (_  ,priv) _                   _) = priv

getPrivateType (Vis _          (_  ,priv)          _) = priv

getPrivateCons (Vis _          _          (_  ,priv)) = priv

analyzeVisibility :: Prog -> Visibilities
analyzeVisibility p =
  Vis (splitVisibleFuncs (progFuncs p))
      (splitVisibleTypes types)
      (splitVisibleCons  (concatMap typeConsDecls
                                    (filter (not . isTypeSyn) types)))
 where
  types = progTypes p

splitVisibleFuncs funcs =
  let (pubs, privs) =  partition (\f -> funcVisibility f == Public) funcs
  in  (map funcName pubs, map funcName privs)

splitVisibleTypes types =
  let (pubs, privs) = partition (\t -> typeVisibility t == Public) types
  in  (map typeName pubs, map typeName privs)

splitVisibleCons cons =
  let (pubs, privs) = partition (\c -> consVisibility c == Public) cons
  in  (map consName pubs, map consName privs)

-- -----------------------------------------------------------------------------
-- Special Identifiers
-- -----------------------------------------------------------------------------

qmark :: QName
qmark = renameQName (prelude, "?")

successType :: QName
successType = renameQName (prelude, "Success")

ioType :: QName
ioType = renameQName (prelude, "IO")

-- Small interface to Sets

empty :: SetRBT a
empty = emptySetRBT (<=)

unionMap :: (a -> SetRBT b) -> [a] -> SetRBT b
unionMap f = foldr unionRBT empty . map f

toList :: SetRBT a -> [a]
toList = setRBT2list
