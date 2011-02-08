------------------------------------------------------------------------
--- This module provides some useful functions to write the code
--- generating AbstractCurry programs more compact and readable.
------------------------------------------------------------------------

module AbstractCurryGoodies where

import AbstractCurry
import Char(toLower)

infixr 9 ~>

-- lower the first character in a string
lowerFirst :: String -> String
lowerFirst (y:ys) = (toLower y) : ys
lowerFirst []     = [] -- this case should not occur, but one never knows...

--- An application of a qualified function name to a list of arguments.
applyF :: QName -> [CExpr] -> CExpr
applyF f es = foldl CApply (CSymbol f) es 

--- A constant, i.e., an application without arguments.
constF :: QName -> CExpr
constF f = applyF f []

--- An application of a variable to a list of arguments.
applyV :: CVarIName -> [CExpr] -> CExpr
applyV v es = foldl CApply (CVar v) es 

--- Constructs a tuple expression from list of component expressions.
tupleExpr :: [CExpr] -> CExpr
tupleExpr es | l==0 = constF (pre "()")
             | l==1 = head es
             | otherwise = applyF (pre ('(' : take (l-1) (repeat ',') ++ ")"))
                                  es
 where l = length es

--- Constructs a tuple pattern from list of component patterns.
tuplePattern :: [CPattern] -> CPattern
tuplePattern ps
  | l==0 = CPComb (pre "()") []
  | l==1 = head ps
  | otherwise = CPComb (pre ('(' : take (l-1) (repeat ',') ++ ")")) ps
 where l = length ps

--- A function type.
(~>) :: CTypeExpr -> CTypeExpr -> CTypeExpr
t1 ~> t2 = CFuncType t1 t2

-- A base type.
baseType :: QName -> CTypeExpr
baseType t = CTCons t []

--- Constructs a list type from element type.
listType :: CTypeExpr -> CTypeExpr
listType a = CTCons (pre "[]") [a]

--- Constructs a tuple type from list of component types.
tupleType :: [CTypeExpr] -> CTypeExpr
tupleType ts | l==0 = baseType (pre "()")
             | l==1 = head ts
             | otherwise = CTCons (pre ('(' : take (l-1) (repeat ',') ++ ")"))
                                  ts
 where l = length ts

--- Constructs an IO type from a type.
ioType :: CTypeExpr -> CTypeExpr
ioType a = CTCons (pre "IO") [a]

--- Constructs a Maybe type from element type.
maybeType :: CTypeExpr -> CTypeExpr
maybeType a = CTCons (pre "Maybe") [a]

stringType :: CTypeExpr
stringType = baseType (pre "String")

intType :: CTypeExpr
intType = baseType (pre "Int")

boolType :: CTypeExpr
boolType = baseType (pre "Bool")

dateType :: CTypeExpr
dateType = baseType ("Time", "CalendarTime")


cfunc :: QName -> Int -> CVisibility -> CTypeExpr -> [CRule] -> CFuncDecl
cfunc name arity v t rules = 
  CFunc name arity v t (CRules CFlex rules)

cmtfunc :: String -> QName -> Int -> CVisibility -> CTypeExpr -> [CRule] -> CFuncDecl
cmtfunc comment name arity v t rules = 
  CmtFunc comment name arity v t (CRules CFlex rules)

-- transform a string constant into AbstractCurry term:
string2ac :: String -> CExpr
string2ac []     = constF (pre "[]")
string2ac (c:cs) = applyF (pre ":") [CLit (CCharc c), string2ac cs]

noGuard :: CExpr -> (CExpr, CExpr)
noGuard e = (CSymbol (pre "success"), e)

pre :: String -> QName
pre f = ("Prelude", f)

cvar :: String -> CExpr
cvar s = CVar (1,s)

-- let declaration (with possibly empty local delcarations):
clet :: [CLocalDecl] -> CExpr -> CExpr
clet locals cexp = if null locals then cexp else CLetDecl locals cexp

ctvar :: String -> CTypeExpr
ctvar s = CTVar (1,s)

-- from Spicey Framework
list2ac :: [CExpr] -> CExpr
list2ac []     = applyF (pre "[]") []
list2ac (c:cs) = applyF (pre ":") [c, list2ac cs]

-----------------------------------------------------------------
renameSymbolInTypeDecl :: (QName -> QName) -> CTypeDecl -> CTypeDecl
renameSymbolInTypeDecl ren tdecl = case tdecl of
  CType qf vis tvars cdecls  -> CType (ren qf) vis tvars
                                      (map (renameSymbolInConsDecl ren) cdecls)
  CTypeSyn qf vis tvars texp -> CTypeSyn (ren qf) vis tvars
                                         (renameSymbolInTypeExpr ren texp)
  CInstance qf texp ctxt rules ->
    CInstance (ren qf) (renameSymbolInTypeExpr ren texp)
              (map (\ (CContext qn tvars) -> CContext (ren qn) tvars) ctxt)
              (map renameSymbolInInstRule rules)
 where
  renameSymbolInInstRule (qf,rule) =
    (ren qf, renameSymbolInRule ren rule)

renameSymbolInConsDecl :: (QName -> QName) -> CConsDecl -> CConsDecl
renameSymbolInConsDecl ren (CCons qf ar vis texps) =
  CCons (ren qf) ar vis  (map (renameSymbolInTypeExpr ren) texps)

renameSymbolInTypeExpr :: (QName -> QName) -> CTypeExpr -> CTypeExpr
renameSymbolInTypeExpr ren texp = case texp of
  CTCons qf texps   -> CTCons (ren qf) (map (renameSymbolInTypeExpr ren) texps)
  CFuncType te1 te2 -> CFuncType (renameSymbolInTypeExpr ren te1)
                                 (renameSymbolInTypeExpr ren te2)
  CTVar v           -> CTVar v

renameSymbolInExpr :: (QName -> QName) -> CExpr -> CExpr
renameSymbolInExpr ren exp = case exp of
  CSymbol qf        -> CSymbol (ren qf)
  CApply e1 e2      -> CApply (renameSymbolInExpr ren e1)
                              (renameSymbolInExpr ren e2)
  CLambda pats e    -> CLambda (map (renameSymbolInPat ren) pats)
                               (renameSymbolInExpr ren e)
  CLetDecl locals e -> CLetDecl (map (renameSymbolInLocal ren) locals)
                                (renameSymbolInExpr ren e)
  CDoExpr stats     -> CDoExpr (map (renameSymbolInStat ren) stats)
  CListComp e stats -> CListComp (renameSymbolInExpr ren e)
                                 (map (renameSymbolInStat ren) stats)
  CCase e branches  -> CCase (renameSymbolInExpr ren e)
                             (map (renameSymbolInBranch ren) branches)
  _ -> exp -- CVar or CLit

renameSymbolInPat :: (QName -> QName) -> CPattern -> CPattern
renameSymbolInPat ren pat = case pat of
  CPComb qf pats    -> CPComb (ren qf) (map (renameSymbolInPat ren) pats)
  CPAs var apat     -> CPAs var (renameSymbolInPat ren apat)
  CPFuncComb f pats -> CPFuncComb (ren f) (map (renameSymbolInPat ren) pats)
  _                 -> pat -- CPVar or CPLit

renameSymbolInBranch :: (QName -> QName) -> CBranchExpr -> CBranchExpr
renameSymbolInBranch ren (CBranch pat e) =
  CBranch (renameSymbolInPat ren pat) (renameSymbolInExpr ren e)

renameSymbolInStat :: (QName -> QName) -> CStatement -> CStatement
renameSymbolInStat ren stat = case stat of
  CSExpr e     -> CSExpr (renameSymbolInExpr ren e)
  CSPat pat e  -> CSPat (renameSymbolInPat ren pat)
                        (renameSymbolInExpr ren e)
  CSLet locals -> CSLet (map (renameSymbolInLocal ren) locals)

renameSymbolInLocal :: (QName -> QName) -> CLocalDecl -> CLocalDecl
renameSymbolInLocal ren local = case local of
  CLocalFunc fdecl       -> CLocalFunc (renameSymbolInFunc ren fdecl)
  CLocalPat pat e locals -> CLocalPat (renameSymbolInPat ren pat)
                                      (renameSymbolInExpr ren e)
                                      (map (renameSymbolInLocal ren) locals)
  _ -> local -- CLoalVar

renameSymbolInFunc :: (QName -> QName) -> CFuncDecl -> CFuncDecl
renameSymbolInFunc ren (CFunc qf ar vis ctype rules) =
  CFunc (ren qf) ar vis (renameSymbolInTypeExpr ren ctype)
        (renameSymbolInRules ren rules)
renameSymbolInFunc ren (CmtFunc cmt qf ar vis ctype rules) =
  CmtFunc cmt (ren qf) ar vis (renameSymbolInTypeExpr ren ctype)
          (renameSymbolInRules ren rules)

renameSymbolInRules :: (QName -> QName) -> CRules -> CRules
renameSymbolInRules ren (CRules ev rules) =
  CRules ev (map (renameSymbolInRule ren) rules)
renameSymbolInRules _ (CExternal s) = CExternal s

renameSymbolInRule :: (QName -> QName) -> CRule -> CRule
renameSymbolInRule ren (CRule pats crhss locals) =
  CRule (map (renameSymbolInPat ren) pats)
        (map (\ (c,rhs)->(renameSymbolInExpr ren c,renameSymbolInExpr ren rhs))
             crhss)
        (map (renameSymbolInLocal ren) locals)

renameOpDecl :: (QName -> QName) -> COpDecl -> COpDecl
renameOpDecl ren (COp qf fix prio) = COp (ren qf) fix prio


-----------------------------------------------------------------
funcDecls (CurryProg _ _ _ fdecls _) = fdecls

funcName (CFunc f _ _ _ _) = f
funcName (CmtFunc _ f _ _ _ _) = f

typeOf (CFunc     _ _ _ texp _) = texp
typeOf (CmtFunc _ _ _ _ texp _) = texp

commentOf (CFunc       _ _ _ _ _) = ""
commentOf (CmtFunc cmt _ _ _ _ _) = cmt

--- Deletes the comment in a function declaration.
deleteCmt (CFunc     qn ar vis texp rules) = CFunc qn ar vis texp rules
deleteCmt (CmtFunc _ qn ar vis texp rules) = CFunc qn ar vis texp rules

--- Deletes the comment in a function declaration if it is the empty string.
deleteCmtIfEmpty (CFunc qn ar vis texp rules)     = CFunc qn ar vis texp rules
deleteCmtIfEmpty (CmtFunc cmt qn ar vis texp rules) =
  if null cmt then CFunc qn ar vis texp rules
              else CmtFunc cmt qn ar vis texp rules

-----------------------------------------------------------------
