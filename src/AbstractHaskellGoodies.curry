------------------------------------------------------------------------
--- This module provides some useful functions to write the code
--- generating AbstractHaskell programs more compact and readable.
------------------------------------------------------------------------

module AbstractHaskellGoodies where

import AbstractHaskell
import Char            (toLower)
import List            (union)

infixr 9 ~>

-- lower the first character in a string
lowerFirst :: String -> String
lowerFirst (y:ys) = (toLower y) : ys
lowerFirst []     = [] -- this case should not occur, but one never knows...

--- An application of a qualified function name to a list of arguments.
applyF :: QName -> [Expr] -> Expr
applyF f es = foldl Apply (Symbol f) es

--- A constant, i.e., an application without arguments.
constF :: QName -> Expr
constF f = applyF f []

--- An application of a variable to a list of arguments.
applyV :: VarIName -> [Expr] -> Expr
applyV v es = foldl Apply (Var v) es

--- Constructs a tuple pattern from list of component patterns.
tuplePat :: [Pattern] -> Pattern
tuplePat ps | l==0 = PComb (pre "()") []
            | l==1 = head ps
            | otherwise = PComb (pre ('(' : take (l-1) (repeat ',') ++ ")")) ps
 where l = length ps

--- Constructs a tuple expression from list of component expressions.
tupleExpr :: [Expr] -> Expr
tupleExpr es | l==0 = constF (pre "()")
             | l==1 = head es
             | otherwise = applyF (pre ('(' : take (l-1) (repeat ',') ++ ")"))
                                  es
 where l = length es

--- Constructs a tuple pattern from list of component patterns.
tuplePattern :: [Pattern] -> Pattern
tuplePattern ps
  | l==0 = PComb (pre "()") []
  | l==1 = head ps
  | otherwise = PComb (pre ('(' : take (l-1) (repeat ',') ++ ")")) ps
 where l = length ps

--- A function type.
(~>) :: TypeExpr -> TypeExpr -> TypeExpr
t1 ~> t2 = FuncType t1 t2

-- A base type.
baseType :: QName -> TypeExpr
baseType t = TCons t []

--- Constructs a list type from element type.
listType :: TypeExpr -> TypeExpr
listType a = TCons (pre "[]") [a]

--- Constructs a tuple type from list of component types.
tupleType :: [TypeExpr] -> TypeExpr
tupleType ts | l==0 = baseType (pre "()")
             | l==1 = head ts
             | otherwise = TCons (pre ('(' : take (l-1) (repeat ',') ++ ")"))
                                  ts
 where l = length ts

--- Constructs an IO type from a type.
ioType :: TypeExpr -> TypeExpr
ioType a = TCons (pre "IO") [a]

--- Constructs a Maybe type from element type.
maybeType :: TypeExpr -> TypeExpr
maybeType a = TCons (pre "Maybe") [a]

stringType :: TypeExpr
stringType = baseType (pre "String")

intType :: TypeExpr
intType = baseType (pre "Int")

boolType :: TypeExpr
boolType = baseType (pre "Bool")

dateType :: TypeExpr
dateType = baseType ("Time", "CalendarTime")

tyVarsOf :: TypeExpr -> [TVarIName]
tyVarsOf (TVar        tv) = [tv]
tyVarsOf (FuncType t1 t2) = tyVarsOf t1 `union` tyVarsOf t2
tyVarsOf (TCons    _ tys) = foldr union [] (map tyVarsOf tys)

--- A typed function declaration.
tfunc :: QName -> Int -> Visibility -> TypeExpr -> [Rule] -> FuncDecl
tfunc name arity v t rules = Func "" name arity v (FType t) (Rules rules)

--- A typed function declaration with a type context.
ctfunc :: QName -> Int -> Visibility -> [Context] -> TypeExpr -> [Rule]
       -> FuncDecl
ctfunc name arity v tc t rules = Func "" name arity v (CType tc t) (Rules rules)

--- An untyped function declaration.
ufunc :: QName -> Int -> Visibility -> [Rule] -> FuncDecl
ufunc name arity v rules = Func "" name arity v Untyped (Rules rules)

--- A typed function declaration with a documentation comment.
cmtfunc :: String -> QName -> Int -> Visibility -> [Context] -> TypeExpr
        -> [Rule] -> FuncDecl
cmtfunc comment name arity v tc t rules =
  Func comment name arity v (CType tc t) (Rules rules)

-- transform a string constant into AbstractHaskell term:
string2ac :: String -> Expr
string2ac = Lit . Stringc

simpleRule :: [Pattern] -> Expr -> Rules
simpleRule ps e = Rules [Rule ps [noGuard e] []]

noGuard :: Expr -> (Expr, Expr)
noGuard e = (Symbol (pre "success"), e)

pre :: String -> QName
pre f = ("Prelude", f)

cvar :: String -> Expr
cvar s = Var (1,s)

-- let declaration (with possibly empty local declarations):
clet :: [LocalDecl] -> Expr -> Expr
clet locals cexp = if null locals then cexp else Let locals cexp

ctvar :: String -> TypeExpr
ctvar s = TVar (1,s)

-- from Spicey Framework
list2ac :: [Expr] -> Expr
list2ac []     = applyF (pre "[]") []
list2ac (c:cs) = applyF (pre ":") [c, list2ac cs]

declVar :: VarIName -> Expr -> LocalDecl
declVar v e = LocalPat (PVar v) e []

-----------------------------------------------------------------
renameSymbolInProg :: (QName -> QName) -> Prog -> Prog
renameSymbolInProg ren (Prog name imports typedecls fundecls opdecls) =
  Prog
    (fst (ren (name, "")))
    (map (\mod -> fst $ ren (mod, "")) imports)
    (map (renameSymbolInTypeDecl ren) typedecls)
    (map (renameSymbolInFunc ren) fundecls)
    (map (renameOpDecl ren) opdecls)

renameSymbolInTypeDecl :: (QName -> QName) -> TypeDecl -> TypeDecl
renameSymbolInTypeDecl ren tdecl = case tdecl of
  Type qf vis tvars cdecls  -> Type (ren qf) vis tvars
                                      (map (renameSymbolInConsDecl ren) cdecls)
  TypeSyn qf vis tvars texp -> TypeSyn (ren qf) vis tvars
                                         (renameSymbolInTypeExpr ren texp)
  Instance qf texp ctxt rules ->
    Instance (ren qf) (renameSymbolInTypeExpr ren texp)
              (map (\ (Context qn tvars) -> Context (ren qn) tvars) ctxt)
              (map renameSymbolInInstRule rules)
 where
  renameSymbolInInstRule (qf,rule) =
    (ren qf, renameSymbolInRule ren rule)

renameSymbolInConsDecl :: (QName -> QName) -> ConsDecl -> ConsDecl
renameSymbolInConsDecl ren (Cons qf ar vis texps) =
  Cons (ren qf) ar vis  (map (renameSymbolInTypeExpr ren) texps)

renameSymbolInTypeExpr :: (QName -> QName) -> TypeExpr -> TypeExpr
renameSymbolInTypeExpr ren texp = case texp of
  TCons qf texps   -> TCons (ren qf) (map (renameSymbolInTypeExpr ren) texps)
  FuncType te1 te2 -> FuncType (renameSymbolInTypeExpr ren te1)
                                 (renameSymbolInTypeExpr ren te2)
  TVar v           -> TVar v

renameSymbolInExpr :: (QName -> QName) -> Expr -> Expr
renameSymbolInExpr ren exp = case exp of
  Symbol qf           -> Symbol (ren qf)
  Apply e1 e2         -> Apply (renameSymbolInExpr ren e1)
                                 (renameSymbolInExpr ren e2)
  Lambda pats e       -> Lambda (map (renameSymbolInPat ren) pats)
                                  (renameSymbolInExpr ren e)
  Let locals e        -> Let (map (renameSymbolInLocal ren) locals)
                                  (renameSymbolInExpr ren e)
  DoExpr stats        -> DoExpr (map (renameSymbolInStat ren) stats)
  ListComp e stats    -> ListComp (renameSymbolInExpr ren e)
                                    (map (renameSymbolInStat ren) stats)
  Case e branches     -> Case (renameSymbolInExpr ren e)
                                (map (renameSymbolInBranch ren) branches)
  Typed e ty          -> Typed (renameSymbolInExpr ren e) ty
  IfThenElse e1 e2 e3 -> IfThenElse (renameSymbolInExpr ren e1)
                                      (renameSymbolInExpr ren e2)
                                        (renameSymbolInExpr ren e3)
  _ -> exp -- Var or Lit

renameSymbolInPat :: (QName -> QName) -> Pattern -> Pattern
renameSymbolInPat ren pat = case pat of
  PComb qf pats    -> PComb (ren qf) (map (renameSymbolInPat ren) pats)
  PAs var apat     -> PAs var (renameSymbolInPat ren apat)
  PFuncComb f pats -> PFuncComb (ren f) (map (renameSymbolInPat ren) pats)
  _                 -> pat -- PVar or PLit

renameSymbolInBranch :: (QName -> QName) -> BranchExpr -> BranchExpr
renameSymbolInBranch ren (Branch pat e) =
  Branch (renameSymbolInPat ren pat) (renameSymbolInExpr ren e)

renameSymbolInStat :: (QName -> QName) -> Statement -> Statement
renameSymbolInStat ren stat = case stat of
  SExpr e     -> SExpr (renameSymbolInExpr ren e)
  SPat pat e  -> SPat (renameSymbolInPat ren pat)
                        (renameSymbolInExpr ren e)
  SLet locals -> SLet (map (renameSymbolInLocal ren) locals)

renameSymbolInLocal :: (QName -> QName) -> LocalDecl -> LocalDecl
renameSymbolInLocal ren local = case local of
  LocalFunc fdecl       -> LocalFunc (renameSymbolInFunc ren fdecl)
  LocalPat pat e locals -> LocalPat (renameSymbolInPat ren pat)
                                      (renameSymbolInExpr ren e)
                                      (map (renameSymbolInLocal ren) locals)

renameSymbolInTypeSig :: (QName -> QName) -> TypeSig -> TypeSig
renameSymbolInTypeSig _ Untyped = Untyped
renameSymbolInTypeSig ren (FType te) = FType (renameSymbolInTypeExpr ren te)
renameSymbolInTypeSig ren (CType tc te) =
  CType (map (\ (Context qn tvars) -> Context (ren qn) tvars) tc)
        (renameSymbolInTypeExpr ren te)

renameSymbolInFunc :: (QName -> QName) -> FuncDecl -> FuncDecl
renameSymbolInFunc ren (Func cmt qf ar vis ctype rules) =
  Func cmt (ren qf) ar vis
       (renameSymbolInTypeSig ren ctype)
       (renameSymbolInRules ren rules)

renameSymbolInRules :: (QName -> QName) -> Rules -> Rules
renameSymbolInRules ren (Rules rules) =
  Rules (map (renameSymbolInRule ren) rules)
renameSymbolInRules _ (External s) = External s

renameSymbolInRule :: (QName -> QName) -> Rule -> Rule
renameSymbolInRule ren (Rule pats crhss locals) =
  Rule (map (renameSymbolInPat ren) pats)
        (map (\ (c,rhs)->(renameSymbolInExpr ren c,renameSymbolInExpr ren rhs))
             crhss)
        (map (renameSymbolInLocal ren) locals)

renameOpDecl :: (QName -> QName) -> OpDecl -> OpDecl
renameOpDecl ren (Op qf fix prio) = Op (ren qf) fix prio

-- ---------------------------------------------------------------
-- Some selector functions.

funcDecls :: Prog -> [FuncDecl]
funcDecls (Prog _ _ _ fs _) = fs

funcName :: FuncDecl -> QName
funcName (Func _ f _ _ _ _) = f

typeOf :: FuncDecl -> TypeSig
typeOf (Func _ _ _ _ ty _) = ty

commentOf :: FuncDecl -> String
commentOf (Func cmt _ _ _ _ _) = cmt
