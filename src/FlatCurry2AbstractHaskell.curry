------------------------------------------------------------------------
--- Transformation from FlatCurry programs into AbstractHaskell programs.
---
--- Restrictions and hacks:
--- * Flexible case expressions are considered as rigid.
--- * A function having type (TVar (-42)) is considered as untyped.
--- * Type parameters occurring in type signatures of functions
---   are decorated with type context "Curry".
---
--- @author Michael Hanus
--- @version February 2011
------------------------------------------------------------------------

module FlatCurry2AbstractHaskell where

import qualified FlatCurry as FC
import AbstractHaskell
import AbstractHaskellGoodies
import List  (union)
import Names (curryPrelude)

------------------------------------------------------------------------

--- Translates a FlatCurry program into an AbstractHaskell program.
fcy2abs :: FC.Prog -> Prog
fcy2abs (FC.Prog mname imps tdecls fdecls ops) =
  Prog mname
       imps
       (map fcy2absTDecl tdecls)
       (map fcy2absFDecl fdecls)
       (map fcy2absOp ops)

------------------------------------------------------------------------

fcy2absTDecl :: FC.TypeDecl -> TypeDecl
fcy2absTDecl (FC.TypeSyn qf vis targs texp) =
  TypeSyn qf (fcy2absVis vis) (map fcy2absTVar targs) (fcy2absTExp texp)
fcy2absTDecl (FC.Type qf vis targs cdecls) =
  Type qf (fcy2absVis vis) (map fcy2absTVar targs) (map fcy2absCDecl cdecls)

fcy2absOp :: FC.OpDecl -> OpDecl
fcy2absOp (FC.Op qf fix prio) = Op qf (fcy2absFix fix) prio

fcy2absFix :: FC.Fixity -> Fixity
fcy2absFix FC.InfixOp  = InfixOp
fcy2absFix FC.InfixlOp = InfixlOp
fcy2absFix FC.InfixrOp = InfixrOp

fcy2absFDecl :: FC.FuncDecl -> FuncDecl
fcy2absFDecl (FC.Func qf ar vis texp rule) =
  if texp == FC.TVar (-42)  -- see module comment
  then Func "" qf ar (fcy2absVis vis) Untyped (fcy2absRule rule)
  else Func "" qf ar (fcy2absVis vis) ftype (fcy2absRule rule)
  where
    tvars = tvarsOf texp
    ftype = if null tvars
      then FType (fcy2absTExp texp)
      else CType (concatMap (\tv ->
            [Context (curryPrelude,"Curry") [fcy2absTVar tv]]) tvars)
            (fcy2absTExp texp)

fcy2absRule :: FC.Rule -> Rules
fcy2absRule (FC.Rule numargs expr) =
  Rules [Rule (map (PVar . fcy2absVar) numargs)
              [noGuard (fcy2absExpr expr)] []]
fcy2absRule (FC.External ename)    = External ename

fcy2absExpr :: FC.Expr -> Expr
fcy2absExpr (FC.Var i) = Var (fcy2absVar i)
fcy2absExpr (FC.Lit l) = Lit (fcy2absLit l)
-- EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL
-- TODO: This is probably the dirtiest hack in the compiler:
-- Because FlatCurry does not allow lambda abstractions, in the module Compile
-- we construct a call to a function like "\f x1 x2 x-42 x3" which is replaced
-- with the expression (\z -> f x1 x2 z x3).
-- EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL ! EVIL
fcy2absExpr (FC.Comb _ qf es)
  | isLambdaHack = applyLambdaHack qf es
  | otherwise      = applyF qf (map fcy2absExpr es)
  where isLambdaHack = head (snd qf) == '\\'
fcy2absExpr (FC.Let bs expr)  = Let (map ldecl bs) (fcy2absExpr expr)
  where ldecl (i,e) = LocalPat (PVar (fcy2absVar i)) (fcy2absExpr e) []
fcy2absExpr (FC.Free vs expr) = Let (map (LocalVar . fcy2absVar) vs)
                                   (fcy2absExpr expr)
fcy2absExpr (FC.Or e1 e2) = applyF (pre "?") (map fcy2absExpr [e1, e2])
--fcy2absExpr (FC.Case FC.Flex _ _) = error "fcy2absExpr: Flex Case occurred!"
fcy2absExpr (FC.Case _ e brs) = Case (fcy2absExpr e) (map fcy2absBranch brs)
fcy2absExpr (FC.Typed e ty)   = Typed (fcy2absExpr e) (fcy2absTExp ty)

applyLambdaHack :: QName -> [FC.Expr] -> Expr
applyLambdaHack (q, fn) es = Lambda [PVar (1003, "z")]
  (applyF funcName (map applyLambda es))
  where
    applyLambda e
      | e == (FC.Var (-42)) = Var (1003, "z")
      | otherwise           = fcy2absExpr e
    funcName = (q, drop 1 fn)

fcy2absBranch :: FC.BranchExpr -> BranchExpr
fcy2absBranch (FC.Branch pat expr) =
  Branch (fcy2absPattern pat) (fcy2absExpr expr)

fcy2absPattern :: FC.Pattern -> Pattern
fcy2absPattern (FC.Pattern qf nums) = PComb qf (map (PVar . fcy2absVar) nums)
fcy2absPattern (FC.LPattern lit)    = PLit (fcy2absLit lit)

fcy2absVis :: FC.Visibility -> Visibility
fcy2absVis FC.Public  = Public
fcy2absVis FC.Private = Private

fcy2absTVar :: FC.TVarIndex -> TVarIName
fcy2absTVar i = (i, 't' : show i)

fcy2absCDecl :: FC.ConsDecl -> ConsDecl
fcy2absCDecl (FC.Cons qf ar vis texps) =
  Cons qf ar (fcy2absVis vis) (map fcy2absTExp texps)

fcy2absTExp :: FC.TypeExpr -> TypeExpr
fcy2absTExp (FC.TVar i)         = TVar (fcy2absTVar i)
fcy2absTExp (FC.FuncType t1 t2) = FuncType (fcy2absTExp t1) (fcy2absTExp t2)
fcy2absTExp (FC.TCons qf texps) = TCons qf (map fcy2absTExp texps)

fcy2absVar :: FC.VarIndex -> VarIName
fcy2absVar i = (i, 'x' : show i)

fcy2absLit :: FC.Literal -> Literal
fcy2absLit (FC.Intc   i) = Intc   i
fcy2absLit (FC.Floatc f) = Floatc f
fcy2absLit (FC.Charc  c) = Charc  c

------------------------------------------------------------------------
-- Auxiliaries:
tvarsOf :: FC.TypeExpr -> [FC.TVarIndex]
tvarsOf (FC.TVar tv)        = [tv]
tvarsOf (FC.FuncType t1 t2) = union (tvarsOf t1) (tvarsOf t2)
tvarsOf (FC.TCons _ texps)  = foldr union [] (map tvarsOf texps)
