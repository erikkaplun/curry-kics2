------------------------------------------------------------------------
--- Transformation from FlatCurry programs into AbstractHaskell programs.
---
--- Restrictions and hacks:
--- * Flexible case expressions are considered as rigid.
--- * A function having type (TVar (-42)) is considered as untyped.
---
--- @author Michael Hanus
--- @version February 2011
------------------------------------------------------------------------

module FlatCurry2AbstractHaskell(fcy2abs) where

import qualified FlatCurry as FC
import AbstractHaskell
import AbstractHaskellGoodies

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
  TypeSyn qf (vis2abs vis) (map tvar2abs targs) (texp2abs texp)
fcy2absTDecl (FC.Type qf vis targs cdecls) =
  Type qf (vis2abs vis) (map tvar2abs targs) (map tcons2abs cdecls)

fcy2absOp (FC.Op qf fix prio) = Op qf (fcy2absFix fix) prio

fcy2absFix FC.InfixOp = InfixOp
fcy2absFix FC.InfixlOp = InfixlOp
fcy2absFix FC.InfixrOp = InfixrOp

fcy2absFDecl (FC.Func qf ar vis texp rule) =
  if texp == FC.TVar (-42)  -- see module comment
  then Func "" qf ar (vis2abs vis) Nothing (fcy2absRule rule)
  else Func "" qf ar (vis2abs vis) (Just (texp2abs texp)) (fcy2absRule rule)

fcy2absRule (FC.Rule numargs expr) =
  Rules [Rule (map (PVar . var2abs) numargs)
              [noGuard (fcy2absExpr expr)] []]
fcy2absRule (FC.External ename) = External ename

fcy2absExpr (FC.Var i) = Var (var2abs i)
fcy2absExpr (FC.Lit l) = Lit (lit2abs l)
fcy2absExpr (FC.Comb _ qf es) = applyF qf (map fcy2absExpr es)
fcy2absExpr (FC.Let bs expr) = Let (map ldecl bs) (fcy2absExpr expr)
 where ldecl (i,e) = LocalPat (PVar (var2abs i)) (fcy2absExpr e) []
fcy2absExpr (FC.Free vs expr) = Let (map (LocalVar . var2abs) vs)
                                   (fcy2absExpr expr)
fcy2absExpr (FC.Or e1 e2) = applyF (pre "?") (map fcy2absExpr [e1,e2])
--fcy2absExpr (FC.Case FC.Flex _ _) = error "fcy2absExpr: Flex Case occurred!"
fcy2absExpr (FC.Case _ e brs) = Case (fcy2absExpr e) (map trBranch brs)
 where
  trBranch (FC.Branch pat expr) = Branch (trPattern pat) (fcy2absExpr expr)

  trPattern (FC.Pattern qf nums) = PComb qf (map (PVar . var2abs) nums)
  trPattern (FC.LPattern lit)    = PLit (lit2abs lit)

------------------------------------------------------------------------
-- Translating FlatCurry to AbstractHaskell
vis2abs :: FC.Visibility -> Visibility
vis2abs FC.Public  = Public
vis2abs FC.Private = Private

tvar2abs :: FC.TVarIndex -> TVarIName
tvar2abs i = (i, "t"++show i)

tcons2abs :: FC.ConsDecl -> ConsDecl
tcons2abs (FC.Cons qf ar vis texps) =
   Cons qf ar (vis2abs vis) (map texp2abs texps)

texp2abs :: FC.TypeExpr -> TypeExpr
texp2abs (FC.TVar i) = TVar (tvar2abs i)
texp2abs (FC.FuncType t1 t2) = FuncType (texp2abs t1) (texp2abs t2)
texp2abs (FC.TCons qf texps) = TCons qf (map texp2abs texps)

var2abs :: FC.VarIndex -> VarIName
var2abs i = (i, "x"++show i)

lit2abs :: FC.Literal -> Literal
lit2abs (FC.Intc   i) = Intc i
lit2abs (FC.Floatc f) = Floatc f
lit2abs (FC.Charc  c) = Charc c

------------------------------------------------------------------------
