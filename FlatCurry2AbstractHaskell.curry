------------------------------------------------------------------------
--- Transformation from FlatCurry programs into AbstractHaskell programs.
---
--- @author Michael Hanus
--- @version February 2011
------------------------------------------------------------------------

module FlatCurry2AbstractHaskell where

import qualified FlatCurry as FC
import AbstractHaskell
import AbstractHaskellGoodies

------------------------------------------------------------------------

--- Translates a FlatCurry program into an AbstractHaskell program.
fcy2ac :: FC.Prog -> Prog
fcy2ac (FC.Prog mname imps tdecls fdecls ops) =
  Prog mname
       imps
       (map fcy2acTDecl tdecls)
       (map fcy2acFDecl fdecls)
       (map fcy2acOp ops)


------------------------------------------------------------------------
-- Generate code for user-defined types.

fcy2acTDecl :: FC.TypeDecl -> TypeDecl
fcy2acTDecl (FC.TypeSyn qf vis targs texp) =
  TypeSyn qf (visibility2ac vis) (map tvar2ac targs) (texp2ac texp)
fcy2acTDecl (FC.Type qf vis targs cdecls) =
  Type qf (visibility2ac vis) (map tvar2ac targs) (map tcons2ac cdecls)

fcy2acOp (FC.Op qf fix prio) = Op qf (fcy2acFix fix) prio

fcy2acFix FC.InfixOp = InfixOp
fcy2acFix FC.InfixlOp = InfixlOp
fcy2acFix FC.InfixrOp = InfixrOp

fcy2acFDecl (FC.Func qf ar vis texp rule) =
  Func qf ar (visibility2ac vis) (texp2ac texp) (fcy2acRule rule)

fcy2acRule (FC.Rule numargs expr) =
  Rules [Rule (map (PVar . var2ac) numargs)
              [noGuard (fcy2acExpr expr)] []]
fcy2acRule (FC.External ename) = External ename

fcy2acExpr (FC.Var i) = Var (var2ac i)
fcy2acExpr (FC.Lit l) = Lit (lit2ac l)
fcy2acExpr (FC.Comb _ qf es) = applyF qf (map fcy2acExpr es)
fcy2acExpr (FC.Let bs expr) = Let (map ldecl bs) (fcy2acExpr expr)
 where ldecl (i,e) = LocalPat (PVar (var2ac i)) (fcy2acExpr e) []
fcy2acExpr (FC.Free vs expr) = Let (map (LocalVar . var2ac) vs)
                                   (fcy2acExpr expr)
fcy2acExpr (FC.Or e1 e2) = applyF (pre "?") (map fcy2acExpr [e1,e2])
fcy2acExpr (FC.Case FC.Flex _ _) = error "fcy2acExpr: Flex Case occurred!"
fcy2acExpr (FC.Case FC.Rigid e brs) = Case (fcy2acExpr e) (map trBranch brs)
 where
  trBranch (FC.Branch pat expr) = Branch (trPattern pat) (fcy2acExpr expr)

  trPattern (FC.Pattern qf nums) = PComb qf (map (PVar . var2ac) nums)
  trPattern (FC.LPattern lit)    = PLit (lit2ac lit)

------------------------------------------------------------------------
-- Translating FlatCurry to AbstractHaskell
visibility2ac :: FC.Visibility -> Visibility
visibility2ac FC.Public  = Public
visibility2ac FC.Private = Private

tvar2ac :: FC.TVarIndex -> TVarIName
tvar2ac i = (i, "t"++show i)

tcons2ac :: FC.ConsDecl -> ConsDecl
tcons2ac (FC.Cons qf ar vis texps) =
   Cons qf ar (visibility2ac vis) (map texp2ac texps)

texp2ac :: FC.TypeExpr -> TypeExpr
texp2ac (FC.TVar i) = TVar (tvar2ac i)
texp2ac (FC.FuncType t1 t2) = FuncType (texp2ac t1) (texp2ac t2)
texp2ac (FC.TCons qf texps) = TCons qf (map texp2ac texps)

var2ac :: FC.VarIndex -> VarIName
var2ac i = (i, "x"++show i)

lit2ac :: FC.Literal -> Literal
lit2ac (FC.Intc   i) = Intc i
lit2ac (FC.Floatc f) = Floatc f
lit2ac (FC.Charc  c) = Charc c

------------------------------------------------------------------------
