--- ----------------------------------------------------------------------------
--- This module extends the import list of a program to cover all modules
--- of which types are used in type signatures.
---
--- Motivation: The type inference for FlatCurry may infer types for expressions
--- which cannot be denoted explicitly in the program because of missing
--- imports. If these expressions are lifted to top-level functions
--- during case lifting, an invalid type signature would be generated.
---
--- @author Björn Peemöller
--- @version August 2013
--- ----------------------------------------------------------------------------

module MissingImports (fixMissingImports) where

import SetRBT

import FlatCurry.Annotated.Types
import FlatCurry.Annotated.Goodies
import State

type ModuleName = String

--- The Module Monad contains all modules used in the current module.
type MM a = State (SetRBT ModuleName) a

--- Extend the import list of an AnnotatedFlatCurry program with modules
--- of which types are used in type signatures of the program
fixMissingImports :: AProg TypeExpr -> AProg TypeExpr
fixMissingImports p@(AProg m _ ts fs os)
  = let allModules = execState (vsProg p) (emptySetRBT (<))
    in  AProg m (setRBT2list $ deleteRBT m allModules) ts fs os

addModule :: ModuleName -> MM ()
addModule m = modifyS $ \ s -> insertRBT m s

vsProg :: AProg TypeExpr -> MM ()
vsProg (AProg _ is _ fs _) = mapS_ addModule is `bindS_` mapS_ vsFuncDecl fs

vsFuncDecl :: AFuncDecl TypeExpr -> MM ()
vsFuncDecl (AFunc _ _ _ ty r) = vsTypeExpr ty `bindS_` vsRule r

vsTypeExpr :: TypeExpr -> MM ()
vsTypeExpr (TVar           _) = returnS ()
vsTypeExpr (FuncType ty1 ty2) = vsTypeExpr ty1 `bindS_` vsTypeExpr ty2
vsTypeExpr (TCons (m, _) tys) = addModule m `bindS_` mapS_ vsTypeExpr tys

vsRule :: ARule TypeExpr -> MM ()
vsRule (ARule     _ _ e) = vsExpr e
vsRule (AExternal   _ _) = returnS ()

vsExpr :: AExpr TypeExpr -> MM ()
vsExpr (AVar       _ _) = returnS ()
vsExpr (ALit       _ _) = returnS ()
vsExpr (AComb _ _ _ es) = mapS_ vsExpr es
vsExpr (ALet    _ ds e) = mapS_ (vsExpr . snd) ds `bindS_` vsExpr e
vsExpr (AFree    _ _ e) = vsExpr e
vsExpr (AOr    _ e1 e2) = vsExpr e1 `bindS_` vsExpr e2
vsExpr (ACase _ _ e bs) = vsExpr e `bindS_` mapS_ (vsExpr . branchExpr) bs
-- Relevant part: Add modules of which types are used in 'ty' to set
vsExpr (ATyped  _ e ty) = vsExpr e `bindS_` vsTypeExpr ty


