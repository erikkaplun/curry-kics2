--- ----------------------------------------------------------------------------
--- This module provides a transformation for typed FlatCurry which changes
--- the type of locally polymorphic sub-expressions to `()`.
---
--- A locally polymorphic sub-expression is an expression where its type
--- contains one or more type variables which are not included in the type
--- of the enclosing function declaration.
---
--- For instance, the function
---
---    f :: Bool
---    f = null []
---
--- where `[]` has the type `[a]`
---
--- will be transformed into
---
---    f :: Bool
---    f = null ([] :: [()])
---
--- This allows the above mentioned type of functions to be compiled into
--- Haskell without typing errors.
---
--- @author Björn Peemöller
--- @version July 2013
--- ----------------------------------------------------------------------------

module DefaultPolymorphic where

import FiniteMap
import List ((\\), nub)

import AFCSubst
import AnnotatedFlatCurry
import AnnotatedFlatCurryGoodies
import State

--- The Default Polymorphic Monad contains a type substitution as its state.
type DPM a = State AFCSubst a

--- The default type local type variables get replaced with.
defaultType :: TypeExpr
defaultType = TCons ("Prelude", "()") []

--- Transform a typed FlatCurry program by replacing all local type variables
--- with the `defaultType`.
defaultPolymorphic :: AProg TypeExpr -> AProg TypeExpr
defaultPolymorphic = updProgFuncs (map dpFunc)

--- Transform a single function.
--- For a function, first the rule is transformed, resulting in an updated
--- rule and a type substitution. The substitution is then applied to the
--- whole function to ensure that the annotated type information stays
--- consistent.
dpFunc :: AFuncDecl TypeExpr -> AFuncDecl TypeExpr
dpFunc (AFunc f k v t r)
  = let vs = tyVars t
        (r', sigma) = runState (dpRule r) (listToFM (<) (zip vs (map TVar vs)))
    in  substFunc sigma (AFunc f k v t r')

--- Transform a single rule.
dpRule :: ARule TypeExpr -> DPM (ARule TypeExpr)
dpRule (ARule   ty vs e) = ARule ty vs `liftS` dpExpr e
dpRule e@(AExternal _ _) = returnS e

--- Transform a single expression.
--- Expressions are transformed in a bottom-up manner, such that the smallest
--- polymorphic expression `e` with type `ty` is replaced with an explicitly
--- typed expression `e :: ty'`, where `ty'` is `ty` with the type variables
--- replaced by `()`.
--- Because the substitutions made are collected, repetitive substitution
--- of the same type variables is avoided. That is,
---
---     null ([] ++ [])
---
--- gets transformed to
---
---     null (([] :: [()]) ++ [])
---
--- instead of
---
---     null (([] :: [()]) ++ ([] :: [()]))
---
--- because both `[]` share the same type variable.
dpExpr :: AExpr TypeExpr -> DPM (AExpr TypeExpr)
dpExpr = trExpr var lit cmb lat fre orr cse bra typ
  where
  var ty v        = default ty $ AVar ty v
  lit ty l        = default ty $ ALit ty l
  cmb ty ct qn es = (AComb ty ct qn `liftS` sequenceS es) `bindS` default ty
  lat ty     bs e = let (vs, es) = unzip bs in
                    sequenceS es `bindS` \es' ->
                    e `bindS` \e' ->
                    default ty $ ALet ty (zip vs es') e'
  fre ty     vs e = e `bindS` default ty . AFree ty vs
  orr ty    e1 e2 = liftS2 (AOr ty) e1 e2 `bindS` default ty
  cse ty  ct e bs = liftS2 (ACase ty ct) e (sequenceS bs)
  bra         p e = ABranch p `liftS` e
  typ ty    e ty2 = ((\e' -> ATyped ty e' ty2) `liftS` e) `bindS` default ty

--- Check whether the given `TypeExpr` contains new type variables and
--- replace them with `defaultType` in this case.
default :: TypeExpr -> AExpr TypeExpr -> DPM (AExpr TypeExpr)
default ty e
  = getS `bindS` \sub ->
    let new = filter (\v -> not (elemFM v sub)) vs in
    if null new
      then returnS e
      else let sub' = listToFM (<) $ zip new (repeat defaultType) in
            modifyS (plusFM sub') `bindS_`
            returnS (ATyped ty e ty)
  where vs = tyVars ty

--- Retrieve all type variables in a type expression.
tyVars :: TypeExpr -> [TVarIndex]
tyVars = nub . trTypeExpr (:[]) (\_ -> concat) (++)
