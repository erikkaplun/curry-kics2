---------------------------------------------------------------------
--- This module provides a transformation on FlatCurry programs.
--- It replaces all calls to cond that stem from guarded expressions
--- by calls to auxiliary functions that are created by this
--- transformation.
--- A call to cond like
---
--- cond (x =:= 5) (f y (g z) + x)
---
--- where x, y, z are Variables and f,g are defined functions
--- will be transformed to a call
---
--- gen_name x y z (x =:= 5)
---
--- where gen_name is a generated function with a fresh name that
--- is defined as
---
--- gen_name x' y' z' s =
---   case s of
---     Success -> f y' (g z') + x'
--- 
--- This transformation can be beneficial for the optimization
--- of bindings for logic variables done by the KiCS2 compiler
---
--- @author fre
--- @version January 2012
---------------------------------------------------------------------


module EliminateCond (eliminateCond) where

import FlatCurry
import FlatCurryGoodies
import State
import List (elemIndex,nub)

-- main elimination function that eliminates cond calls in the whole
-- program
eliminateCond :: Prog -> Prog
eliminateCond p = updProgFuncs (concatMap transFunc) p

-- eliminates cond calls in a function by introducing new functions
-- that perform pattern matching. The transformed function is
-- returnd along with the auxiliary functions
transFunc :: FuncDecl -> [FuncDecl]
transFunc f@(Func _ _ _ _ (External _)) = [f]
transFunc (Func name k v t (Rule vars exp)) =
  let (newExp,(newFuns,_)) = runState (transExpr name exp) ([],0)
  in Func name k v t (Rule vars newExp) : newFuns

-- the transformation of an expression is done in the state monad
-- the state consists of the introduced auxiliary functions and
-- a counter for name generation
transExpr :: (String,String) -> Expr -> State Expr ([FuncDecl],Int)
transExpr n e = trExpr (returnS. Var) (returnS . Lit) transComb 
                transLet transFree transOr transCase transBranch transTyped e
 where 
  -- This is where the interesting stuff happens, if a call to
  -- cond is found, the call to an auxiliary function is generated
  transComb cType cName cargs =
    case (cName,cargs) of
     (("Prelude","cond"),[arg1,arg2]) 
       -> arg1 `bindS` \newArg1 ->
          arg2 `bindS` \newArg2 ->
          makeAuxFuncCall n newArg1 newArg2 
     _ -> sequenceS cargs `bindS` \newArgs ->
          returnS (Comb cType cName newArgs)
  -- for other expressions the transformation has to be done
  -- for their subexpressions
  transLet bindings exp = 
   let (vars, exps) = unzip bindings
   in sequenceS exps `bindS` \newExps ->
      exp            `bindS` \newExp  ->
      returnS (Let (zip vars newExps) newExp)
  transFree vars exp = 
    exp `bindS` \newExp -> 
    returnS (Free vars newExp)
  transOr exp1 exp2 =
   exp1 `bindS` \nExp1 ->
   exp2 `bindS` \nExp2 ->
   returnS (Or nExp1 nExp2)
  transCase ct exp bexps = 
    exp `bindS` \nExp ->
    sequenceS bexps `bindS` \nBexps ->
    returnS (Case ct nExp nBexps)
  transBranch pat exp = exp `bindS` \ newExp -> 
                        returnS (Branch pat newExp)
  transTyped exp ty = exp `bindS` \e' -> returnS (Typed e' ty)


-- This function creates a new function for a call to cond
-- it takes the two arguments of the cond call and creates
-- a new function that takes as arguments all the
-- unbound variables in the second argument of the cond call
-- and the first argument (the Success-expression).
-- The created function performs pattern matching on the
-- Success-exression and returns the second argument of
-- the cond-call with all unbound variables replaced
-- by the arguments of the newly created function
-- The created function is saved in the state, the
-- call to this function is returned. 
makeAuxFuncCall :: (String,String) -> Expr -> Expr -> State Expr ([FuncDecl],Int)
makeAuxFuncCall name succExpr newBody =
  getS                            `bindS` \(funs,idx) ->
  setS (newFun idx:funs, idx + 1) `bindS` \_          ->
  returnS (Comb FuncCall (mkNewName name idx) (map Var argVars ++ [succExpr])) 
 where
  newFun i = Func (mkNewName name i) 
                  numArgs
                  Private
                  (TVar (-42))
                  (Rule [1 .. numArgs] 
                        (Case Flex (Var numArgs)
                              [Branch (Pattern ("Prelude", "Success") [])
                                      (rnmAllVars renameVarFun newBody)]))
  argVars = unboundVars newBody
  numArgs = length argVars + 1
  renameVarFun v = maybe v (+1) (elemIndex v argVars) 
  mkNewName (mod,oldName) idx = (mod, "__cond_" ++ show idx ++ "_" ++ oldName)


-- returns all Variables in an expression that are not bound in the expression
unboundVars :: Expr -> [Int]
unboundVars e = nub (trExpr (:[]) (const []) comb leT freE oR casE branch typed e)
 where
 comb _ _ = concat
 leT bindings vars = let (bound,varsRHS) = unzip bindings
                     in filter (`notElem` bound) (concat (vars:varsRHS))
 freE frees = filter (`notElem` frees)
 oR         = (++)
 casE _ vars  bVars  = concat (vars:bVars)
 branch (Pattern _ bound) vars = filter (`notElem` bound) vars
 typed vs _ = vs
