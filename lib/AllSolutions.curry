------------------------------------------------------------------------------
--- This module contains a collection of functions for
--- obtaining lists of solutions to constraints.
--- These operations are useful to encapsulate
--- non-deterministic operations between I/O actions in
--- order to connects the worlds of logic and functional programming
--- and to avoid non-determinism failures on the I/O level.
---
--- In contrast the "old" concept of encapsulated search
--- (which could be applied to any subexpression in a computation),
--- the operations to encapsulate search in this module
--- are I/O actions in order to avoid some anomalities
--- in the old concept.
------------------------------------------------------------------------------

module AllSolutions(getAllValues,getOneValue,getAllSolutions,getOneSolution,
                    getAllFailures)  where

import SearchTree

--- Gets all values of an expression (currently, via an incomplete
--- depth-first strategy). Conceptually, all values are computed
--- on a copy of the expression, i.e., the evaluation of the expression
--- does not share any results. Moreover, the evaluation suspends
--- as long as the expression contains unbound variables.
getAllValues :: a -> IO [a]
getAllValues e = getSearchTree e >>= return . allValuesDFS

--- Gets one value of an expression (currently, via an incomplete
--- left-to-right strategy). Returns Nothing if the search space
--- is finitely failed.
getOneValue :: a -> IO (Maybe a)
getOneValue x = do
  st <- getSearchTree x
  let vals = allValuesDFS st
  return (if null vals then Nothing else Just (head vals))

--- Gets all solutions to a constraint (currently, via an incomplete
--- depth-first left-to-right strategy). Conceptually, all solutions
--- are computed on a copy of the constraint, i.e., the evaluation
--- of the constraint does not share any results. Moreover, this
--- evaluation suspends if the constraints contain unbound variables.
--- Similar to Prolog's findall.
getAllSolutions :: (a->Success) -> IO [a]
getAllSolutions c = getAllValues (let x free in (x,c x)) >>= return . map fst

--- Gets one solution to a constraint (currently, via an incomplete
--- left-to-right strategy). Returns Nothing if the search space
--- is finitely failed.
getOneSolution :: (a->Success) -> IO (Maybe a)
getOneSolution c = do
  sols <- getAllSolutions c
  return (if null sols then Nothing else Just (head sols))

--- Returns a list of values that do not satisfy a given constraint.
--- @param x - an expression (a generator evaluable to various values)
--- @param c - a constraint that should not be satisfied
--- @return A list of all values of e such that (c e) is not provable
getAllFailures :: a -> (a->Success) -> IO [a]
getAllFailures generator test =
 do xs <- getAllValues generator
    failures <- mapIO (naf test) xs
    return $ concat failures

-- (naf c x) returns [x] if (c x) fails, and [] otherwise.
naf :: (a->Success) -> a -> IO [a]
naf c x = getOneSolution (lambda c x) >>= returner x

lambda :: (a->Success) -> a -> () -> Success
lambda c x _ = c x

returner :: a -> Maybe b -> IO [a]
returner x mbl = return (maybe [x] (const []) mbl)


