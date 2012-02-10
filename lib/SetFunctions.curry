------------------------------------------------------------------------
--- This module provides an interface to set functions similarly to
--- the prototypical implementation of set functions in PAKCS.
--- IMPORTANT NOTE: this module only implements the same interface
--- but the functionality is different!
--- This stub is only intended to allow the compilation of
--- PAKCS applications containing set functions with KiCS2.
---
--- @author Michael Hanus
--- @version Wed Nov 16 18:30:02 CET 2011
------------------------------------------------------------------------

module SetFunctions
         (set0,set1,set2,set3,set4,set5,set6,set7,
          Values,isEmpty,valueOf,
          mapValues,foldValues,minValue,maxValue,
          values2list,printValues,sortValues,sortValuesBy)
 where

import Sort(mergeSort)
import SearchTree

--- Combinator to transform a 0-ary function into a corresponding set function.
set0 :: b -> Values b
set0 f = Values (allValuesDFS (someSearchTree f))

--- Combinator to transform a unary function into a corresponding set function.
set1 :: (a1 -> b) -> a1 -> Values b
set1 f x = Values (allVs (f (cover x)))

--- Combinator to transform a binary function into a corresponding set function.
set2 :: (a1 -> a2 -> b) -> a1 -> a2 -> Values b
set2 f x1 x2 = Values (allVs (f (cover x1) (cover x2)))

--- Combinator to transform a function of arity 3
--- into a corresponding set function.
set3 :: (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> Values b
set3 f x1 x2 x3 = Values (allVs (f (cover x1) (cover x2) (cover x3)))

--- Combinator to transform a function of arity 4
--- into a corresponding set function.
set4 :: (a1 -> a2 -> a3 -> a4 -> b) -> a1 -> a2 -> a3 -> a4 -> Values b
set4 f x1 x2 x3 x4 = Values (allVs (f (cover x1) (cover x2) (cover x3) (cover x4)))

--- Combinator to transform a function of arity 5
--- into a corresponding set function.
set5 :: (a1 -> a2 -> a3 -> a4 -> a5 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> Values b
set5 f x1 x2 x3 x4 x5 = Values (allVs (f (cover x1)(cover x2)(cover x3)(cover x4)(cover x5)))

--- Combinator to transform a function of arity 6
--- into a corresponding set function.
set6 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> Values b
set6 f x1 x2 x3 x4 x5 x6 =
 Values (allVs (f (cover x1)(cover x2)(cover x3)(cover x4)(cover x5)(cover x6)))

--- Combinator to transform a function of arity 7
--- into a corresponding set function.
set7 :: (a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> b)
      -> a1 -> a2 -> a3 -> a4 -> a5 -> a6 -> a7 -> Values b
set7 f x1 x2 x3 x4 x5 x6 x7 = 
 Values (allVs (f (cover x1)(cover x2)(cover x3)
                  (cover x4)(cover x5)(cover x6)(cover x7)))

------------------------------------------------------------------------
-- Axuiliaries:

-- collect all values of an expression in a list:
allVs x = allValuesDFS (someSearchTree x)

-- Function that covers identifiers
cover :: a -> a
cover external


------------------------------------------------------------------------
--- Abstract type representing multisets of values.

data Values a = Values [a]

--- Is a multiset of values empty?
isEmpty :: Values _ -> Bool
isEmpty (Values vs) = null vs

--- Is some value an element of a multiset of values?
valueOf :: a -> Values a -> Bool
valueOf e (Values s) = e `elem` s

--- Accumulates all elements of a multiset of values by applying a binary
--- operation. This is similarly to fold on lists, but the binary operation
--- must be <b>commutative</b> so that the result is independent of the order
--- of applying this operation to all elements in the multiset.
mapValues :: (a -> b) -> Values a -> Values b
mapValues f (Values s) = Values (map f s)

--- Accumulates all elements of a multiset of values by applying a binary
--- operation. This is similarly to fold on lists, but the binary operation
--- must be <b>commutative</b> so that the result is independent of the order
--- of applying this operation to all elements in the multiset.
foldValues :: (a -> a -> a) -> a -> Values a -> a
foldValues f z (Values s) = foldr f z s

--- Returns the minimal element of a non-empty multiset of values
--- with respect to a given total ordering on the elements.
minValue :: (a -> a -> Bool) -> Values a -> a
minValue leq (Values s) = minOf s
 where
  minOf [x] = x
  minOf (x:y:ys) = let m1 = minOf (y:ys)
                    in if leq x m1 then x else m1

--- Returns the maximal element of a non-empty multiset of value
--- with respect to a given total ordering on the elements.
maxValue :: (a -> a -> Bool) -> Values a -> a
maxValue leq (Values s) = maxOf s
 where
  maxOf [x] = x
  maxOf (x:y:ys) = let m1 = maxOf (y:ys)
                    in if leq x m1 then m1 else x

--- Puts all elements of a multiset of values in a list.
--- Since the order of the elements in the list might depend on
--- the time of the computation, this operation is an I/O action.
values2list :: Values a -> IO [a]
values2list (Values s) = return s

--- Prints all elements of a multiset of values.
printValues :: Values _ -> IO ()
printValues s = values2list s >>= mapIO_ print

--- Transforms a multiset of values into a list sorted by
--- the standard term ordering. As a consequence, the multiset of values
--- is completely evaluated.
sortValues :: Values a -> [a]
sortValues = sortValuesBy (<=)

--- Transforms a multiset of values into a list sorted by a given ordering
--- on the values. As a consequence, the multiset of values
--- is completely evaluated.
--- In order to ensure that the result of this operation is independent of the
--- evaluation order, the given ordering must be a total order.
sortValuesBy :: (a -> a -> Bool) -> Values a -> [a]
sortValuesBy leq (Values s) = mergeSort leq s

------------------------------------------------------------------------
