------------------------------------------------------------------------------
-- Some tests for the library UnsafeSearchTree.
------------------------------------------------------------------------------

import Assertion
import UnsafeSearchTree
import qualified SearchTree as ST

--------------------------------------------------------------------------------
-- Testing isVar:

testIsVar1 =
  assertTrue "isVar1" (not (isVar (someValue (let x free in x =:= (_,_) &> x))))

testIsVar2 =
  assertTrue "isVar2" (isVar (fst (someValue (let x free in x =:= (_,_) &> x))))

--------------------------------------------------------------------------------
-- Testing varId:

testGetVarId1 =
  assertTrue "varId1"
   (let (a,b) = someValue (let x free in x =:= (_,_) &> x)
     in (varId a /= varId b))

testGetVarId2 =
  assertTrue "varId2"
   (let (a,b) = someValue (let x,y,z free in (x =:= (y,z) & y=:=z) &> x)
     in (varId a == varId b))

--------------------------------------------------------------------------------
-- The following tests also demonstrate why the encapsulated search
-- with unbound variables in result values is non-declarative.

testNumSols1 =
  assertEqual "number of non-ground values"
   (length (allValuesDFS (someSearchTree (let x free in id (x::Bool)))))
   1

testNumSols2 =
  assertEqual "number of ground values"
   (length (allValuesDFS (someSearchTree (let x free in not (not (x::Bool))))))
   2

-- However, there is no difference w.r.t. the SearchTree library:

testNumSols3 =
  assertEqual "number of SearchTree values 1"
   (length (ST.allValuesDFS (ST.someSearchTree (let x free in id (x::Bool)))))
   2

testNumSols4 =
  assertEqual "number of SearchTree values 2"
   (length (ST.allValuesDFS (ST.someSearchTree
                                 (let x free in not (not (x::Bool))))))
   2

--------------------------------------------------------------------------------
