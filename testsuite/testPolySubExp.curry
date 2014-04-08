------------------------------------------------------------------------------
--- Some tests for locally polymorphic sub-expressions, i.e., polymorphic
--- sub-expressions where the polymorphism is not visible in the type of the
--- enclosing expression.
--- This led to compilation errors with an earlier version of KiCS2, and is
--- fixed in commit:931bffe5085e5355cd7659f98d704f7c2932d2e6.
---
--- To run all tests automatically by the currytest tool, use the command:
--- "currytest testPolySubExp"
---
--- @author Björn Peemöller
--- @version July 2013
------------------------------------------------------------------------------

import Assertion

poly1 = fst (3, id)

testPoly1 = assertEqual "polymorphic id" poly1 3

poly2 = null []

testPoly2 = assertEqual "null of empty list" poly2 True

poly3 = case [] of
          _ -> 42

testPoly3 = assertEqual "matching empty list" poly3 42
