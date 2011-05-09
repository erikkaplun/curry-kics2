------------------------------------------------------------------------------
--- This module defines the datatype and operations for the
--- Curry module tester "currytest".
---
--- @author Michael Hanus
--- @version May 2011
------------------------------------------------------------------------------

module Assertion(-- for writing test cases:
                 Assertion,assertTrue,assertEqual,
                 assertValues,assertSolutions,assertIO,assertEqualIO,
                 -- the remaining entities are only used by the test tool:
                 checkAssertion,
                 seqStrActions,writeAssertResult,
                 ProtocolMsg(..),
                 showTestMod,showTestCase,showTestEnd,showTestCompileError)
 where

import AllSolutions
import List((\\))
import Socket -- for sending results to test GUI
import IO(hPutStrLn,hClose)

infixl 1 `seqStrActions`

--- Datatype for defining test cases.
--- @cons AssertTrue   s b     - assert (with name s) that b must be true
--- @cons AssertEqual  s e1 e2 - assert (with name s) that e1 and e2 must
---                              be equal (w.r.t. ==)
--- @cons AssertValues s e vs  - assert (with name s) that vs is the multiset
---                              of all values of e (i.e., all values of e are
---                              compared with the elements in vs w.r.t. ==)
--- @cons AssertSolutions s c vs - assert (with name s) that constraint
---   abstraction c has the multiset of solutions vs
---   (i.e., the solutions of c are compared with the elements in vs w.r.t. ==)
--- @cons AssertIO     s a r   - assert (with name s) that I/O action a
---                              yields the result value r
--- @cons AssertEqualIO s a1 a2 - assert (with name s) that I/O actions a1 and
---                               a2 yield equal (w.r.t. ==) results
data Assertion a = AssertTrue      String Bool
                 | AssertEqual     String a a
                 | AssertValues    String a [a]
                 | AssertSolutions String (a->Success) [a]
                 | AssertIO        String (IO a) a
                 | AssertEqualIO   String (IO a) (IO a)


assertTrue :: String -> Bool -> Assertion ()
assertTrue s b = AssertTrue s b

assertEqual :: String -> a -> a -> Assertion a
assertEqual s x y = AssertEqual s x y

assertValues :: String -> a -> [a] -> Assertion a
assertValues s x y = AssertValues s x y

assertSolutions :: String -> (a->Success) -> [a] -> Assertion a
assertSolutions s x y = AssertSolutions s x y

assertIO :: String -> IO a -> a -> Assertion a
assertIO s x y = AssertIO s x y

assertEqualIO :: String -> IO a -> IO a -> Assertion a
assertEqualIO s x y = AssertEqualIO s x y

--- Combines two actions and combines their results.
--- Used by the currytest tool.
seqStrActions :: IO (String,Bool) -> IO (String,Bool) -> IO (String,Bool)
seqStrActions a1 a2 =
  do (s1,b1) <- a1
     (s2,b2) <- a2
     return (s1++s2,b1&&b2)

--- Executes and checks an assertion, and process the result
--- by an I/O action.
--- Used by the currytest tool.
--- @param protocol - an action to be applied after test execution
--- @param assertion - an assertion to be tested
--- @return a protocol string and a flag whether the test was successful
checkAssertion :: ((String,Bool) -> IO (String,Bool)) -> Assertion _
                                                      -> IO (String,Bool)
checkAssertion prot (AssertTrue name cond) =
  catchFail (checkAssertTrue name cond)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertEqual name call result) =
  catchFail (checkAssertEqual name call result)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertValues name expr results) =
  catchFail (checkAssertValues name expr results)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertSolutions name constr results) =
  catchFail (checkAssertSolutions name constr results)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertIO name action result) =
  catchFail (checkAssertIO name action result)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot
checkAssertion prot (AssertEqualIO name action1 action2) =
  catchFail (checkAssertEqualIO name action1 action2)
            (return ("FAILURE of "++name++": no solution or error\n",False))
   >>= prot

-- Checks Boolean assertion.
checkAssertTrue :: String -> Bool -> IO (String,Bool)
checkAssertTrue name cond =
  if cond
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": assertion not satisfied\n",False)

-- Checks equality assertion.
checkAssertEqual :: String -> a -> a -> IO (String,Bool)
checkAssertEqual name call result = do
  let r = call
  if r==result
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": equality assertion not satisfied:\n"++
                "Computed answer: "++show r++"\n"++
                "Expected answer: "++show result++"\n",False)

-- Checks all values assertion.
checkAssertValues :: String -> a -> [a] -> IO (String,Bool)
checkAssertValues name call results = do
  rs <- getAllValues call
  if null (rs \\ results) && null (results \\ rs)
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": values assertion not satisfied:\n"++
                "Computed values: "++show rs++"\n"++
                "Expected values: "++show results++"\n",False)

-- Checks all solutions of a constraint abstraction.
checkAssertSolutions :: String -> (a->Success) -> [a] -> IO (String,Bool)
checkAssertSolutions name _ _ =
  return ("FAILURE of "++name++": assertSolution not yet implemented!\n",False)
{-
checkAssertSolutions name constr results = do
  rs <- getAllSolutions constr
  if null (rs \\ results) && null (results \\ rs)
   then return ("OK: "++name++"\n",True)
   else return ("FAILURE of "++name++": solutions assertion not satisfied:\n"++
                "Computed values: "++show rs++"\n"++
                "Expected values: "++show results++"\n",False)
-}

-- Checks an IO assertion.
checkAssertIO :: String -> IO a -> a -> IO (String,Bool)
checkAssertIO name action result = do
  r <- action
  if r==result
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": IO assertion not satisfied:\n"++
                 "Computed answer: "++show r++"\n"++
                 "Expected answer: "++show result++"\n\n",False)

-- Checks equality of results of two IO assertions.
checkAssertEqualIO :: String -> IO a -> IO a -> IO (String,Bool)
checkAssertEqualIO name action1 action2 = do
  r1 <- action1
  r2 <- action2
  if r1==r2
    then return ("OK: "++name++"\n",True)
    else return ("FAILURE of "++name++": IO equality assertion not satisfied:\n"++
                 "Computed answer 1: "++show r1++"\n"++
                 "Computed answer 2: "++show r2++"\n\n",False)

--- Writes the results of assertion checking into a file and stdout,
--- if the results are non-empty.
--- Used by the currytest tool.
writeAssertResult :: (String,Bool) -> IO ()
writeAssertResult (result,flag) =
  if flag
  then putStrLn (result++"All tests successfully passed.")
  else putStrLn (result++"FAILURE occurred in some assertions!\n")


----------------------------------------------------------------------------
-- The following entities are used to implement the test GUI:

--- The messages sent to the test GUI.
--- Used by the currytest tool.
data ProtocolMsg = TestModule String | TestCase String Bool | TestFinished
                 | TestCompileError

--- Sends message to GUI for showing test of a module.
--- Used by the currytest tool.
showTestMod :: Int -> String -> IO ()
showTestMod portnum modname = sendToLocalSocket portnum (TestModule modname)

--- Sends message to GUI for showing result of executing a test case.
--- Used by the currytest tool.
showTestCase :: Int -> (String,Bool) -> IO (String,Bool)
showTestCase portnum (s,b) = do
  sendToLocalSocket portnum (TestCase s b)
  return (s,b)

--- Sends message to GUI for showing end of module test.
--- Used by the currytest tool.
showTestEnd :: Int -> IO ()
showTestEnd portnum = sendToLocalSocket portnum TestFinished

--- Sends message to GUI for showing compilation errors in a module test.
--- Used by the currytest tool.
showTestCompileError :: Int -> IO ()
showTestCompileError portnum = sendToLocalSocket portnum TestCompileError

--- Sends protocol message to local socket.
sendToLocalSocket :: Int -> ProtocolMsg -> IO ()
sendToLocalSocket portnum msg = do
  h <- connectToSocket "localhost" portnum
  hPutStrLn h (show msg)
  hClose h

-- end of module Assertion
