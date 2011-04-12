module Main where

import System
import System.Process
import Test.HUnit

dut = "./MainUnification"

testCmd descr arg res = TestLabel descr $ TestCase $ do
  let cmd = dut ++ ' ' : arg
  (ec, out, err) <- readProcessWithExitCode dut [arg] []
  assertEqual ("exitcode of " ++ cmd) ExitSuccess ec
  assertEqual ("stderr of " ++ cmd) [] err
  assertEqual ("stdout of " ++ cmd) res (lines out)

main = runTestTT tests

tests = TestList $
  [ testCmd "" "goal0" ["Success"]
  , testCmd "" "goal1" ["!"]
  , testCmd "" "goal2" ["Success"]
  , testCmd "" "goal3" ["True"]
  , testCmd "" "goal4" ["[True,True]"]
  , testCmd "" "goal5" ["[True,True]"]
  , testCmd "" "goal6" ["[True,True]"]
  , testCmd "" "goal7" ["Success"]
  , testCmd "" "goal8" ["[True,True]"]
  , testCmd "" "goal9" ["[True,True]"]
  , testCmd "" "goal10" ["[False,False]"]
  , testCmd "" "goal11" ["[False,False,False]"]
  , testCmd "" "goal12" ["[False,False,False]"]
  , testCmd "" "goal13" ["[False,False,False]"]
  , testCmd "" "goal14" ["False"]
  , testCmd "" "goal15" ["False", "True"]
  , testCmd "" "goal16" ["[True]"]
  , testCmd "" "goal17" ["[True]"]
  , testCmd "" "goal18" ["[True]"]
  , testCmd "" "goal19" []
  , testCmd "" "goal20" ["[True]"]
  , testCmd "" "goal21" ["[True]","[False]"]
  , testCmd "" "goal22" ["Success"]
  , testCmd "" "goal23" ["[Free]"]
  , testCmd "" "goal24" ["[True]"]
  , testCmd "" "goal25" ["[False,Free]"]
  , testCmd "" "goal26" ["[False]","[True]"]
  , testCmd "" "goal27" ["[False,True,True]"]
  , testCmd "" "goal28" ["[True]"]
  , testCmd "" "goal29" ["[True]"]
  , testCmd "" "goal30" []
  ]