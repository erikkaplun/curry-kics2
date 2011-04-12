module Main where

import TestHelper

testFP = testCmd "./MainFunctionPattern"

main = runTestTT $ TestList tests

tests =
  [ testFP "" "goal1" ["Success"]
  ]