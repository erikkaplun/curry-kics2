module Main where

import TestHelper

testUni = testCmd "./MainUnification"

main = runTestTT $ TestList tests

tests =
  [ testUni "" "goal0" ["Success"]
  , testUni "" "goal1" ["!"]
  , testUni "" "goal2" ["Success"]
  , testUni "" "goal3" ["True"]
  , testUni "" "goal4" ["[True,True]"]
  , testUni "" "goal5" ["[True,True]"]
  , testUni "" "goal6" ["[True,True]"]
  , testUni "" "goal7" ["Success"]
  , testUni "" "goal8" ["[True,True]"]
  , testUni "" "goal9" ["[True,True]"]
  , testUni "" "goal10" ["[False,False]"]
  , testUni "" "goal11" ["[False,False,False]"]
  , testUni "" "goal12" ["[False,False,False]"]
  , testUni "" "goal13" ["[False,False,False]"]
  , testUni "" "goal14" ["False"]
  , testUni "" "goal15" ["False", "True"]
  , testUni "" "goal16" ["[True]"]
  , testUni "" "goal17" ["[True]"]
  , testUni "" "goal18" ["[True]"]
  , testUni "" "goal19" []
  , testUni "" "goal20" ["[True]"]
  , testUni "" "goal21" ["[True]","[False]"]
  , testUni "" "goal22" ["Success"]
  , testUni "" "goal23" ["[Free]"]
  , testUni "" "goal24" ["[True]"]
  , testUni "" "goal25" ["[False,Free]"]
  , testUni "" "goal26" ["[False]","[True]"]
  , testUni "" "goal27" ["[False,True,True]"]
  , testUni "" "goal28" ["[True]"]
  , testUni "" "goal29" ["[True]"]
  ]