module Main where

import System

import Basics
import Curry_Unification


main = do
  [goal] <- getArgs
  test goal

test "goal0" = print d_C_goal0
test "goal1" = print d_C_goal1
test "goal2" = prdfs nd_C_goal2
test "goal3" = prdfs nd_C_goal3
test "goal4" = prdfs nd_C_goal4
test "goal5" = prdfs nd_C_goal5
test "goal6" = prdfs nd_C_goal6
test "goal7" = prdfs nd_C_goal7
test "goal8" = prdfs nd_C_goal8
test "goal9" = prdfs nd_C_goal9
test "goal10" = prdfs nd_C_goal10
test "goal11" = prdfs nd_C_goal11
test "goal12" = prdfs nd_C_goal12
test "goal13" = prdfs nd_C_goal13
test "goal14" = prdfs nd_C_goal14
test "goal15" = prdfs nd_C_goal15
test "goal16" = prdfs nd_C_goal16
test "goal17" = prdfs nd_C_goal17
test "goal18" = prdfs nd_C_goal18
test "goal19" = prdfs nd_C_goal19
test "goal20" = prdfs nd_C_goal20
test "goal21" = prdfs nd_C_goal21
test "goal22" = prdfs nd_C_goal22
test "goal23" = prdfs nd_C_goal23
test "goal24" = prdfs nd_C_goal24
test "goal25" = prdfs nd_C_goal25
test "goal26" = prdfs nd_C_goal26
test "goal27" = prdfs nd_C_goal27
test "goal28" = prdfs nd_C_goal28
test "goal29" = prdfs nd_C_goal29
test "goal30" = prdfs nd_C_goal30
test "goal31" = prdfs nd_C_goal31
test "goal32" = prdfs nd_C_goal32
test "goal33" = prdfs nd_C_goal33
test "goal34" = prdfs nd_C_goal34
test _       = error "unknown goal"