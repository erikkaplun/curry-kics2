module Main where

import System

import Basics
import Curry_FunctionPattern

main =getArgs >>= test . head

test "goal1" = prdfs print nd_C_goal1
test _       = error "unknown goal"