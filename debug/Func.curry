module Func where

data Bool = True | False

f :: Bool -> Bool -> Bool
f x y = True ? False

g :: Bool -> Bool -> Bool
g x = (\y -> True ? False) ? (\y -> True)

fg :: Bool -> Bool -> Bool
fg = f ? g