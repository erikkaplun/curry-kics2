module FunctionPattern where

last (xs ++ [x]) = x

k0 x = True
pair x y = (x, y)
f (pair (k0 x) x) = True

g True False = True

h (g x x) = True