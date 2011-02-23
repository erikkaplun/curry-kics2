module Func where

data Bool = True | False

dconst :: Bool
dconst = True

ndconst :: Bool
ndconst = True ? False

dfun :: Bool -> Bool
dfun True  = False
dfun False = True

ndfun :: Bool -> Bool
ndfun b = True ? b

dfun2 :: Bool -> Bool -> Bool
dfun2 x y = case x of
  True -> y
  False -> False

ndfun2 :: Bool -> Bool -> Bool
ndfun2 x y = case ndconst of
  True -> x
  False -> y
