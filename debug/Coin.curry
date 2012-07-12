module Coin where

--data Bool = True | False

coin = True ? False

selfEq x = iff x x

iff True  y = y
iff False y = mynot y

mynot True  = False
mynot False = True

goal0 = selfEq coin
