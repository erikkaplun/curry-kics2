module NarrowTest where

data Nat = O | S Nat

isFinite :: Nat -> Bool
isFinite O = True
isFinite (S n) = isFinite n

frees f = f ? frees f

main = O =:= m &> isFinite m where m free

goal0 = y =:= not x &> x where x, y free