module HoTest2 where

data Nat = O | S Nat

data Pair a = Pair a a

inc x = S x

dec (S x) = x

coin = inc ? dec

double :: (a -> b) -> Pair (a -> b)
double x = Pair x x

double2 x = Pair x x

appPair (Pair f g) x = f (g x)

main = appPair (double coin) (S O)

main2 = appPair (double2 coin) (S O)