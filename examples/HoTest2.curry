module HoTest2 where

data Nat = O | S Nat

data Oct a = Oct a a a a a a a a

inc x = S x

dec (S x) = x

coin = inc ? dec

oct :: (a -> b) -> Oct (a -> b)
oct x = Oct x x x x x x x x

appOct (Oct f g h i j k l m) x = f (g (h (i (j (k (l (m x)))))))

main = appOct (oct coin) (S O)