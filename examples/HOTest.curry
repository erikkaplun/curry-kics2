module HOTest (mymap) where

data List a = Nil | Cons a (List a)

mymap :: (a -> b) -> List a -> List b
mymap _ Nil         = Nil
mymap f (Cons x xs) = Cons (f x) (mymap f xs)
