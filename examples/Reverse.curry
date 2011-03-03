module Reverse where

--data Bool = True | False
data List a = Nil | Cons a (List a)

--- Reverses the order of all elements in a list.
reverse :: List a -> List a
reverse = foldl (flip Cons) Nil

-- Version without eta-reduce
reverse2 :: List a -> List a
reverse2 xs = foldl (flip Cons) Nil xs

foldl :: (a -> b -> a) -> a -> List b -> a
foldl _ z Nil         = z
foldl f z (Cons x xs) = foldl f (f z x) xs

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x

testList = Cons True (Cons False Nil)

goal0 = reverse testList
goal1 = reverse2 testList
