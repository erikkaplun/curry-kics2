module TestReverse where

data List a = Nil | Cons a (List a)

--- Reverses the order of all elements in a list.
reverse    :: List a -> List a
reverse    = foldl (flip consFunc) Nil

reverse'    :: List a -> List a
reverse' = foldl (flip Cons) Nil

foldl            :: (a -> b -> a) -> a -> List b -> a
foldl _ z Nil         = z
foldl f z (Cons x xs) = foldl f (f z x) xs

flip            :: (a -> b -> c) -> b -> a -> c
flip  f x y     = f y x


consFunc :: a -> List a -> List a
consFunc x xs = Cons x xs