module HOTest (mymap) where

data Nat = O | S Nat

add O n = n
add (S x) y = S (add x y)

double x = add x x

mult O _ = O
mult (S x) y = add y (mult x y)

two = S (S O)
four = double two
nat16 = mult four four

data MyList a = Cons a (MyList a) | Nil

natList O = Nil
natList (S x) = Cons (S x) (natList x)

isList Nil = True
isList (Cons _ xs) = isList xs

goal1 = natList nat16

mymap :: (a -> b) -> MyList a -> MyList b
mymap _ Nil         = Nil
mymap f (Cons x xs) = Cons (f x) (mymap f xs)

main = mymap (add two) goal1
