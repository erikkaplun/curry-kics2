module NatList where

data Nat = O | S Nat

add O n = n
add (S x) y = S (add x y)

double x = add x x

mult O _ = O
mult (S x) y = add y (mult x y)

two = S (S O)
four = double two
nat16 = mult four four
nat256 = mult nat16 nat16
nat4096 = mult nat256 nat16
nat16384 = mult nat4096 four

data MyList a = Cons a (MyList a) | Nil

data MyBool = True | False

not True = False
not False = True


append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)


reverse Nil = Nil
reverse (Cons x xs) = append (reverse xs) (Cons x Nil)

natList O = Nil
natList (S x) = Cons (S x) (natList x)

isList Nil = True
isList (Cons _ xs) = isList xs

goal0 = reverse (Cons True (Cons False (Cons False Nil)))

goal1 = reverse (natList nat16)
goal2 = reverse (natList nat256)
goal3 = isList (reverse (natList nat4096))
goal4 = isList (reverse (natList nat16384)) -- 134.242.305 rev. steps

main = goal4
