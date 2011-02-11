module DetTest where


data MyList a = Cons a (MyList a) | Nil

data MyBool = True | False


not True = False
not False = True


append Nil xs = xs
append (Cons x xs) ys = Cons x (append xs ys)


reverse Nil = Nil
reverse (Cons x xs) = append (reverse xs) (Cons x Nil)

goal0 = reverse (Cons True (Cons False (Cons False Nil)))
