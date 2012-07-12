{- ---------------------------------------------------------------------------
BUG
Status: Solved
Description: calls to constructor functions should be wrapped using wrapD
--------------------------------------------------------------------------- -}
module Bug4 where

data List a = Nil | Cons a (List a)

consFunc :: a -> List a -> List a
consFunc x xs = Cons x xs

foldr :: (a -> b -> b) -> b -> List a -> b
foldr _ z Nil         = z
foldr f z (Cons x xs) = f x (foldr f z xs)

listid :: List a -> List a
listid xs = foldr Cons Nil xs

listid2 :: List a -> List a
listid2 xs = foldr consFunc Nil xs

{-

c_C_listid :: Generable t0 => C_List t0 -> IDSupply -> C_List t0
c_C_listid x1 x3000 = let
     x2000 = x3000
      in (c_C_foldr C_Cons C_Nil x1 x2000)

Curry_Bug4.hs:95:20:
    Couldn't match expected type `Func t0 (Func t1 t1)'
           against inferred type `t01 -> C_List t01 -> C_List t01'
    In the first argument of `c_C_foldr', namely `C_Cons'
    In the expression: (c_C_foldr C_Cons C_Nil x1 x2000)
    In the expression:
        let x2000 = x3000 in (c_C_foldr C_Cons C_Nil x1 x2000)

------------------------------------------------------------------------------

c_C_listid2 :: Generable t0 => C_List t0 -> IDSupply -> C_List t0
c_C_listid2 x1 x3000 = let
     x2000 = x3000
      in (c_C_foldr (wrapD (wrapD . c_C_consFunc)) C_Nil x1 x2000)

okay

-}
