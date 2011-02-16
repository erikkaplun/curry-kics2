{- ---------------------------------------------------------------------------
BUG
Status: Open
Description: unclear, but does not compile
--------------------------------------------------------------------------- -}
module Bug3 where

data Wrap a = Wrap (a -> a)

appWrap :: Wrap (Success) -> Success -> Success
appWrap (Wrap f) a = f a
