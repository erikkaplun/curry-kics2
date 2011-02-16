{- ---------------------------------------------------------------------------
BUG
Status: Solved
Description: Higher-order-functions inside other data structures are
  not converted to Funcs in the type signature of the non-deterministic
  variant.
--------------------------------------------------------------------------- -}
module Bug2 where

data Wrap a = Wrap a

appWrap :: Wrap (Success -> Success) -> Success -> Success
appWrap (Wrap f) a = f a

{-
d_C_appWrap :: C_Wrap (C_Success -> C_Success) -> C_Success -> C_Success
d_C_appWrap x1 x2 = case x1 of
     (C_Wrap x3) -> d_C_apply x3 x2
     (Choice_C_Wrap x1000 x1001 x1002) -> narrow x1000 (d_C_appWrap x1001 x2) (d_C_appWrap x1002 x2)
     (Guard_C_Wrap x1000 x1001) -> guardCons x1000 (d_C_appWrap x1001 x2)
     _ -> failCons

-- should be: c_C_appWrap :: C_Wrap (Func C_Success C_Success) -> C_Success -> IDSupply -> C_Success
c_C_appWrap :: C_Wrap (C_Success -> C_Success) -> C_Success -> IDSupply -> C_Success
c_C_appWrap x1 x2 x3000 = case x1 of
     (C_Wrap x3) -> let
          x2000 = x3000
           in (c_C_apply x3 x2 x2000)
     (Choice_C_Wrap x1000 x1001 x1002) -> narrow x1000 (c_C_appWrap x1001 x2 x3000) (c_C_appWrap x1002 x2 x3000)
     (Guard_C_Wrap x1000 x1001) -> guardCons x1000 (c_C_appWrap x1001 x2 x3000)
     _ -> failCons
-}