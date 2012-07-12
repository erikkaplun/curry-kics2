{-# LANGUAGE MagicHash #-}

import GHC.Exts ((-#), (<=#))

external_d_C_minus :: C_Int -> C_Int -> C_Int
external_d_C_minus (C_Int x) (C_Int y) = C_Int (x -# y)
external_d_C_minus x         y         = x `d_OP_minus` y

external_d_C_leq :: C_Int -> C_Int -> C_Bool
external_d_C_leq (C_Int x) (C_Int y) = if (x <=# y) then C_True else C_False
external_d_C_leq x         y         = x `d_OP_lt_eq` y
