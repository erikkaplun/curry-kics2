{- ---------------------------------------------------------------------------
BUG
Status: Solved
Description: calls to deterministic functions inside nondeterministic
  functions are not wrapped by wrapD
--------------------------------------------------------------------------- -}
module Bug1 where

-- missing wrapD
coin = id ? id

{-
module Curry_Bug1 (c_C_id, c_C_coin) where

import ID
import Basics
import Curry_Prelude

c_C_coin :: IDSupply -> Func t0 t0
c_C_coin x3000 = let
     x2000 = x3000
      in (c_OP_qmark c_C_id c_C_id x2000)
      -- should be: (c_OP_qmark (wrapD c_C_id) (wrapD c_C_id) x2000
-}
