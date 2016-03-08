-- Removed since Curry version 0.9.0
-- data Success = Success

data Int
  = Int      IntPrim
  | CurryInt BinInt
data IntPrim

data Float = Float FloatPrim
data FloatPrim

data Char = Char CharPrim
data CharPrim

data IO a = IO (IOPrim a)
data IOPrim _

data Func a b = Func (a -> IDSupply -> b)
data IDSupply

data PrimData a = PrimData a
