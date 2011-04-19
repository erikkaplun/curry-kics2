data Success = Success

data Int
  = Int      IntPrim
  | CurryInt Integer
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