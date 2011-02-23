{-# LANGUAGE TypeOperators #-}

module Basics where

import ID

-- Type to encode the selection taken in a Choice structure
data Choice = NoChoice | ChooseLeft | ChooseRight | BindTo ID | BoundTo ID
  deriving Show

data Try a = Val a | Fail | Choice ID a a | Free ID a a | Guard Constraint a
  deriving Show

tryChoice :: ID -> a -> a -> Try a
tryChoice i@(ID _)     = Choice i
tryChoice i@(FreeID _) = Free i

-- Class for data that support nondeterministic values
class NonDet a where
  choiceCons :: ID -> a -> a -> a
  failCons   :: a
  guardCons  :: Constraint -> a -> a
  try        :: a -> Try a

narrow :: NonDet a => ID -> a -> a -> a
narrow (FreeID i) = choiceCons (ID i)
narrow i = choiceCons i

-- Class for data that support generators
class NonDet a => Generable a where
  generate :: IDSupply -> a

---------------------------------------------------------------------
-- Computations to normal form

-- Class for data that supports normal form computations.
-- The normal form computation is combined with a continuation to be
-- applied to the normal form.
class NonDet a => NormalForm a where
  ($!!) :: NonDet b => (a -> b) -> a -> b

-- Auxiliary operation to apply a continuation to the normal form.
($$!!) :: (NormalForm a, NonDet b) => (a -> b) -> a -> b
cont $$!! x = nf (try x)
  where
    nf (Val v)        = cont $!! v
    nf Fail           = failCons
    nf (Choice i x y) = choiceCons i (nf (try x)) (nf (try y))
    nf (Free i x y)   = cont (choiceCons i x y)
    nf (Guard c e)    = guardCons c (nf (try e))


---------------------------------------------------------------------
-- The implementation of the Success type must be added here since
-- it is used in the class Unifiable.

data C_Success = C_Success
               | Choice_C_Success ID C_Success C_Success
               | Fail_C_Success
               | Guard_C_Success Constraint C_Success

instance Show C_Success where
  showsPrec d C_Success = showString "success"
  showsPrec d (Choice_C_Success i x y) = showsChoice d i x y
  showsPrec d Fail_C_Success = showChar '!'

instance NonDet C_Success where
  choiceCons = Choice_C_Success
  failCons   = Fail_C_Success
  guardCons  = Guard_C_Success
  try (Choice_C_Success i x y) = tryChoice i x y
  try Fail_C_Success           = Fail
  try (Guard_C_Success c e)    = Guard c e
  try x = Val x

instance Generable C_Success where
  generate _ = C_Success

instance NormalForm C_Success where
  cont $!! s@C_Success = cont s
  cont $!! x = cont $$!! x

---------------------------------------------------------------------
-- Unification

-- Class for data that support unification
class (NonDet a, NormalForm a) => Unifiable a where
  (=.=) :: a -> a -> C_Success
  bind :: ID -> a -> [Constraint]

data Constraint = ID :=: Choice
 deriving Show

(=:=) :: Unifiable a => a -> a -> C_Success
_ =:= _ = error "(=:=) undefined"

(&) :: C_Success -> C_Success -> C_Success
_ & _ = error "(&) undefined"

---------------------------------------------------------------------
-- Auxiliaries for Show

showsChoice :: Show a => Int -> ID -> a -> a -> ShowS
showsChoice d i@(FreeID _) _ _ = shows i
showsChoice d r x1 x2 =
  showChar '(' .
  showsPrec d x1 .
  showString " ?" . shows r . showChar ' ' .
  showsPrec d x2 .
  showChar ')'

showsGuard :: (Show a, Show b) => Int -> a -> b -> ShowS
showsGuard d c e = showsPrec d c . showString " &> " . showsPrec d e

---------------------------------------------------------------------
-- Higher Order

instance NonDet (a -> b) where
  choiceCons = undefined
  failCons = undefined
  guardCons = undefined
  try = undefined

instance Generable (a -> b) where
  generate = undefined

data Func a b = Func (a -> IDSupply -> b)
              | Func_Choice ID (Func a b) (Func a b)
              | Func_Fail
              | Func_Guard Constraint (Func a b)

type a :-> b = Func a b

infixr 0 :->

instance NonDet (Func a b) where
  choiceCons = Func_Choice
  failCons = Func_Fail
  guardCons = Func_Guard
  try (Func_Choice i x1 x2) = Choice i x1 x2
  try (Func_Fail) = Fail
  try v = Val v

instance Generable (Func a b) where
  generate = undefined

-- make a deterministic function non-deterministic
nd :: (a -> b) -> a -> IDSupply -> b
nd f a _ = f a

wrapDX wrap f = wrapNX wrap (nd f)

wrapNX wrap f = Func (\a s -> wrap $ f a s)

-- wrap a higher-order function with one argument
wrapD :: (a -> b) -> a :-> b
wrapD f = wrapDX id f
-- Func (\a _ -> f a)

wrapD2 :: (a -> b -> c) -> a :-> b :-> c
wrapD2 f = wrapDX (wrapDX id) f

wrapD3 :: (a -> b -> c -> d) -> a :-> b :-> c :-> d
wrapD3 f = wrapDX (wrapDX (wrapDX id)) f

wrapN :: (a -> IDSupply -> b) -> Func a b
wrapN f = wrapNX id f

wrapN2 :: (a -> b -> IDSupply -> c) -> a :-> b :-> c
wrapN2 f = wrapDX (wrapNX id) f

wrapN3 :: (a -> b -> c -> IDSupply -> d) -> a :-> b :-> c :-> d
wrapN3 f = wrapDX (wrapDX (wrapNX id)) f



unwrap :: Func a b -> IDSupply -> a -> b
unwrap (Func f) s x = f x s

-- TODO: from Prelude

-- c_OP_qmark :: NonDet a => a -> a -> IDSupply -> a
-- c_OP_qmark x y ids = let i = thisID ids in i `seq` choiceCons i x y

-- c_C_apply :: Func a b -> a -> IDSupply -> b
-- c_C_apply (Func f) s x = f s x
--
-- d_C_apply :: (a -> b) -> a -> b
-- d_C_apply f x = f x

eval :: (IDSupply -> a) -> IO a
eval goal = initSupply >>= return . goal