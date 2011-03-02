{-# LANGUAGE TypeOperators #-}

module Basics where

import ID

-- Type to encode the selection taken in a Choice structure
data Choice
  = NoChoice
  | ChooseLeft
  | ChooseRight
  | BindTo ID
  | BoundTo ID
  deriving Show

data Constraint = ID :=: Choice
 deriving Show

data Try a
  = Val a
  | Fail
  | Choice ID a a
  | Free ID a a
  | Guard Constraint a
  deriving Show

tryChoice :: ID -> a -> a -> Try a
tryChoice i@(ID _)     = Choice i
tryChoice i@(FreeID _) = Free i

-- ---------------------------------------------------------------------------
-- Non-determinism
-- ---------------------------------------------------------------------------

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

-- ---------------------------------------------------------------------------
-- Computations to normal form
-- ---------------------------------------------------------------------------

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
    nf (Free i x y)   = error "($$!!) with free variable" -- was: cont (choiceCons i x y)
    nf (Guard c e)    = guardCons c (nf (try e))

nfChoice :: (NormalForm a, NonDet b) => (a -> b) -> ID -> a -> a -> b
nfChoice cont i x1 x2 = choiceCons i (cont $!! x1) (cont $!! x2)

-- ---------------------------------------------------------------------------
-- Unification
-- ---------------------------------------------------------------------------

-- Class for data that support unification
class (NonDet a, NormalForm a) => Unifiable a where
  (=.=) :: a -> a -> C_Success
  bind :: ID -> a -> [Constraint]

(=:=) :: Unifiable a => a -> a -> C_Success
_ =:= _ = error "(=:=) undefined"

(&) :: C_Success -> C_Success -> C_Success
_ & _ = error "(&) undefined"

-- ---------------------------------------------------------------------------
-- Built-in types
-- ---------------------------------------------------------------------------

-- The implementation of the Success type must be added here since it is used
-- in the class Unifiable.
data C_Success
  = C_Success
  | Choice_C_Success ID C_Success C_Success
  | Fail_C_Success
  | Guard_C_Success Constraint C_Success

instance Eq C_Success where
  C_Success == C_Success = True
  _         == _         = False

instance Ord C_Success where
  C_Success <= C_Success = True
  _         <= _         = False

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

instance Unifiable C_Success where
  C_Success =.= C_Success = C_Success
  _         =.= _         = Fail_C_Success
  bind i (Choice_C_Success j@(FreeID _) _ _) = [(i :=: (BindTo j))]

-- Higher Order Funcs

data Func a b = Func (a -> IDSupply -> b)
              | Func_Choice ID (Func a b) (Func a b)
              | Func_Fail
              | Func_Guard Constraint (Func a b)

instance Eq (Func a b) where
  f == g = False

instance Ord (Func a b) where
  f <= g = False

instance Show (Func a b) where
  show = error "show for Func is undefined"

instance NonDet (Func a b) where
  choiceCons = Func_Choice
  failCons = Func_Fail
  guardCons = Func_Guard
  try (Func_Choice i x1 x2) = Choice i x1 x2
  try (Func_Fail) = Fail
  try v = Val v

instance Generable (Func a b) where
  generate = error "generate for Func is undefined"

instance NormalForm (Func a b) where
  cont $!! f@(Func _) = cont f
  cont $!! f          = cont $$!! f

instance Unifiable (Func a b) where
  (=.=) = error "(=.=) for Func is undefined"
  bind = error "bind for Func is undefined"

-- Higher Order functions

instance Eq (a -> b) where
  f == g = False

instance Ord (a -> b) where
  f <= g = False

instance Show (a -> b) where
  show = error "show for function is undefined"

instance NonDet (a -> b) where
  choiceCons = undefined
  failCons = undefined
  guardCons = undefined
  try = undefined

instance Generable (a -> b) where
  generate = undefined

instance NormalForm (a -> b) where
  cont $!! f = cont f

instance Unifiable (a -> b) where
  (=.=) = error "(=.=) for function is undefined"
  bind = error "bind for function is undefined"

-- ---------------------------------------------------------------------------
-- IO
-- ---------------------------------------------------------------------------

-- TODO: reason about IO and non-determinism

data C_IO a
     = Choice_C_IO ID (C_IO a) (C_IO a)
     | Fail_C_IO
     | Guard_C_IO Constraint (C_IO a)
     | C_IO (IO a)

instance Eq (C_IO a) where
  (==) = error "(==) for C_IO"

instance Ord (C_IO a) where
  (<=) = error "(<=) for C_IO"

instance Show (C_IO a) where
  show = error "show for C_IO"

instance NonDet (C_IO a) where
  choiceCons = Choice_C_IO
  failCons = Fail_C_IO
  guardCons = Guard_C_IO
  try (Choice_C_IO i x y) = tryChoice i x y
  try Fail_C_IO = Fail
  try (Guard_C_IO c e) = Guard c e
  try x = Val x

instance Generable (C_IO a) where
  generate _ = error "C_IO: generate"

instance NormalForm (C_IO a) where
  cont $!! io@(C_IO _) = cont io
  cont $!! x           = cont $$!! x

instance Unifiable (C_IO a) where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_IO j@(FreeID _) _ _) = [(i :=: (BindTo j))]

toIO :: C_IO a -> IO a
toIO (C_IO io) = io

fromIO :: IO a -> C_IO a
fromIO io = C_IO io

-- ---------------------------------------------------------------------------
-- Auxiliaries for Show
-- ---------------------------------------------------------------------------

showsChoice :: Show a => Int -> ID -> a -> a -> ShowS
-- showsChoice d i@(FreeID _) _ _ = shows i
showsChoice d r x1 x2 =
  showChar '(' .
  showsPrec d x1 .
  showString " ?" . shows r . showChar ' ' .
  showsPrec d x2 .
  showChar ')'

showsGuard :: (Show a, Show b) => Int -> a -> b -> ShowS
showsGuard d c e = showsPrec d c . showString " &> " . showsPrec d e

-- ---------------------------------------------------------------------------
-- Auxiliaries for non-determinism
-- ---------------------------------------------------------------------------

-- make a deterministic function non-deterministic
nd :: (a -> b) -> a -> IDSupply -> b
nd f a _ = f a

wrapDX :: (c -> b) -> (a -> c) -> Func a b
wrapDX wrap f = wrapNX wrap (nd f)

wrapNX :: (c -> b) -> (a -> IDSupply -> c) -> Func a b
wrapNX wrap f = Func (\a s -> wrap $ f a s)

eval :: (IDSupply -> a) -> IO a
eval goal = initSupply >>= return . goal

evalIO :: (IDSupply -> C_IO a) -> IO a
evalIO goal = initSupply >>= (\s -> let (C_IO act) = goal s in act)

{-
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
-}
