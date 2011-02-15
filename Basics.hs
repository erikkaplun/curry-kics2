module Basics where

import ID

data Try a = Val a | Fail | Choice ID a a | Free ID a a | Guard Constraint a
  deriving Show

tryChoice :: ID -> a -> a -> Try a
tryChoice i@(ID _)     = Choice i
tryChoice i@(FreeID _) = Free i

narrow :: NonDet a => ID -> a -> a -> a
narrow (FreeID i) = choiceCons (ID i)
narrow i = choiceCons i

-- Class for data that support nondeterministic values
class NonDet a where
  choiceCons :: ID -> a -> a -> a
  failCons   :: a
  guardCons  :: Constraint -> a -> a
  try        :: a -> Try a

-- Type to encode the selection taken in a Choice structure
data Choice = NoChoice | ChooseLeft | ChooseRight | BindTo ID | BoundTo ID
 deriving Show


-- Class for data that support generators
class NonDet a => Generable a where
  generate :: ID -> a


---------------------------------------------------------------------
-- Computations to normal form

-- Class for data that supports normal form computations.
-- The normal form computation is cmbined with a continuation to be
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

instance NonDet C_Success where
  choiceCons = Choice_C_Success
  failCons   = Fail_C_Success
  guardCons  = Guard_C_Success

  try (Choice_C_Success i x y) = tryChoice i x y
  try Fail_C_Success           = Fail
  try (Guard_C_Success c e)    = Guard c e
  try x = Val x

instance Show C_Success where
  showsPrec d C_Success = showString "success"
  showsPrec d (Choice_C_Success i x y) = showsChoice d i x y
  showsPrec d Fail_C_Success = showChar '!'

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
  showString " ?" . shows r .
  showsPrec d x2 .
  showChar ')' 

showsGuard :: (Show a, Show b) => Int -> a -> b -> ShowS
showsGuard d c e = showsPrec d c . showString " &> " . showsPrec d e

---------------------------------------------------------------------