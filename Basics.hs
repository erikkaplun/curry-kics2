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

data Choice = NoChoice | ChooseLeft | ChooseRight | BindTo ID
 deriving Show

data Constraint = ID :=: Choice
 deriving Show

class NonDet a where
  choiceCons :: ID -> a -> a -> a
  failCons   :: a
  guardCons  :: Constraint -> a -> a
  try        :: a -> Try a


-- Class for data that support generators
class NonDet a => Generable a where
  generate :: ID -> a


-- Class for data that support unification
--class (NonDet a, NormalForm a) => Unifiable a where
class NonDet a => Unifiable a where
  (=.=) :: a -> a -> C_Success
  --bind :: ID -> a -> Constraint


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


---------------------------------------------------------------------
-- Unification

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
-- Higher Order
data Func a b = Func (a -> IDSupply -> b)
              | Func_Choice ID (Func a b) (Func a b)
              | Func_Fail
              | Func_Guard Constraint (Func a b)

instance NonDet (Func a b) where 
  choiceCons = Func_Choice
  failCons = Func_Fail
  guardCons = Func_Guard
  try (Func_Choice i x1 x2) = Choice i x1 x2
  try (Func_Fail) = Fail
  try v = Val v




wrapD :: (a -> b) -> Func a b
wrapD f = Func (\ x s -> f x)

wrapN :: (a -> IDSupply -> b) -> Func a b
wrapN = Func 




---------------------------------------------------------------------