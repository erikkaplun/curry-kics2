module Basics where

import ID

data Try a = Val a | Choice ID a a | Free ID a a | Guard Constraint a
  deriving Show

tryChoice :: ID -> a -> a -> Try a
tryChoice i@(ID _)     = Choice i
tryChoice i@(FreeID _) = Free i

narrow (FreeID i) = ID i

data Choice = NoChoice | ChooseLeft | ChooseRight | BindTo ID
 deriving Show

data Constraint = ID :=: Choice
 deriving Show

class NonDet a where
  choiceCons :: ID -> a -> a -> a
  failCons   :: a
  guardCons  :: Constraint -> a -> a
  try        :: a -> Try a


class NonDet a => Generable a where
  generate :: ID -> a

-- Auxiliaries for Show
showsChoice :: Show a => Int -> ID -> a -> a -> ShowS
showsChoice d i@(FreeID _) _ _ = shows i
showsChoice d r x1 x2 = 
  showChar '(' . 
  showsPrec d x1 .
  showString " ?" . shows r .
  showsPrec d x2 .
  showChar ')' 
