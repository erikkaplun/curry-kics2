-- ---------------------------------------------------------------------------
-- | ID implementation using IORefs
-- ---------------------------------------------------------------------------
module IDImpl
  ( Choice (..), ID (..), IDSupply
  , mkInt, initSupply, leftSupply, rightSupply, thisID
  , lookupChoiceRaw, setChoice
  ) where

import Data.IORef
import System.IO.Unsafe

-- Type to encode the selection taken in a Choice structure
data Choice
  = NoChoice        -- No choice has been made so far
  | ChooseLeft
  | ChooseRight
  | ChooseN Int Int -- ChooseN consIdx argCnt is the choice for the
                    -- constructor with the index consIdx which has argCnt
                    -- arguments
  | BindTo ID       -- a free or narrowed variable is bound to the
                    -- free variable with the given id; the bindings of the
                    -- IDs for the arguments have not been propagated yet
  | BoundTo ID Int  -- a free or narrowed variable is bound to the variable
                    -- with the given ID; the bindings for the n arguments
                    -- have also been propagated
  deriving (Eq, Show)

-- Type to identify different Choice structures in a non-deterministic result.
-- Here we implement it as IO references
data ID
  = ID Ref
  | FreeID IDSupply
  | Narrowed IDSupply
    deriving Eq

type Ref = IORef Choice

instance Show ID where
  show (ID _)       = "ID"
  show (FreeID _)   = "Free"
  show (Narrowed _) = "Narrowed"

-- Conversion of ID into integer not possible for this implementation
mkInt :: ID -> Integer
mkInt = error "IDSupplyIORef.mkInt"

ref :: ID -> Ref
ref (ID r) = r
ref (FreeID (IDSupply r _ _)) = r
ref (Narrowed (IDSupply r _ _)) = r

-- ---------------------
-- ID Supply
-- ---------------------

data IDSupply = IDSupply Ref IDSupply IDSupply

instance Eq IDSupply where
  IDSupply i _ _ == IDSupply j _ _ = i == j

initSupply :: IO IDSupply
initSupply = getPureSupply NoChoice

{-# NOINLINE getPureSupply #-}
getPureSupply :: Choice -> IO IDSupply
getPureSupply def = do
  s1 <- unsafeInterleaveIO (getPureSupply def)
  s2 <- unsafeInterleaveIO (getPureSupply def)
  r  <- unsafeInterleaveIO (newIORef def)
  return (IDSupply r s1 s2)

leftSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply _ s _) = s

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply _ _ s) = s

thisID :: IDSupply -> ID
thisID (IDSupply r _ _) = ID r

-- ---------------------
-- Managing choices
-- ---------------------

lookupChoiceRaw :: ID -> IO Choice
lookupChoiceRaw i = readIORef (ref i)

setChoice :: ID -> Choice -> IO ()
setChoice i c = writeIORef (ref i) c
