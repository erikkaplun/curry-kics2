module ID(Choice(..), ID(..), IDSupply,
          initSupply, leftSupply, rightSupply, thisID, freeID,
          lookupChoice, setChoice)
 where

import Data.IORef
import System.IO.Unsafe

-- Type to encode the selection taken in a Choice structure
data Choice
  = NoChoice
  | ChooseLeft
  | ChooseRight
  | BindTo ID
  | BoundTo ID
  deriving Show

-- Type to identify different Choice structures in a non-deterministic result.
-- Here we implement it as IO references
data ID = ID (IORef Choice)
        | FreeID (IORef Choice)

instance Show ID where 
  show (ID _)       = "ID" 
  show (FreeID _)   = "Free"

data IDSupply = IDSupply ID IDSupply IDSupply

initSupply :: IO IDSupply
initSupply = getPureSupply NoChoice

{-# NOINLINE getPureSupply #-}
getPureSupply :: Choice -> IO IDSupply
getPureSupply def = do
    s1 <- unsafeInterleaveIO (getPureSupply def)
    s2 <- unsafeInterleaveIO (getPureSupply def)
    n  <- unsafeInterleaveIO  (newIORef def)
    return (IDSupply (ID n) s1 s2)

leftSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply _ s _) = s

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply _ _ s) = s

thisID :: IDSupply -> ID
thisID (IDSupply i _ _) = i

freeID :: IDSupply -> ID
freeID (IDSupply (ID i) _ _) = FreeID i

-----------------------
-- Managing choices
-----------------------

lookupChoice :: ID -> IO Choice
lookupChoice (ID r) = readIORef r

setChoice :: ID -> Choice -> IO ()
setChoice (ID r) c = writeIORef r c

