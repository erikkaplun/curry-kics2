module ID(Choice(..), ID(..), IDSupply,
          initSupply, leftSupply, rightSupply, thisID, freeID,
          lookupChoice, setChoice, leftID, rightID, narrowID)
 where

import Data.IORef
import qualified Data.Map 
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
-- Here we implement it as integer values.
data ID
  = ID Integer
  | FreeID IDSupply
    deriving (Eq, Ord)

instance Show ID where
  show (ID i)       = show i --"ID"
  show (FreeID i)   = "Free" ++ show i

newtype IDSupply = IDSupply Integer
  deriving (Eq,Ord)

instance Show IDSupply where
  show (IDSupply i) = show i

initSupply :: IO IDSupply
initSupply = return (IDSupply 1)

leftSupply :: IDSupply -> IDSupply
leftSupply (IDSupply i) = IDSupply (2 * i)

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply i) = IDSupply (2 * i + 1)

thisID :: IDSupply -> ID
thisID (IDSupply i) = ID i

freeID :: IDSupply -> ID
freeID s = FreeID s

leftID, rightID :: ID -> ID
leftID  (FreeID s) = freeID (leftSupply s)
rightID (FreeID s) = freeID (rightSupply s)

narrowID :: ID -> ID
narrowID (FreeID s) = thisID s
narrowID i          = i
-----------------------
-- Managing choices
-----------------------

type SetOfChoices = Data.Map.Map Integer Choice

store :: IORef SetOfChoices
store = unsafePerformIO (newIORef Data.Map.empty)

lookupChoice :: ID -> IO Choice
lookupChoice (ID r) = lookupRef r
lookupChoice (FreeID (IDSupply r)) = lookupRef r

lookupRef :: Integer -> IO Choice
lookupRef r = do
  st <- readIORef store
  return $ maybe NoChoice id (Data.Map.lookup r st)

setChoice :: ID -> Choice -> IO ()
setChoice (ID r) c = do
  st <- readIORef store 
  writeIORef store $ case c of 
    NoChoice -> Data.Map.delete r st
    _        -> Data.Map.insert r c st


