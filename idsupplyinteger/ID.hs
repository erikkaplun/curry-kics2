module ID(Choice(..), ID(..), IDSupply,
          mkInt,initSupply, leftSupply, rightSupply, thisID, freeID,
          lookupChoice, setChoice, leftID, rightID, narrowID,
          setUnsetChoice)
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
  deriving (Eq,Show)

-- Type to identify different Choice structures in a non-deterministic result.
-- Here we implement it as integer values.
data ID
  = ID Integer
  | FreeID IDSupply
    deriving (Eq, Ord)

instance Show ID where
  show (ID i)       = show i --"ID"
  show (FreeID i)   = "Free" ++ show i

-- Conversion of ID into integer (for monadic search operators).
mkInt :: ID -> Integer
mkInt (ID i) = i
mkInt (FreeID _) = error "IDSupplyInteger.mkInt: FreeID occurred?" 

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

rawID :: ID -> Integer
rawID (ID i) = i
rawID (FreeID (IDSupply i)) = i

-----------------------
-- Managing choices
-----------------------

type SetOfChoices = Data.Map.Map Integer Choice

store :: IORef SetOfChoices
store = unsafePerformIO (newIORef Data.Map.empty)

lookupChoiceRaw :: ID -> IO Choice
lookupChoiceRaw (ID r) = lookupRef r
lookupChoiceRaw (FreeID (IDSupply r)) = lookupRef r

lookupRef :: Integer -> IO Choice
lookupRef r = do
  st <- readIORef store
  return $ maybe NoChoice id (Data.Map.lookup r st)

lookupChoice :: ID -> IO Choice
lookupChoice i = lookupChoiceRaw i >>= unchain
  where
    unchain (BoundTo j) = lookupChoice j

    unchain (BindTo j)  = do 
      setChoice i (BoundTo j)
      setChoice (leftID i) (BindTo (leftID j))
      setChoice (rightID i) (BindTo (rightID j))
      lookupChoice j
      
    unchain c           = return c

resetFreeVar :: ID -> IO ()
resetFreeVar i = lookupChoiceRaw i >>= propagate 
  where
    propagate (BindTo _)  = setChoice i NoChoice
    propagate (BoundTo _) = do
        let il = leftID i
            ir = rightID i
        setChoice i NoChoice
        resetFreeVar il
        resetFreeVar ir

setChoice :: ID -> Choice -> IO ()
setChoice i c = do
  st <- readIORef store 
  writeIORef store $ case c of 
    NoChoice -> Data.Map.delete (rawID i) st
    _        -> Data.Map.insert (rawID i) c st

setChoiceGetID :: ID -> Choice -> IO ID
setChoiceGetID i c = lookupChoiceRaw i >>= unchain
  where
    unchain (BindTo j)  = setChoiceGetID j c --error "assumption violated" 
    unchain (BoundTo j) = setChoiceGetID j c
    unchain _           = setChoice i c >> return i



setUnsetChoice :: ID -> Choice -> IO (Maybe (IO ()))
setUnsetChoice i c = do j <- setChoiceGetID i c
                        case c of
                         BindTo _ -> return (Just (resetFreeVar j))
                         _        -> return (Just (setChoice j NoChoice))


