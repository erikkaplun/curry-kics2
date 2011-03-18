module ID
  ( Choice (..), ID (..), IDSupply
  , mkInt, leftID, rightID, narrowID
  , initSupply, leftSupply, rightSupply, thisID, freeID
  , lookupChoice, setChoice, setUnsetChoice
  , store
  ) where

import Data.IORef
import qualified Data.Map
import System.IO.Unsafe

-- Type to encode the selection taken in a Choice structure
data Choice
  = NoChoice
  | ChooseLeft Int
  | ChooseRight Int
  | BindTo ID
  | BoundTo ID Int  -- ID is the ID of the variable the free variable is bound
                    -- to, Int is the number of child IDs to which the binding
                    -- has been propagated.
  deriving (Eq,Show)

-- Type to identify different Choice structures in a non-deterministic result.
-- Here we implement it as integer values.
data ID
  = ID Integer
  | FreeID IDSupply
--   | IDFree IDSupply Int
    deriving (Eq, Ord)

instance Show ID where
  show (ID i)       = show i
  show (FreeID i)   = "Free" ++ show i
--   show (IDFree i _) = "_" ++ show i

-- Conversion of ID into integer (for monadic search operators).
mkInt :: ID -> Integer
mkInt (ID i) = i
mkInt (FreeID _) = error "IDSupplyInteger.mkInt: FreeID occurred?"

leftID, rightID :: ID -> ID
leftID  (FreeID s) = freeID (leftSupply s)
rightID (FreeID s) = freeID (rightSupply s)

narrowID :: ID -> ID
narrowID (FreeID s)   = thisID s
-- narrowID (IDFree s _) = thisID s
narrowID i@(ID _)     = i

rawID :: ID -> Integer
rawID (ID i) = i
rawID (FreeID (IDSupply i)) = i
-- rawID (IDFree i _)          = i

-- ---------------------
-- ID Supply
-- ---------------------

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

thisRawID :: IDSupply -> Integer
thisRawID = rawID . thisID

-- -- TODO: Strict evaluation?
-- mkIDFree :: IDSupply -> Int -> ID
-- mkIDFree s numOfUsedIds = IDFree s numOfUsedIds

-- ---------------------
-- Managing choices
-- ---------------------

type SetOfChoices = Data.Map.Map Integer Choice

store :: IORef SetOfChoices
store = unsafePerformIO (newIORef Data.Map.empty)

lookupChoiceRaw :: ID -> IO Choice
lookupChoiceRaw (ID r) = lookupRef r
lookupChoiceRaw (FreeID (IDSupply r)) = lookupRef r
-- lookupChoiceRaw (IDFree (IDSupply r) _) = lookupRef r

lookupRef :: Integer -> IO Choice
lookupRef r = do
  st <- readIORef store
  return $ Data.Map.findWithDefault NoChoice r st

-- lookupChoice x@(IDFree xs xnum) = lookupChoiceRaw x >>= unchain
--   where
--     unchain (BoundTo y _) = lookupChoice y
--
--     unchain (BindTo y@(IDFree ys ynum)) = do
--       c <- lookupChoice y
--       -- xnum == ynum && xnum >= 0 ?
--       setChoice x (BoundTo y (xnum ? ynum))
--
--       sequence_ $ zipWith (\x' y' -> setChoice x' (BindTo y'))
--                           (nextNIDs xs xnum) (nextNIDs ys ynum)
--
--
--     unchain c           = return c
lookupChoice :: ID -> IO Choice
lookupChoice i = lookupChoiceRaw i >>= unchain
  where
    unchain (BoundTo j _) = lookupChoice j

    unchain (BindTo j)  = do
      c <- lookupChoice j
      case c of
        NoChoice        -> return ()
        ChooseLeft num  -> propagate i j num
        ChooseRight num -> propagate i j num
        errChoice       -> error $ "ID.lookupChoice returned " ++ show errChoice
      return c

    unchain c           = return c

    propagate x@(FreeID xs) y@(FreeID ys) cnt = do
      setChoice x (BoundTo y cnt)
      sequence_ $ zipWith (\x' y' -> setChoice x' (BindTo y'))
                    (nextNIDs xs cnt) (nextNIDs ys cnt)

-- Compute a list of the next n free IDs for a given IDSupply s
nextNIDs :: IDSupply -> Int -> [ID]
nextNIDs s n
  | n <  0    = error $ "ID.nextNIDs with negative number " ++ show n
  | n == 0    = []
  | n == 1    = [freeID (leftSupply s)]
  | otherwise = nextNIDs' s n
  where
    nextNIDs' s n
      | n == 0    = []
      | n == 1    = [freeID s]
      | otherwise = nextNIDs' (leftSupply s) (n - halfn) ++ nextNIDs' (rightSupply s) halfn
        where halfn = n `div` 2

-- resetFreeVar (IDFree i _) = lookupRef i >>= propagate
--   where
--     propagate (BindTo _)  = setChoiceRaw i NoChoice
--     propagate (BoundTo j is) = do
--       setChoiceRaw i NoChoice
--       mapM_ (resetFreeVar . ID) is
resetFreeVar :: ID -> IO ()
resetFreeVar i@(FreeID s) = lookupChoiceRaw i >>= propagate
  where
    propagate (BindTo _)  = setChoice i NoChoice
    propagate (BoundTo _ num) = do
      setChoice i NoChoice
      mapM_ resetFreeVar $ nextNIDs s num

setChoice :: ID -> Choice -> IO ()
setChoice i = setChoiceRaw (rawID i)

setChoiceRaw :: Integer -> Choice -> IO ()
setChoiceRaw i c = do
  st <- readIORef store
  writeIORef store $ case c of
    NoChoice -> Data.Map.delete i st
    _        -> Data.Map.insert i c st

setChoiceGetID :: ID -> Choice -> IO ID
setChoiceGetID i c = lookupChoiceRaw i >>= unchain
  where
    unchain (BindTo j)    = setChoiceGetID j c
    unchain (BoundTo j _) = setChoiceGetID j c
    unchain _             = setChoice i c >> return i

setUnsetChoice :: ID -> Choice -> IO (Maybe (IO ()))
setUnsetChoice i c = do
  j <- setChoiceGetID i c
  case c of
    BindTo _ -> return (Just (resetFreeVar j))
    _        -> return (Just (setChoice j NoChoice))
