-- ---------------------------------------------------------------------------
-- | ID implementation using GHCs UniqSupply
-- ---------------------------------------------------------------------------

module IDImpl
  ( Ref, mkIntRef, IDSupply, initSupply, leftSupply, rightSupply, thisRef
  , lookupChoiceRef, setChoiceRef
  ) where

import UniqSupply(UniqSupply, mkSplitUniqSupply, splitUniqSupply, uniqFromSupply)
import Unique(Unique, getKey)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as Map (Map, empty, delete, insert, findWithDefault)
import System.IO.Unsafe (unsafePerformIO)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Choice, defaultChoice, isDefaultChoice)

-- |References to 'Choice's are represented as 'Integer's
type Ref = Unique

mkIntRef :: Ref -> Integer
mkIntRef = toInteger . getKey

-- ---------------------
-- ID Supply
-- ---------------------

newtype IDSupply = IDSupply UniqSupply

instance Eq IDSupply where
  s1 ==s2 = thisRef s1 == thisRef s2

instance Show IDSupply where
  show s = show (thisRef s)

initSupply :: IO IDSupply
initSupply = mkSplitUniqSupply 'a' >>= return . IDSupply

leftSupply :: IDSupply -> IDSupply
leftSupply (IDSupply s) = IDSupply (fst (splitUniqSupply s))

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply s) = IDSupply (snd (splitUniqSupply s))

thisRef :: IDSupply -> Ref
thisRef (IDSupply s) = uniqFromSupply s

-- ---------------------
-- Managing choices
-- ---------------------

type SetOfChoices = Map.Map Ref Choice

{-# NOINLINE store #-}
store :: IORef SetOfChoices
store = unsafePerformIO (newIORef Map.empty)

lookupChoiceRef :: Ref -> IO Choice
lookupChoiceRef r = do
  st <- readIORef store
  return $ Map.findWithDefault defaultChoice r st

setChoiceRef :: Ref -> Choice -> IO ()
setChoiceRef r c = modifyIORef store $ if isDefaultChoice c
  then Map.delete r
  else Map.insert r c