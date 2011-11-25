-- ---------------------------------------------------------------------------
-- | IDSupply implementation using GHC's UniqSupply
-- ---------------------------------------------------------------------------
module IDSupply
  ( IDSupply, initSupply, leftSupply, rightSupply, unique
  , Unique, mkInteger
  , Store (..)
  ) where

import Control.Monad (liftM)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as Map (Map, empty, delete, findWithDefault, insert)
import System.IO.Unsafe (unsafePerformIO)
import UniqSupply
  (UniqSupply, mkSplitUniqSupply, splitUniqSupply, uniqFromSupply)
import Unique (Unique, getKey)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Choice, defaultChoice, isDefaultChoice)

-- |References to 'Choice's are represented using GHC's 'UniqSupply'
newtype IDSupply = IDSupply { uniqSupply :: UniqSupply }

instance Eq IDSupply where
  s1 == s2 = unique s1 == unique s2

instance Show IDSupply where
  show = tail . show . unique -- tail to avoid showing of leading 'a'

-- |Retrieve an 'Integer' representation of the unique identifier
mkInteger :: IDSupply -> Integer
mkInteger = toInteger . getKey . unique

leftSupply :: IDSupply -> IDSupply
leftSupply = IDSupply . fst . splitUniqSupply . uniqSupply

rightSupply :: IDSupply -> IDSupply
rightSupply = IDSupply . snd . splitUniqSupply . uniqSupply

unique :: IDSupply -> Unique
unique = uniqFromSupply . uniqSupply

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = IDSupply `liftM` mkSplitUniqSupply 'a'

-- |Type class for a Choice 'Store'
class (Monad m) => Store m where
  -- |Get the stored 'Choice', defaulting to 'defaultChoice'
  getChoiceRaw    :: IDSupply -> m Choice
  -- |Set the 'Choice'
  setChoiceRaw    :: IDSupply -> Choice -> m ()
  -- |Unset the 'Choice'
  unsetChoiceRaw  :: IDSupply -> m ()

-- |Internal store for 'Choice's
store :: IORef (Map.Map Unique Choice)
store = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE store #-}

instance Store IO where
  getChoiceRaw s        = Map.findWithDefault defaultChoice (unique s)
                          `liftM` readIORef store
  setChoiceRaw s c
    | isDefaultChoice c = modifyIORef store $ Map.delete (unique s)
    | otherwise         = modifyIORef store $ Map.insert (unique s) c
  unsetChoiceRaw s      = modifyIORef store $ Map.delete (unique s)
