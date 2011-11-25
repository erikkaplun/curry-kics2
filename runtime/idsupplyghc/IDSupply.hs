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

-- |Retrieve an 'Unique' representation of the unique identifier
mkInteger :: Unique -> Integer
mkInteger = toInteger . getKey

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
  getChoiceRaw    :: Unique -> m Choice
  -- |Set the 'Choice'
  setChoiceRaw    :: Unique -> Choice -> m ()
  -- |Unset the 'Choice'
  unsetChoiceRaw  :: Unique -> m ()

-- |Internal store for 'Choice's
store :: IORef (Map.Map Unique Choice)
store = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE store #-}

instance Store IO where
  getChoiceRaw u        = Map.findWithDefault defaultChoice u
                          `liftM` readIORef store
  setChoiceRaw u c
    | isDefaultChoice c = modifyIORef store $ Map.delete u
    | otherwise         = modifyIORef store $ Map.insert u c
  unsetChoiceRaw u      = modifyIORef store $ Map.delete u
