-- ---------------------------------------------------------------------------
-- | IDSupply implementation using Integer
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

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Choice, defaultChoice, isDefaultChoice)

type Unique = Integer

-- |References to 'Choice's are represented using 'Integer'
newtype IDSupply = IDSupply { unique :: Unique }
  deriving Eq

instance Show IDSupply where
  show (IDSupply i) = show i

-- |Retrieve an 'Integer' representation of the unique identifier
mkInteger :: IDSupply -> Integer
mkInteger = unique

leftSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply i) = IDSupply (2 * i)

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply i) = IDSupply (2 * i + 1)

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = return (IDSupply 1)

-- |Type class for a Choice 'Store'
class (Monad m) => Store m where
  -- |Get the stored 'Choice', defaulting to 'defaultChoice'
  getChoiceRaw    :: IDSupply -> m Choice
  -- |Set the 'Choice'
  setChoiceRaw    :: IDSupply -> Choice -> m ()
  -- |Unset the 'Choice'
  unsetChoiceRaw  :: IDSupply -> m ()

store :: IORef (Map.Map Unique Choice)
store = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE store #-}

instance Store IO where
  getChoiceRaw s        = Map.findWithDefault defaultChoice (unique s)
                          `liftM` readIORef store
  setChoiceRaw    s c
    | isDefaultChoice c = modifyIORef store $ Map.delete (unique s)
    | otherwise         = modifyIORef store $ Map.insert (unique s) c
  unsetChoiceRaw  s     = modifyIORef store $ Map.delete (unique s)
