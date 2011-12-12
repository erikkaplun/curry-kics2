-- ---------------------------------------------------------------------------
-- | IDSupply implementation using Integer
-- ---------------------------------------------------------------------------
module IDSupply
  ( IDSupply, initSupply, leftSupply, rightSupply, unique
  , Unique, mkInteger, showUnique
  , Store (..)
  ) where

import Control.Monad (liftM)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as Map (Map, empty, delete, findWithDefault, insert)
import System.IO.Unsafe (unsafePerformIO)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Decision, defaultDecision, isDefaultDecision)

type Unique = Integer

-- |References to 'Decision's are represented using 'Integer'
newtype IDSupply = IDSupply { unique :: Unique }
  deriving Eq

instance Show IDSupply where
  show (IDSupply i) = show i

-- |Retrieve an 'Integer' representation of the unique identifier
mkInteger :: Unique -> Integer
mkInteger = id

leftSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply i) = IDSupply (2 * i)

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply i) = IDSupply (2 * i + 1)

showUnique :: Unique -> String
showUnique = show

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = return (IDSupply 1)

-- |Type class for a Decision 'Store'
class (Monad m) => Store m where
  -- |Get the stored 'Decision', defaulting to 'defaultDecision'
  getDecisionRaw    :: Unique -> m Decision
  -- |Set the 'Decision'
  setDecisionRaw    :: Unique -> Decision -> m ()
  -- |Unset the 'Decision'
  unsetDecisionRaw  :: Unique -> m ()

store :: IORef (Map.Map Unique Decision)
store = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE store #-}

instance Store IO where
  getDecisionRaw u        = Map.findWithDefault defaultDecision u
                            `liftM` readIORef store
  setDecisionRaw u c
    | isDefaultDecision c = modifyIORef store $ Map.delete u
    | otherwise           = modifyIORef store $ Map.insert u c
  unsetDecisionRaw u      = modifyIORef store $ Map.delete u
