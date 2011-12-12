-- ---------------------------------------------------------------------------
-- | IDSupply implementation using GHC's UniqSupply
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
import UniqSupply
  (UniqSupply, mkSplitUniqSupply, splitUniqSupply, uniqFromSupply)
import Unique (Unique, getKey)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Decision, defaultDecision, isDefaultDecision)

-- |References to 'Decision's are represented using GHC's 'UniqSupply'
newtype IDSupply = IDSupply { uniqSupply :: UniqSupply }

instance Eq IDSupply where
  s1 == s2 = unique s1 == unique s2

instance Show IDSupply where
  show = showUnique . unique

-- |Retrieve an 'Unique' representation of the unique identifier
mkInteger :: Unique -> Integer
mkInteger = toInteger . getKey

leftSupply :: IDSupply -> IDSupply
leftSupply = IDSupply . fst . splitUniqSupply . uniqSupply

rightSupply :: IDSupply -> IDSupply
rightSupply = IDSupply . snd . splitUniqSupply . uniqSupply

unique :: IDSupply -> Unique
unique = uniqFromSupply . uniqSupply

showUnique :: Unique -> String
showUnique = tail . show -- tail to avoid showing of leading 'a'

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = IDSupply `liftM` mkSplitUniqSupply 'a'

-- |Type class for a Decision 'Store'
class (Monad m) => Store m where
  -- |Get the stored 'Decision', defaulting to 'defaultDecision'
  getDecisionRaw    :: Unique -> m Decision
  -- |Set the 'Decision'
  setDecisionRaw    :: Unique -> Decision -> m ()
  -- |Unset the 'Decision'
  unsetDecisionRaw  :: Unique -> m ()

-- |Internal store for 'Decision's
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
