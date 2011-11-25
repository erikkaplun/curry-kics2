-- ---------------------------------------------------------------------------
-- | IDSupply implementation using IORefs and GHC's UniqSupply
-- ---------------------------------------------------------------------------
module IDSupply
  ( IDSupply, initSupply, leftSupply, rightSupply, unique
  , Unique, mkInteger
  , Store (..)
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO  (unsafeDupableInterleaveIO)
import UniqSupply
  (UniqSupply, mkSplitUniqSupply, splitUniqSupply, uniqFromSupply)
import Unique (Unique, getKey)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Choice, defaultChoice)

data IDSupply = IDSupply
  { ref         :: IORef Choice -- ^ Choice for this IDSupply
  , unique      :: Unique       -- ^ unique identifier
  , leftSupply  :: IDSupply     -- ^ path to the left IDSupply
  , rightSupply :: IDSupply     -- ^ path to the right IDSupply
  }

instance Eq IDSupply where
  s1 == s2 = unique s1 == unique s2

instance Show IDSupply where
  show = tail . show . unique -- tail to avoid showing of leading 'a'

-- |Retrieve an 'Integer' representation of the unique identifier
mkInteger :: IDSupply -> Integer
mkInteger = toInteger . getKey . unique

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = mkSplitUniqSupply 'a' >>= getPureSupply

-- |Internal construction of an 'IDSupply' using 'unsafeDupableInterleaveIO'
-- to enable a lazy construction of the child 'IDSupply's inside the 'IO'
-- monad.
-- Without this unsafe function, the construction would loop infinitely.
--
-- /Note:/ Previously, this was implemented using 'unsafePerformIO', but
-- as 'unsafePerformIO' traverse the entire call stack to perform blackholing
-- this resulted in a very bad performance.
--
-- For more information, see
-- <http://www.haskell.org/pipermail/glasgow-haskell-users/2011-March/020223.html>
getPureSupply :: UniqSupply -> IO IDSupply
getPureSupply uniqS = do
  let (leftS, rightS) = splitUniqSupply uniqS
  s1 <- unsafeDupableInterleaveIO $ getPureSupply leftS
  s2 <- unsafeDupableInterleaveIO $ getPureSupply rightS
  r  <- unsafeDupableInterleaveIO $ newIORef defaultChoice
  return (IDSupply r (uniqFromSupply uniqS) s1 s2)
{-# NOINLINE getPureSupply #-}

-- |Type class for a Choice 'Store'
class (Monad m) => Store m where
  -- |Get the stored 'Choice', defaulting to 'defaultChoice'
  getChoiceRaw    :: IDSupply -> m Choice
  -- |Set the 'Choice'
  setChoiceRaw    :: IDSupply -> Choice -> m ()
  -- |Unset the 'Choice'
  unsetChoiceRaw  :: IDSupply -> m ()

instance Store IO where
  getChoiceRaw    s   = readIORef  (ref s)
  setChoiceRaw    s c = writeIORef (ref s) c
  unsetChoiceRaw  s   = writeIORef (ref s) defaultChoice
