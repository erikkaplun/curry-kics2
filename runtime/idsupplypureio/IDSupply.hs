-- ---------------------------------------------------------------------------
-- | IDSupply implementation using only IORefs
-- ---------------------------------------------------------------------------
module IDSupply
  ( IDSupply, initSupply, leftSupply, rightSupply, unique
  , Unique, mkInteger
  , Store (..)
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO  (unsafeDupableInterleaveIO)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Choice, defaultChoice)

newtype Unique = Unique { unqRef :: IORef Choice } deriving Eq

instance Show Unique where
  show _ = ""

data IDSupply = IDSupply
  { unique      :: Unique   -- ^ Choice and unique identifier for this IDSupply
  , leftSupply  :: IDSupply -- ^ path to the left IDSupply
  , rightSupply :: IDSupply -- ^ path to the right IDSupply
  }

instance Eq IDSupply where
  s1 == s2 = unique s1 == unique s2

instance Show IDSupply where
  show = show . unique

-- |Retrieve an 'Integer' representation of the unique identifier
mkInteger :: Unique -> Integer
mkInteger _ = 0

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = getPureSupply

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
getPureSupply :: IO IDSupply
getPureSupply = do
  s1 <- unsafeDupableInterleaveIO getPureSupply
  s2 <- unsafeDupableInterleaveIO getPureSupply
  r  <- unsafeDupableInterleaveIO $ newIORef defaultChoice
  return (IDSupply (Unique r) s1 s2)
{-# NOINLINE getPureSupply #-}

-- |Type class for a Choice 'Store'
class (Monad m) => Store m where
  -- |Get the stored 'Choice', defaulting to 'defaultChoice'
  getChoiceRaw    :: Unique -> m Choice
  -- |Set the 'Choice'
  setChoiceRaw    :: Unique -> Choice -> m ()
  -- |Unset the 'Choice'
  unsetChoiceRaw  :: Unique -> m ()

instance Store IO where
  getChoiceRaw    u   = readIORef  (unqRef u)
  setChoiceRaw    u c = writeIORef (unqRef u) c
  unsetChoiceRaw  u   = writeIORef (unqRef u) defaultChoice
