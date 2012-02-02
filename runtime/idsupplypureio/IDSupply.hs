-- ---------------------------------------------------------------------------
-- | IDSupply implementation using only IORefs
-- ---------------------------------------------------------------------------
module IDSupply
  ( IDSupply, initSupply, leftSupply, rightSupply, unique
  , Unique, mkInteger, showUnique
  , getDecisionRaw, setDecisionRaw, unsetDecisionRaw
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import GHC.IO  (unsafeDupableInterleaveIO)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Decision, defaultDecision)

newtype Unique = Unique { unqRef :: IORef Decision } deriving Eq

data IDSupply = IDSupply
  { unique      :: Unique   -- ^ Decision and unique identifier for this IDSupply
  , leftSupply  :: IDSupply -- ^ path to the left IDSupply
  , rightSupply :: IDSupply -- ^ path to the right IDSupply
  }

instance Eq IDSupply where
  s1 == s2 = unique s1 == unique s2

instance Show IDSupply where
  show = showUnique . unique

-- |Retrieve an 'Integer' representation of the unique identifier
mkInteger :: Unique -> Integer
mkInteger _ =
 error "idsupply pureio: no integer representation available, use other supply!"

showUnique :: Unique -> String
showUnique _ = ""

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
  r  <- unsafeDupableInterleaveIO $ newIORef defaultDecision
  return (IDSupply (Unique r) s1 s2)
{-# NOINLINE getPureSupply #-}

getDecisionRaw :: Unique -> IO Decision
getDecisionRaw u = readIORef (unqRef u)

setDecisionRaw :: Unique -> Decision -> IO ()
setDecisionRaw u c = writeIORef (unqRef u) c

unsetDecisionRaw :: Unique -> IO ()
unsetDecisionRaw u = writeIORef (unqRef u) defaultDecision
