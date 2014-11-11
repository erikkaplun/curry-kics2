-- ---------------------------------------------------------------------------
-- | IDSupply implementation using Integer
-- ---------------------------------------------------------------------------
module IDSupply
  ( IDSupply, initSupply, leftSupply, rightSupply, unique
  , Unique, mkInteger, showUnique
  , getDecisionRaw, setDecisionRaw, unsetDecisionRaw
  ) where

import           Control.Monad    (liftM)
import           Data.IORef       (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as Map  (Map, empty, delete, findWithDefault, insert)
import           System.IO.Unsafe (unsafePerformIO)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Decision, defaultDecision, isDefaultDecision)

type Unique = Integer

-- |References to 'Decision's are represented using 'Integer'
newtype IDSupply = IDSupply { unique :: Unique }

instance Eq IDSupply where
  s1 == s2 = unique s1 == unique s2

instance Show IDSupply where
  show = showUnique . unique

-- |Retrieve an 'Integer' representation of the unique identifier
mkInteger :: Unique -> Integer
mkInteger = id

showUnique :: Unique -> String
showUnique = show

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = return (IDSupply 1)

leftSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply i) = IDSupply (2 * i)

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply i) = IDSupply (2 * i + 1)

-- |Internal store for 'Decision's
store :: IORef (Map.Map Unique Decision)
store = unsafePerformIO (newIORef Map.empty)
{-# NOINLINE store #-}

getDecisionRaw :: Unique -> IO Decision
getDecisionRaw u = Map.findWithDefault defaultDecision u
                   `liftM` readIORef store

setDecisionRaw :: Unique -> Decision -> IO ()
setDecisionRaw u c
  | isDefaultDecision c = modifyIORef store $ Map.delete u -- collect garbage
  | otherwise           = modifyIORef store $ Map.insert u c

unsetDecisionRaw :: Unique -> IO ()
unsetDecisionRaw = modifyIORef store . Map.delete
