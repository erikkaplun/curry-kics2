-- ---------------------------------------------------------------------------
-- | IDSupply implementation using giant number representation by Paul Tarau
-- ---------------------------------------------------------------------------
module IDSupply
  ( IDSupply, initSupply, leftSupply, rightSupply, unique
  , Unique, mkInteger, showUnique
  , getDecisionRaw, setDecisionRaw, unsetDecisionRaw
  ) where

import Control.Monad (liftM)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
--import qualified Data.Map as Map (Map, empty, delete, findWithDefault, insert)
import System.IO.Unsafe (unsafePerformIO)

import Giant (n, o, i, emptyTMap, lookupT, insertT, deleteT, T(..), TMap)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Decision, defaultDecision, isDefaultDecision)

type Unique = T

-- |References to 'Decision's are represented using the tree representation for giant numbers
newtype IDSupply = IDSupply { unique :: Unique }

instance Eq IDSupply where
  s1 == s2 = unique s1 == unique s2

instance Show IDSupply where
  show = showUnique . unique

-- |Retrieve an 'Integer' representation of the unique identifier
mkInteger :: Unique -> Integer
mkInteger = n

showUnique :: Unique -> String
showUnique = show . n

-- |Initialize a new 'IDSupply'
initSupply :: IO IDSupply
initSupply = return (IDSupply T)

leftSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply u) = IDSupply (o u) 

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply u) = IDSupply (i u)

-- |Internal store for 'Decision's
store :: IORef (TMap Decision)
store = unsafePerformIO (newIORef emptyTMap)
{-# NOINLINE store #-}

getDecisionRaw :: Unique -> IO Decision
getDecisionRaw u = (\s -> maybe defaultDecision  id (lookupT u s))
                   `liftM` readIORef store

setDecisionRaw :: Unique -> Decision -> IO ()
setDecisionRaw u c
  | isDefaultDecision c = modifyIORef store $ deleteT u -- collect garbage
  | otherwise           = modifyIORef store $ insertT u c

unsetDecisionRaw :: Unique -> IO ()
unsetDecisionRaw = modifyIORef store . deleteT
