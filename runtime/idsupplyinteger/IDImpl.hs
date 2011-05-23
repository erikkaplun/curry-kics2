-- ---------------------------------------------------------------------------
-- | ID implementation using Integers
-- ---------------------------------------------------------------------------
module IDImpl
  ( Ref, mkIntRef, IDSupply, initSupply, leftSupply, rightSupply, thisRef
  , lookupChoiceRef, setChoiceRef
  ) where

import Data.IORef (IORef, newIORef, readIORef, modifyIORef)
import qualified Data.Map as Map (Map, empty, delete, insert, findWithDefault)
import System.IO.Unsafe (unsafePerformIO)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Choice, defaultChoice, isDefaultChoice)

-- |References to 'Choice's are represented as 'Integer's
type Ref = Integer

mkIntRef :: Ref -> Integer
mkIntRef = id

-- ---------------------
-- ID Supply
-- ---------------------

newtype IDSupply = IDSupply Ref
  deriving Eq

instance Show IDSupply where
  show (IDSupply i) = show i

initSupply :: IO IDSupply
initSupply = return (IDSupply 1)

leftSupply :: IDSupply -> IDSupply
leftSupply (IDSupply i) = IDSupply (2 * i)

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply i) = IDSupply (2 * i + 1)

thisRef :: IDSupply -> Ref
thisRef (IDSupply i) = i

-- ---------------------
-- Managing choices
-- ---------------------

type SetOfChoices = Map.Map Ref Choice

{-# NOINLINE store #-}
store :: IORef SetOfChoices
store = unsafePerformIO (newIORef Map.empty)

lookupChoiceRef :: Ref -> IO Choice
lookupChoiceRef r = do
  st <- readIORef store
  return $ Map.findWithDefault defaultChoice r st

setChoiceRef :: Ref -> Choice -> IO ()
setChoiceRef r c = modifyIORef store $ if isDefaultChoice c
  then Map.delete r
  else Map.insert r c
