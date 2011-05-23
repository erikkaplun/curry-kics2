-- ---------------------------------------------------------------------------
-- | ID implementation using IORefs
-- ---------------------------------------------------------------------------
module IDImpl
  ( Ref, mkIntRef, IDSupply, initSupply, leftSupply, rightSupply, thisRef
  , lookupChoiceRef, setChoiceRef
  ) where

import Data.IORef
import GHC.IO (unsafeDupableInterleaveIO)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Choice, defaultChoice)

-- |References to 'Choice's are represented as 'IORef's
newtype Ref = Ref { unRef :: IORef Choice } deriving Eq
-- type Ref = IORef Choice
-- unRef = id

instance Show Ref where
  show _ = ""

-- |A conversion of a 'Ref' into an integer is not possible for this
--  implementation
mkIntRef :: Ref -> Integer
mkIntRef = error "IDImpl.mkIntRef"

-- ---------------------
-- ID Supply
-- ---------------------

data IDSupply = IDSupply Ref IDSupply IDSupply

instance Eq IDSupply where
  IDSupply i _ _ == IDSupply j _ _ = i == j

instance Show IDSupply where
  show _ = ""

initSupply :: IO IDSupply
initSupply = getPureSupply defaultChoice

{-# NOINLINE getPureSupply #-}
getPureSupply :: Choice -> IO IDSupply
getPureSupply def = do
  s1 <- unsafeDupableInterleaveIO (getPureSupply def)
  s2 <- unsafeDupableInterleaveIO (getPureSupply def)
  r  <- unsafeDupableInterleaveIO (newIORef def)
  return (IDSupply (Ref r) s1 s2)
--   return (IDSupply r s1 s2)

leftSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply _ s _) = s

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply _ _ s) = s

thisRef :: IDSupply -> Ref
thisRef (IDSupply r _ _) = r

-- ---------------------
-- Managing choices
-- ---------------------

lookupChoiceRef :: Ref -> IO Choice
lookupChoiceRef = readIORef . unRef

setChoiceRef :: Ref -> Choice -> IO ()
setChoiceRef r c = writeIORef (unRef r) c
