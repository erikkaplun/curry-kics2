-- ---------------------------------------------------------------------------
-- | ID implementation using IORefs
-- ---------------------------------------------------------------------------
{-# LANGUAGE FlexibleInstances #-}
module IDImpl
  ( Ref, mkIntRef, IDSupply, initSupply, leftSupply, rightSupply, thisRef
  , lookupChoiceRef, setChoiceRef
  ) where

import Data.IORef
import System.IO.Unsafe (unsafeInterleaveIO)

-- SOURCE pragma to allow mutually recursive dependency
import {-# SOURCE #-} ID (Choice, defaultChoice, isDefaultChoice)

-- |References to 'Choice's are represented as 'IORef's
type Ref = IORef Choice

instance Show (IORef Choice) where
  show _ = ""

-- |A conversion of a 'Ref' into an integer is not possible for this
--  implementation
mkIntRef :: Ref -> Integer
mkIntRef = error "IDSupplyIORef.mkIntRef"

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
  s1 <- unsafeInterleaveIO (getPureSupply def)
  s2 <- unsafeInterleaveIO (getPureSupply def)
  r  <- unsafeInterleaveIO (newIORef def)
  return (IDSupply r s1 s2)

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
lookupChoiceRef = readIORef

setChoiceRef :: Ref -> Choice -> IO ()
setChoiceRef = writeIORef
