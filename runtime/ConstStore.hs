{-# LANGUAGE CPP, ExistentialQuantification #-}

module ConstStore where

import ID
import qualified Data.Map as Map
import Data.IORef       (IORef, readIORef, modifyIORef, newIORef)
import Unsafe.Coerce    (unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Constraint Store
-- ---------------------------------------------------------------------------

type ConstStore = Map.Map Integer Value

data Value = forall a . V a

unV :: Value -> a
unV (V x) = unsafeCoerce x

-- |Empty constraint store
emptyCs :: ConstStore
emptyCs = Map.empty

-- insert a constraint into a constraint store
addCs :: Constraints -> ConstStore -> ConstStore

-- combine two constraint stores
combineCs :: ConstStore -> ConstStore -> ConstStore

-- lookupCs looks for a velue bound in a constraint store
-- if the value is found the given function is applied,
-- if not, the default value is returned
lookupCs :: ConstStore -> ID -> (a -> b) -> b -> b

lookupWithGlobalCs :: ConstStore -> ID -> (a -> b) -> b -> b

-- the implementation
#ifdef DISABLE_CS

addCs _   = id
combineCs = const
lookupCs _ _ _ def = def
lookupWithGlobalCs _ _ _ def = def

#else

addCs (StructConstr  _) store = store
addCs (ValConstr i v _) store = id $! Map.insert (getKey i) (V v) store

combineCs = Map.union 

lookupCs cs i f def = maybe def (f . unV) (Map.lookup (getKey i) cs)

lookupWithGlobalCs cs i f def = lookupCs cs i f
                              $ lookupCs pureGlobalCs i f def

#endif

-- ---------------------------------------------------------------------------
-- Global Constraint Store
-- ---------------------------------------------------------------------------

pureGlobalCs :: ConstStore

-- |Retrieve the global constraint store
lookupGlobalCs :: IO ConstStore

-- adds a Constraint to the global constraint store
addToGlobalCs :: Constraints -> IO ()

-- the implementation

#ifdef DISABLE_CS

pureGlobalCs    = emptyCs
lookupGlobalCs  = return emptyCs
addToGlobalCs _ = return ()

#else

globalCs :: IORef ConstStore
globalCs = unsafePerformIO $ newIORef emptyCs
{-# NOINLINE globalCs #-}

pureGlobalCs = unsafePerformIO lookupGlobalCs

lookupGlobalCs = readIORef globalCs

addToGlobalCs (StructConstr     _) = return ()
addToGlobalCs cs@(ValConstr _ _ _) = modifyIORef globalCs (addCs cs)

#endif
