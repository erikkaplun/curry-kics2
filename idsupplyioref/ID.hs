module ID(Choice(..), ID(..), IDSupply,
          initSupply, leftSupply, rightSupply, thisID, freeID,
          lookupChoice, setChoice, leftID, rightID, narrowID,
          setUnsetChoice)
 where

import Data.IORef
import System.IO.Unsafe

-- Type to encode the selection taken in a Choice structure
data Choice
  = NoChoice
  | ChooseLeft
  | ChooseRight
  | BindTo ID
  | BoundTo ID
  deriving (Eq,Show)

-- Type to identify different Choice structures in a non-deterministic result.
-- Here we implement it as IO references
data ID = ID Ref
        | FreeID IDSupply
 deriving Eq

type Ref = IORef Choice

instance Show ID where 
  show (ID _)       = "ID" 
  show (FreeID _)   = "Free"

data IDSupply = IDSupply ID IDSupply IDSupply
instance Eq IDSupply where IDSupply i _ _ == IDSupply j _ _ = i == j

initSupply :: IO IDSupply
initSupply = getPureSupply NoChoice

{-# NOINLINE getPureSupply #-}
getPureSupply :: Choice -> IO IDSupply
getPureSupply def = do
    s1 <- unsafeInterleaveIO (getPureSupply def)
    s2 <- unsafeInterleaveIO (getPureSupply def)
    n  <- unsafeInterleaveIO  (newIORef def)
    return (IDSupply (ID n) s1 s2)

leftSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply _ s _) = s


rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply _ _ s) = s

thisID :: IDSupply -> ID
thisID (IDSupply i _ _) = i

freeID :: IDSupply -> ID
freeID i = FreeID i

leftID, rightID :: ID -> ID
leftID  (FreeID s) = freeID (leftSupply s)
rightID (FreeID s) = freeID (rightSupply s)

ref :: ID -> Ref
ref (ID x)       = x
ref (FreeID (IDSupply (ID x) _ _)) = x

narrowID :: ID -> ID
narrowID (FreeID s) = let! r =  ref (thisID s) in (ID r)
narrowID i          = i

-----------------------
-- Managing choices
-----------------------

lookupChoiceRaw :: ID -> IO Choice
lookupChoiceRaw i = readIORef (ref i) 

lookupChoice :: ID -> IO Choice
lookupChoice i = lookupChoiceRaw i >>= unchain
  where
    unchain (BoundTo j) = lookupChoice j

    unchain (BindTo j)  = do 
      setChoice i (BoundTo j)
      setChoice (leftID i) (BindTo (leftID j))
      setChoice (rightID i) (BindTo (rightID j))
      lookupChoice j
      
    unchain c           = return c

resetFreeVar :: ID -> IO ()
resetFreeVar i = lookupChoiceRaw i >>= propagate 
  where
    propagate (BindTo _)  = setChoice i NoChoice
    propagate (BoundTo _) = do
        let il = leftID i
            ir = rightID i
        setChoice i NoChoice
        resetFreeVar il
        resetFreeVar ir

setChoice :: ID -> Choice -> IO ()
setChoice i c = writeIORef (ref i) c

setChoiceGetID :: ID -> Choice -> IO ID
setChoiceGetID i c = lookupChoiceRaw i >>= unchain
  where
    unchain (BindTo j)  = setChoiceGetID j c --error "assumption violated" 
    unchain (BoundTo j) = setChoiceGetID j c
    unchain _           = setChoice i c >> return i



setUnsetChoice :: ID -> Choice -> IO (Maybe (IO ()))
setUnsetChoice i c = do j <- setChoiceGetID i c
                        case c of
                         BindTo _ -> return (Just (resetFreeVar j))
                         _        -> return (Just (setChoice j NoChoice))

