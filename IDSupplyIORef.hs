----------------
-- IDSupply
----------------

newtype ID = ID (IORef Choice) deriving Eq

instance Show ID where show _ = "r"

mkInt :: ID -> Integer
mkInt = error "IDSupplyIORef.mkInt" 

data IDSupply = IDSupply ID IDSupply IDSupply

thisID :: IDSupply -> ID
thisID (IDSupply i _ _) = i

thisIDForFree :: IDSupply -> ID
thisIDForFree = error "free variable must be used with other supply library"

bind :: ID -> ID -> Solved
bind i j = undefined

(=:=) :: Try a -> Try a -> C_Success
(=:=) = undefined

leftID, rightID :: ID -> ID
leftID  _ = undefined
rightID _ = undefined

nfChoiceIO :: (NormalFormIO a,NonDet a) => (a -> IO b) -> ID -> a -> a -> IO b
nfChoiceIO = undefined

narrow :: NonDet a => ID -> a -> a -> a
narrow = choiceCons

nfChoice :: (NormalForm a,NonDet b) => (a -> b) -> ID -> a -> a -> b
nfChoice cont i x1 x2 = choiceCons i (cont $!! x1) (cont $!! x2)

tryChoice :: ID -> a -> a -> Try a
tryChoice = Choice

leftSupply, rightSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply _ s _) = s
rightSupply (IDSupply _ _ s) = s

initIDSupply :: IO IDSupply
initIDSupply = getPureSupply NoChoice

{-# NOINLINE getPureSupply #-}
getPureSupply :: Choice -> IO IDSupply
getPureSupply def = do
    s1 <- unsafeInterleaveIO (getPureSupply def)
    s2 <- unsafeInterleaveIO (getPureSupply def)
    n  <- unsafeInterleaveIO  (newIORef def)
    return (IDSupply (ID n) s1 s2)


lookupChoice :: ID -> IO Choice
lookupChoice (ID r) = readIORef r

setChoice :: ID -> Choice -> IO ()
setChoice (ID r) c = writeIORef r c

showsChoice :: Show a => Int -> ID -> a -> a -> ShowS
showsChoice d r x1 x2 = 
  showChar '(' . 
  showsPrec d x1 .
  showString " ?" . shows r .
  showsPrec d x2 .
  showChar ')' 