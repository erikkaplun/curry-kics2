module ID where

data ID
  = ID Integer
  | FreeID Integer
    deriving (Eq, Ord)

instance Show ID where
  show (ID i)       = show i --"ID"
  show (FreeID i)   = "Free" ++ show i

newtype IDSupply = IDSupply Integer

initSupply :: IO IDSupply
initSupply = return (IDSupply 1)

leftSupply :: IDSupply -> IDSupply
leftSupply (IDSupply i) = IDSupply (2 * i)

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply i) = IDSupply (2 * i + 1)

thisID :: IDSupply -> ID
thisID (IDSupply i) = ID i

freeID :: IDSupply -> ID
freeID (IDSupply i) = FreeID i
