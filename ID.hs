module ID where

data ID = ID Int
        | FreeID Int

instance Show ID where 
  show (ID _)       = "ID" 
  show (FreeID _)   = "Free"

initID = ID 1

leftID (ID i) = ID (2*i)

rightID (ID i) = ID (2*i+1)

freeID (ID i) = FreeID i

data IDSupply = IDSupply ID IDSupply IDSupply

thisID :: IDSupply -> ID
thisID (IDSupply i _ _) = i