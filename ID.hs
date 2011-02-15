module ID where

data ID = ID Int
        | FreeID Int

instance Show ID where 
  show (ID i)       = show i --"ID" 
  show (FreeID _)   = "Free"

initID = ID 1

leftID (ID i) = ID (2*i)

rightID (ID i) = ID (2*i+1)

freeID (ID i) = FreeID i

data IDSupply = IDSupply ID IDSupply IDSupply

thisID :: IDSupply -> ID
thisID (IDSupply i _ _) = i

leftSupply, rightSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply _ s _) = s
rightSupply (IDSupply _ _ s) = s