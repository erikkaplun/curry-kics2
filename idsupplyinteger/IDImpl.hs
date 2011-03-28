-- ---------------------------------------------------------------------------
-- | ID implementation using Integers
-- ---------------------------------------------------------------------------
module IDImpl
  ( Choice (..), ID (..), Constraint (..), IDSupply
  , mkInt, initSupply, leftSupply, rightSupply, thisID
  , lookupChoiceRaw, setChoice
  , store
  ) where

import Data.IORef
import qualified Data.Map
import System.IO.Unsafe

-- Type to encode the selection taken in a Choice structure
data Choice
  = NoChoice        -- No choice has been made so far
  | ChooseLeft
  | ChooseRight
  | ChooseN Int Int -- ChooseN consIdx argCnt is the choice for the
                    -- constructor with the index consIdx which has argCnt
                    -- arguments
  | BindTo ID       -- a free or narrowed variable is bound to the
                    -- free variable with the given id; the bindings of the
                    -- IDs for the arguments have not been propagated yet
  | BoundTo ID Int  -- a free or narrowed variable is bound to the variable
                    -- with the given ID; the bindings for the n arguments
                    -- have also been propagated
  | LazyBind [Constraint]
    deriving Show

instance Eq Choice where
  NoChoice    == NoChoice    = True
  ChooseLeft  == ChooseLeft  = True
  ChooseRight == ChooseRight = True
  ChooseN c _ == ChooseN d _ = c == d
  BindTo  i   == BindTo  j   = i == j
  BoundTo i _ == BoundTo j _ = i == j
  LazyBind cs == LazyBind ds = cs == ds
  _           == _           = False

-- Type to identify different Choice structures in a non-deterministic result.
-- Here we implement it as integer values.
data ID
  = ID Ref
  | FreeID IDSupply
  | Narrowed IDSupply
    deriving (Eq, Ord)

type Ref = Integer

instance Show ID where
  show (ID i)       = show i
  show (FreeID i)   = "Free" ++ show i
  show (Narrowed i) = "Narrowed" ++ show i

data Constraint = ID :=: Choice
                | Failed
  deriving (Eq, Show)

-- Conversion of ID into integer (for monadic search operators).
mkInt :: ID -> Integer
mkInt (ID i) = i
mkInt (FreeID _) = error "IDSupplyInteger.mkInt: FreeID occurred?"

ref :: ID -> Ref
ref (ID r) = r
ref (FreeID (IDSupply r)) = r
ref (Narrowed (IDSupply r)) = r

-- ---------------------
-- ID Supply
-- ---------------------

newtype IDSupply = IDSupply Ref
  deriving (Eq, Ord)

instance Show IDSupply where
  show (IDSupply i) = show i

initSupply :: IO IDSupply
initSupply = return (IDSupply 1)

leftSupply :: IDSupply -> IDSupply
leftSupply (IDSupply i) = IDSupply (2 * i)

rightSupply :: IDSupply -> IDSupply
rightSupply (IDSupply i) = IDSupply (2 * i + 1)

thisID :: IDSupply -> ID
thisID (IDSupply i) = ID i

-- ---------------------
-- Managing choices
-- ---------------------

type SetOfChoices = Data.Map.Map Ref Choice

store :: IORef SetOfChoices
store = unsafePerformIO (newIORef Data.Map.empty)

lookupChoiceRaw :: ID -> IO Choice
lookupChoiceRaw i = do
  st <- readIORef store
  return $ Data.Map.findWithDefault NoChoice (ref i) st

setChoice :: ID -> Choice -> IO ()
setChoice i c = modifyIORef store $ case c of
  NoChoice -> Data.Map.delete (ref i)
  _        -> Data.Map.insert (ref i) c
