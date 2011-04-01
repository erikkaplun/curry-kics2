-- ---------------------------------------------------------------------------
-- ID module
-- ---------------------------------------------------------------------------
module ID
  ( Constraint (..), Choice (..), defaultChoice, isDefaultChoice
  , ID (..), IDSupply
  , mkInt, leftID, rightID, narrowID
  , initSupply, leftSupply, rightSupply, thisID, freeID
    -- * Choice management
  , lookupChoice, lookupID, lookupChoiceID, setChoice, setUnsetChoice
  ) where

import Control.Monad (liftM, when, zipWithM_)
import IDImpl

-- ---------------------------------------------------------------------------
-- Constraint
-- ---------------------------------------------------------------------------

-- |Type to encode constraints for a Choice(s) structure
data Constraint
  -- |Binding of an 'ID' to a 'Choice'
  = ID :=: Choice
  -- |Unsolvable constraint
  | Unsolvable
  -- |Non-deterministic choice between two lists of constraints
  | ConstraintChoice ID [Constraint] [Constraint]
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Choice
-- ---------------------------------------------------------------------------

-- |Type to encode the selection taken in a Choice(s) structure
data Choice
    -- |No choice has been made so far
  = NoChoice
    -- |The left value of an (?) is chosen
  | ChooseLeft
    -- |The right value of an (?) is chosen
  | ChooseRight
    -- |ChooseN consIdx argCnt is the choice for the constructor with the
    --  index consIdx which has argCnt arguments
  | ChooseN Int Int
    -- |a free or narrowed variable is bound to the free variable with the
    --  given id; the bindings of the IDs for the arguments have not been
    --  propagated yet
  | BindTo ID
    -- |A free or narrowed variable is bound to the variable with the given
    --  'ID'; the bindings for the n arguments have also been propagated
  | BoundTo ID Int
    -- |A free variable is lazily bound to an expression by a function
   --   pattern
  | LazyBind [Constraint]
    deriving Show

instance Eq Choice where
  NoChoice    == NoChoice    = True
  ChooseLeft  == ChooseLeft  = True
  ChooseRight == ChooseRight = True
  ChooseN c _ == ChooseN d _ = c == d
  BindTo  i   == BindTo  j   = error "ID.Choice.(==) on BindTo"
  BoundTo i _ == BoundTo j _ = error "ID.Choice.(==) on BoundTo"
  LazyBind _  == LazyBind _  = error "ID.Choice.(==) on LazyBind"
  _           == _           = False

defaultChoice :: Choice
defaultChoice = NoChoice

isDefaultChoice :: Choice -> Bool
isDefaultChoice NoChoice = True
isDefaultChoice _        = False

-- ---------------------------------------------------------------------------
-- ID
-- ---------------------------------------------------------------------------

-- Type to identify different Choice structures in a non-deterministic result
data ID
  = ID Ref
  | FreeID IDSupply
  | Narrowed IDSupply
    deriving Eq

instance Show ID where
  show (ID i)       = "?" ++ show i
  show (FreeID i)   = "Free" ++ show i
  show (Narrowed i) = "Narrowed" ++ show i

leftID :: ID -> ID
leftID  (FreeID s) = freeID (leftSupply s)

rightID :: ID -> ID
rightID (FreeID s) = freeID (rightSupply s)

narrowID :: ID -> ID
narrowID i@(ID _)       = i
narrowID (FreeID s)     = Narrowed s
narrowID n@(Narrowed _) = n

freeID :: IDSupply -> ID
freeID = FreeID

thisID :: IDSupply -> ID
thisID s = ID (thisRef s)

ref :: ID -> Ref
ref (ID       r) = r
ref (FreeID   s) = thisRef s
ref (Narrowed s) = thisRef s

-- Conversion of ID into integer (for monadic search operators).
mkInt :: ID -> Integer
mkInt (ID       i) = mkIntRef i
mkInt (FreeID   _) = error "ID.mkInt: FreeID"
mkInt (Narrowed _) = error "ID.mkInt: Narrowed"

-- ---------------------------------------------------------------------------
-- Choice Management
-- ---------------------------------------------------------------------------

-- ------------------
-- Looking up choices
-- ------------------

lookupChoice :: ID -> IO Choice
lookupChoice i = fst `liftM` lookupChoiceID i

lookupID :: ID -> IO ID
lookupID i = snd `liftM` lookupChoiceID i

lookupChoiceID :: ID -> IO (Choice, ID)
lookupChoiceID i = lookupChoiceRaw i >>= unchain
  where
    -- For BindTo, we shorten chains of multiple BindTos by directly binding
    -- to the last ID in the chain.
    unchain (BindTo j) = do
      (c, lastId) <- lookupChoiceID j -- resolve last id and its choice
      case c of
        NoChoice      -> shortenChain j lastId
        ChooseN _ num -> propagateBind i lastId num
        LazyBind _    -> shortenChain j lastId
        _             -> error $ "ID.lookupChoiceID returned " ++ show c
      return (c, lastId)
      where
        shortenChain j lastId =
          when (j /= lastId) (setChoiceRaw i (BindTo lastId))

    -- For BoundTo, the chains should already be shortened since the Choice
    -- "BoundTo" is only set if the variable j has been set to a "ChooseN"
    -- and could not have been changed in between.
    unchain (BoundTo j _) = lookupChoiceID j

    -- For all other choices, there are no chains at all
    unchain c           = return (c, i)

-- ---------------
-- Setting choices
-- ---------------

-- |Set the 'Choice' for the given 'ID'
setChoice :: ID -> Choice -> IO ()
setChoice i c = setChoiceRaw i c >> return ()

-- Set the given 'Choice' for the given 'ID' and return an action to recover
-- the former 'Choice'
setUnsetChoice :: ID -> Choice -> IO (IO ())
setUnsetChoice i c = do
  mChange <- setChoiceGetChange i c
  case mChange of
    Nothing                     -> return (return ())
    Just (oldChoice, changedId) -> return $ case c of
      BindTo _ -> resetFreeVar changedId oldChoice
      _        -> setChoiceRaw changedId oldChoice

-- |Set the 'Choice' for the given 'ID', eventually following a chain and
--  return the ultimately changed 'ID' and its former 'Choice'
setChoiceGetChange :: ID -> Choice -> IO (Maybe (Choice, ID))
-- We do not bind an ID to itself to avoid cycles
setChoiceGetChange i (BindTo j) | i == j = return Nothing
setChoiceGetChange i c = lookupChoiceRaw i >>= unchain
  where
    -- BindTo, BoundTo: change the last variable in the chain
    unchain (BindTo k)    = do
      retVal <- setChoiceGetChange k c
      case c of
--         ChooseN _ num -> propagateBind i k num
        _             -> return ()
      return retVal
    unchain (BoundTo k _) = setChoiceGetChange k c
    unchain oldChoice     = case c of
      BindTo j -> do
        -- Avoid binding i to a variable which is transitively bound to i
        lastId <- lookupID j
        if lastId == i
          then return Nothing
          else setChoiceRaw i c >> return (Just (oldChoice, i))
      _        -> setChoiceRaw i c >> return (Just (oldChoice, i))

-- |Reset a free variable to its former 'Choice' and reset its children if
--  the binding has already been propagated
resetFreeVar :: ID -> Choice -> IO ()
resetFreeVar i oldChoice = case i of
  ID _       -> error $ "resetFreeVar " ++ show i
  FreeID s   -> resetFreeVar' oldChoice s
  Narrowed s -> resetFreeVar' oldChoice s
  where
    resetFreeVar' c s = lookupChoiceRaw i >>= propagate c s

    propagate c s (BindTo _)      = setChoiceRef (thisRef s) c
    propagate c s (BoundTo _ num) = do
      setChoiceRef (thisRef s) c
      mapM_ (resetFreeVar' NoChoice) $ nextNSupplies s num

-- -------------------
-- Auxiliary functions
-- -------------------

-- |Lookup the 'Choice' stored for the given 'ID', without following chains
lookupChoiceRaw :: ID -> IO Choice
lookupChoiceRaw = lookupChoiceRef . ref

-- |Set the 'Choice' for the given 'ID' in the store, without following chains
setChoiceRaw :: ID -> Choice -> IO ()
setChoiceRaw i c = do
  putStrLn $ show i ++ " -> " ++ show c
  setChoiceRef (ref i) c

-- |Propagate a binding of ID x to ID y for the next cnt child IDs
--  x is expected to be either a free or a narrowed variable,
--  y is expected to be a free variable
propagateBind :: ID -> ID -> Int -> IO ()
propagateBind x@(FreeID sx) y@(FreeID sy) cnt = do
  setChoice x (BoundTo y cnt)
  zipWithM_ (\x' y' -> setChoice x' (BindTo y')) (nextNIDs sx cnt) (nextNIDs sy cnt)
propagateBind x@(Narrowed sx) y@(FreeID sy) cnt = do
  setChoice x (BoundTo y cnt)
  zipWithM_ (\x' y' -> setChoice x' (BindTo y')) (nextNIDs sx cnt) (nextNIDs sy cnt)
propagateBind errx erry errcnt = error $
  "propagateBind " ++ show errx ++ ' ' : show erry ++ ' ' : show errcnt

-- Compute a list of the next n free 'ID's for a given 'IDSupply' s
nextNIDs :: IDSupply -> Int -> [ID]
nextNIDs s n = map freeID $ nextNSupplies s n

-- |Compute a list of the next n independent 'IDSupply's for a given
--  'IDSupply' s
nextNSupplies :: IDSupply -> Int -> [IDSupply]
nextNSupplies s n
  | n <  0    = error $ "ID.nextNIDs with negative number " ++ show n
  | n == 0    = []
  | n == 1    = [leftSupply s]
  | otherwise = nextNSupplies' s n
  where
    nextNSupplies' s n
      | n == 0    = []
      | n == 1    = [s]
      | otherwise = nextNSupplies' (leftSupply  s) (n - halfn) ++
                    nextNSupplies' (rightSupply s) halfn
        where halfn = n `div` 2
