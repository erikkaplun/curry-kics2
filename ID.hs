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

import Control.Monad (liftM, when)
import IDImpl

-- ---------------------------------------------------------------------------
-- Constraint
-- ---------------------------------------------------------------------------

data Constraint
  -- binding of an 'ID' to a 'Choice'
  = ID :=: Choice
  -- Failure (unsolvable constraint)
  | Unsolvable
  -- non-deterministic choice between two lists of constraints
  | ConstraintChoice ID [Constraint] [Constraint]
    deriving (Eq, Show)

-- ---------------------------------------------------------------------------
-- Choice
-- ---------------------------------------------------------------------------

-- Type to encode the selection taken in a Choice(s) structure
data Choice
    -- No choice has been made so far
  = NoChoice
    -- The left value of an (?) is chosen
  | ChooseLeft
    -- The right value of an (?) is chosen
  | ChooseRight
    -- ChooseN consIdx argCnt is the choice for the constructor with the
    -- index consIdx which has argCnt arguments
  | ChooseN Int Int
    -- a free or narrowed variable is bound to the free variable with the
    -- given id; the bindings of the IDs for the arguments have not been
    -- propagated yet
  | BindTo ID
    -- a free or narrowed variable is bound to the variable with the given ID;
    -- the bindings for the n arguments have also been propagated
  | BoundTo ID Int
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
narrowID (FreeID s)     = Narrowed s
narrowID n@(Narrowed _) = n
narrowID i@(ID _)       = i

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

lookupChoice :: ID -> IO Choice
lookupChoice i = fst `liftM` lookupChoiceID i

lookupID :: ID -> IO ID
lookupID i = snd `liftM` lookupChoiceID i

lookupChoiceID :: ID -> IO (Choice, ID)
lookupChoiceID i = do
--   putStrLn $ "lookupChoiceID " ++ show i
--   raw <- lookupChoiceRaw i
--   putStrLn $ "Raw result: " ++ show raw
  lookupChoiceRaw i >>= unchain
  where
    unchain (BoundTo j _) = lookupChoiceID j

    unchain (BindTo  j  ) = do
      (c, id) <- lookupChoiceID j
      case c of
        ChooseN _ num -> propagateBind i j num
        NoChoice      -> return ()
        LazyBind _    -> return ()
        errChoice     -> error $ "ID.lookupChoice returned " ++ show errChoice
      return (c, id)

    unchain c           = return (c, i) {-do
      putStrLn $ "ID.lookupChoice returned " ++ show c ++ " for ID " ++ show i
      return (c, i)-}

lookupChoiceRaw :: ID -> IO Choice
lookupChoiceRaw = lookupChoiceRef . ref

-- Propagate a binding of variable x to variable y for the next cnt child ids
-- x is expected to be either a free or a narrowed variable,
-- y is expected to be a free variable
propagateBind :: ID -> ID -> Int -> IO ()
propagateBind x@(FreeID xs) y@(FreeID ys) cnt = do
  setChoice x (BoundTo y cnt)
  sequence_ $ zipWith (\x' y' -> setChoice x' (BindTo y'))
                      (nextNIDs xs cnt) (nextNIDs ys cnt)
propagateBind x@(Narrowed xs) y@(FreeID ys) cnt = do
  setChoice x (BoundTo y cnt)
  sequence_ $ zipWith (\x' y' -> setChoice x' (BindTo y'))
                      (nextNIDs xs cnt) (nextNIDs ys cnt)
propagateBind errx erry errcnt = error $
  "propagateBind " ++ show errx ++ ' ' : show erry ++ ' ' : show errcnt

-- Compute a list of the next n free IDs for a given IDSupply s
nextNIDs :: IDSupply -> Int -> [ID]
nextNIDs s n
  | n <  0    = error $ "ID.nextNIDs with negative number " ++ show n
  | n == 0    = []
  | n == 1    = [freeID (leftSupply s)]
  | otherwise = nextNIDs' s n
  where
    nextNIDs' s n
      | n == 0    = []
      | n == 1    = [freeID s]
      | otherwise = nextNIDs' (leftSupply s) (n - halfn) ++ nextNIDs' (rightSupply s) halfn
        where halfn = n `div` 2


setChoice :: ID -> Choice -> IO ()
setChoice i c = setChoiceRef (ref i) c


-- Set the given 'Choice' for the given 'ID' and return an action to recover
-- the former 'Choice'
setUnsetChoice :: ID -> Choice -> IO (IO ())
setUnsetChoice i c = do
  (oldChoice, changedId) <- setChoiceGetChoiceID i c -- get current Choice
--   putStrLn $ "setUnsetChoice setting: " ++ show changedId ++ " to " ++ show c
  return $ case c of
    BindTo _ -> do
      resetFreeVar changedId
--       putStrLn $ "setUnsetChoice resetting free: " ++ show changedId ++ " to " ++ show  oldChoice
      setChoice changedId oldChoice -- TODO: BindTo k -> if changedId == k then return (return ()) else return (resetFreeVar changedId)
    _        -> do
--       putStrLn $ "setUnsetChoice resetting: " ++ show changedId ++ " to " ++ show  oldChoice
      setChoice changedId oldChoice -- was NoChoice


-- Set the 'Choice' for the last 'ID' referenced by the given 'ID' and
-- return the ultimately changed 'ID' and its former 'Choice'
setChoiceGetChoiceID :: ID -> Choice -> IO (Choice, ID)
-- setChoiceGetChoiceID i (BindTo j) | i == j = return (NoChoice, i) -- TODO: avoid cyclic bind; notice that NoChoice will not be evaluated
setChoiceGetChoiceID i c = lookupChoiceRaw i >>= unchain
  where
    unchain (BoundTo j _) = setChoiceGetChoiceID j c

    unchain (BindTo  j  ) = do
      (resC, resId) <- setChoiceGetChoiceID j c
      case c of
        ChooseN _ num -> propagateBind i j num
        _             -> return ()
      return (resC, resId)

    unchain oldChoice     = case c of
      BindTo j -> do
        (lastChoice, lastid) <- lookupChoiceID j
        setChoice i c -- TODO: when (lastid /= i) $ setChoice i (BindTo lastid)
        return (oldChoice, i)
      _        -> setChoice i c >> return (oldChoice, i)

resetFreeVar :: ID -> IO ()
resetFreeVar i = case i of
  ID _       -> error $ "resetFreeVar " ++ show i
  FreeID s   -> resetFreeVar' s
  Narrowed s -> resetFreeVar' s
  where
    resetFreeVar' s = lookupChoiceRaw i >>= propagate s

    propagate _ (BindTo _)  = setChoice i NoChoice
    propagate s (BoundTo _ num) = do
      setChoice i NoChoice
      mapM_ resetFreeVar $ nextNIDs s num
