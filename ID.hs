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
  , setChoiceRaw, nextNIDs
  ) where

import Control.Monad (liftM, when, zipWithM_)
import IDImpl

trace :: Bool
trace = True

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
  -- |Non-deterministic choice between a list of lists of constraints
  | ConstraintChoices ID [[Constraint]]
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

-- TODO: Idea: Split ChooseN Int Int into two cases
--   - ChooseFree Int Int     for free variables and
--   - ChooseNarrowed Int     for narrowed variables
-- to further separate these constructs and avoid the use of irregular values
-- like ChooseN 0 (-1)


instance Eq Choice where
  NoChoice    == NoChoice    = True
  ChooseLeft  == ChooseLeft  = True
  ChooseRight == ChooseRight = True
  ChooseN c _ == ChooseN d _ = c == d
  BindTo  _   == BindTo  _   = error "ID.Choice.(==): BindTo"
  BoundTo _ _ == BoundTo _ _ = error "ID.Choice.(==): BoundTo"
  LazyBind _  == LazyBind _  = error "ID.Choice.(==): LazyBind"
  _           == _           = False

-- |Default 'Choice'. The default 'Choice' is provided via a function to break
--  recursive dependencies.
defaultChoice :: Choice
defaultChoice = NoChoice

-- |Is the given 'Choice' the 'defaultChoice'?
isDefaultChoice :: Choice -> Bool
isDefaultChoice NoChoice = True
isDefaultChoice _        = False

-- ---------------------------------------------------------------------------
-- ID
-- ---------------------------------------------------------------------------

-- Type to identify different Choice structures in a non-deterministic result
data ID
    -- |Identifier for a choice introduced by the use of the (?) operator
  = ID Ref
    -- |Identifier for a choice for a free variable
  | FreeID IDSupply
    -- |Identifier for a choice for a narrowed variable (free before)
  | Narrowed IDSupply
    deriving Eq

instance Show ID where
  show (ID i)       = "?" ++ show i
  show (FreeID i)   = "Free" ++ show i
  show (Narrowed i) = "Narrowed" ++ show i

-- |Construct an 'ID' for a free variable from an 'IDSupply'
freeID :: IDSupply -> ID
freeID = FreeID

-- |Construct an 'ID' for a binary choice from an 'IDSupply'
thisID :: IDSupply -> ID
thisID s = ID (thisRef s)

-- |Retrieve the 'Ref' from an 'ID'
ref :: ID -> Ref
ref (ID       r) = r
ref (FreeID   s) = thisRef s
ref (Narrowed s) = thisRef s

-- |Retrieve the 'IDSupply' from an free or narrowed 'ID'
supply :: ID -> IDSupply
supply (ID       _) = error "ID.supply: ID"
supply (FreeID   s) = s
supply (Narrowed s) = s

-- |Retrieve the left child 'ID' from an free 'ID'
leftID :: ID -> ID
leftID  (FreeID s) = freeID (leftSupply s)
leftID  _          = error "ID.leftID: no FreeID"

-- |Retrieve the right child 'ID' from an free 'ID'
rightID :: ID -> ID
rightID (FreeID s) = freeID (rightSupply s)
rightID  _         = error "ID.rightID: no FreeID"

-- |Convert a free or narrowed 'ID' into a narrowed one
narrowID :: ID -> ID
narrowID (ID _)         = error "ID.narrowID: ID"
narrowID (FreeID s)     = Narrowed s
narrowID n@(Narrowed _) = n

-- |Conversion of ID into integer for monadic search operators
mkInt :: ID -> Integer
mkInt (ID       i) = mkIntRef i
mkInt (FreeID   _) = error "ID.mkInt: FreeID"
mkInt (Narrowed _) = error "ID.mkInt: Narrowed"

-- |Ensure that an 'ID' is not an 'ID' for a binary choice
ensureNotID :: ID -> ID
ensureNotID (ID _)         = error "ensureNotID: ID"
ensureNotID x@(FreeID _)   = x
ensureNotID x@(Narrowed _) = x

-- |Ensure that an 'ID' is an 'ID' for a free variable
ensureFreeID :: ID -> ID
ensureFreeID (ID _)       = error "ensureFreeID: ID"
ensureFreeID x@(FreeID _) = x
ensureFreeID (Narrowed _) = error "ensureFreeID: Narrowed"

-- ---------------------------------------------------------------------------
-- Choice Management
-- ---------------------------------------------------------------------------

-- ------------------
-- Looking up choices
-- ------------------

-- |Lookup the 'Choice' an 'ID' ultimately is bound to
lookupChoice :: ID -> IO Choice
lookupChoice i = fst `liftM` lookupChoiceID i

-- |Lookup the 'ID' an 'ID' ultimately is bound to
lookupID :: ID -> IO ID
lookupID i = snd `liftM` lookupChoiceID i

-- |Lookup the 'Choice' and the 'ID' an 'ID' ultimately is bound to
lookupChoiceID :: ID -> IO (Choice, ID)
lookupChoiceID i = do
  when trace $ putStrLn $ "lookupChoiceID " ++ show i
  r <- lookupChoiceRaw i >>= unchain
  when trace $ putStrLn $ "lookupChoiceID returned " ++ take 200 (show r)
  return r
  where
    -- For BindTo, we shorten chains of multiple BindTos by directly binding
    -- to the last ID in the chain.
    unchain (BindTo j) = do
      retVal@(c, lastId) <- lookupChoiceID j
      case c of
        NoChoice      -> shortenTo lastId
        ChooseN _ num -> propagateBind i lastId num
        LazyBind _    -> shortenTo lastId
        _             -> error $ "ID.lookupChoiceID: " ++ show c
      return retVal
      where
        shortenTo lastId = when (j /= lastId) $ setChoiceRaw i (BindTo lastId)

    -- For BoundTo, the chains should already be shortened since the Choice
    -- "BoundTo j" is only set if the variable j has been set to a "ChooseN"
    -- and therefore could not have been changed in between.
    unchain (BoundTo j _) = lookupChoiceID j

    -- For all other choices, there are no chains at all
    unchain c           = return (c, i)

-- ---------------
-- Setting choices
-- ---------------

-- |Set the 'Choice' for the given 'ID'
setChoice :: ID -> Choice -> IO ()
setChoice i c = setChoiceGetChange i c >> return ()

-- Set the given 'Choice' for the given 'ID' and return an action to recover
-- the former 'Choice'
setUnsetChoice :: ID -> Choice -> IO (IO ())
setUnsetChoice i c = do
  mChange <- setChoiceGetChange i c
  case mChange of
    Nothing                     -> return (return ())
    Just (oldChoice, changedId) -> return $ case c of
      BindTo _ -> resetFreeVar changedId oldChoice
      _        -> do
        when trace $ putStrLn $ "reset " ++ show changedId ++ " to " ++ take 200 (show oldChoice)
        setChoiceRaw changedId oldChoice

-- |Set the 'Choice' for the given 'ID', eventually following a chain and
--  return the ultimately changed 'ID' and its former 'Choice'
setChoiceGetChange :: ID -> Choice -> IO (Maybe (Choice, ID))
-- We do not bind an ID to itself to avoid cycles
setChoiceGetChange i (BindTo j) | ref i == ref j = return Nothing
setChoiceGetChange i c = do
  when trace $ putStrLn $ "setChoiceGetChange " ++ show i ++ ' ' : take 200 (show c)
  r <- lookupChoiceRaw i >>= unchain
  when trace $ putStrLn $ "setChoiceGetChange returned " ++ take 200 (show r)
  return r
  where
    -- BindTo: change the last variable in the chain and propagate the binding
    -- TODO: At the moment the propagation is necessary, but may be removed
    -- later (cf. examples/Unification.curry, goal25)
    unchain (BindTo k)    = do
      retVal <- setChoiceGetChange k c
      case c of
        ChooseN _ num -> propagateBind i k num
        _             -> return ()
      return retVal
    -- BoundTo: change the last variable in the chain
    unchain (BoundTo k _) = setChoiceGetChange k c
    unchain oldChoice     = case c of
      BindTo j -> do
        -- Avoid binding i to a variable which is transitively bound to i
        lastId <- lookupID j
        if ref lastId == ref i
          then return Nothing
          else setChoiceRaw i c >> return (Just (oldChoice, i))
      _        -> setChoiceRaw i c >> return (Just (oldChoice, i))

-- |Reset a free variable to its former 'Choice' and reset its children if
--  the binding has already been propagated
resetFreeVar :: ID -> Choice -> IO ()
resetFreeVar i oldChoice = reset oldChoice (supply i)
  where
    reset c s = do
      when trace $ putStrLn $ "reset " ++ show i ++ " to " ++ take 200 (show c)
      lookupChoiceRef (thisRef s) >>= propagate c s

    propagate c s (BindTo _)      = setChoiceRef (thisRef s) c
    propagate c s (BoundTo _ num) = do
      setChoiceRef (thisRef s) c
      mapM_ (reset NoChoice) $ nextNSupplies s num
    propagate _ _ _ = error "ID.resetFreeVar.propagate: no binding"

-- -------------------
-- Auxiliary functions
-- -------------------

-- |Lookup the 'Choice' stored for the given 'ID' without following chains
lookupChoiceRaw :: ID -> IO Choice
lookupChoiceRaw i = do
  r <- lookupChoiceRef $ ref i
--   when trace $ putStrLn $ "lookupChoiceRaw " ++ show i ++ " -> " ++ show r
  return r

-- |Set the 'Choice' for the given 'ID' in the store without following chains
setChoiceRaw :: ID -> Choice -> IO ()
setChoiceRaw i c = do
--   when trace $ putStrLn $ "setChoiceRaw " ++ show i ++ ' ' : show c
  setChoiceRef (ref i) c

-- |Propagate a binding of 'ID' x to 'ID' y for the next cnt independent child
--  'ID's. x as well as y are both expected to be either a free or a narrowed
--  variable
propagateBind :: ID -> ID -> Int -> IO ()
propagateBind x y cnt = do
  -- bind i to j
  setChoiceRaw x (BoundTo y cnt)
  -- propagate the binding to the children
  zipWithM_ (\a b -> setChoice a (BindTo b))
    (nextNIDs xFreeNarrowed cnt) (nextNIDs yFree cnt)
  where
    xFreeNarrowed = ensureNotID x
    yFree = ensureFreeID y

-- Compute a list of the next n free 'ID's for a given 'ID'
nextNIDs :: ID -> Int -> [ID]
nextNIDs = nextNIDsFromSupply . supply

-- Compute a list of the next n free 'ID's for a given 'IDSupply' s
nextNIDsFromSupply :: IDSupply -> Int -> [ID]
nextNIDsFromSupply s n = map freeID $ nextNSupplies s n

-- |Compute the next n independent 'IDSupply's for a given 'IDSupply' s
nextNSupplies :: IDSupply -> Int -> [IDSupply]
nextNSupplies s n
  | n <  0    = error $ "ID.nextNSupplies: " ++ show n
  | n == 0    = []
  | n == 1    = [leftSupply s]
  | otherwise = nextNSupplies' s n
  where
    nextNSupplies' s' n'
      | n' == 1    = [s']
      | otherwise = nextNSupplies' (leftSupply  s') (n' - halfn) ++
                    nextNSupplies' (rightSupply s') halfn
        where halfn = n' `div` 2
