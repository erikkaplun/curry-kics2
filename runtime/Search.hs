module Search where

import Control.Monad
import Control.Monad.State.Strict
import Control.Parallel.TreeSearch
import qualified Data.Map

import Debug
import ID
import MonadList
import PrimTypes
import Solver (SolutionTree (..), solves)
import Types

-- TODO what to do whith choices and failures
toIO :: C_IO a -> IO a
toIO (C_IO io) = io
toIO (Choice_C_IO _ _ _) = error "toIO: Non-determinism in IO occured"
toIO (Guard_C_IO constraints e) = do
  st <- solves constraints
  case st of
    SuccessST _ -> toIO e
    FailST           -> error "toIO (Guard): failed"
    ChoiceST  _ _ _  -> error "toIO (Guard): Non-determinism in IO occured"
    ChoicesST _ _    -> error "toIO (Guard): Non-determinism in IO occured"
toIO Fail_C_IO = error "toIO: failed"
toIO (Choices_C_IO i choices) = do
  c <- lookupChoice i
  case c of
    ChooseN idx _ -> toIO (choices !! idx)
    NoChoice -> error "toIO (Choices): Non-determinism in IO occured"
    LazyBind constraints -> toIO (guardCons constraints (choicesCons i choices))

fromIO :: IO a -> C_IO a
fromIO io = C_IO io

-- ---------------------------------------------------------------------------
-- Simple evaluation
-- ---------------------------------------------------------------------------

eval :: Show a => (IDSupply -> a) -> IO ()
eval goal = initSupply >>= print . goal

evalD :: Show a => a -> IO ()
evalD goal = print goal

evalIO :: (NormalForm a, Show a) => (IDSupply -> C_IO a) -> IO ()
-- evalIO goal = initSupply >>= toIO . goal >>= print
evalIO goal = computeWithDFS goal >>= execIOList

evalDIO :: (NormalForm a, Show a) => C_IO a -> IO ()
evalDIO goal = toIO goal >> return ()

execIOList :: IOList (C_IO a) -> IO ()
execIOList MNil                 = return ()
execIOList (MCons xact getRest) = toIO xact >> getRest >>= execIOList
execIOList (WithReset l _)      = l >>= execIOList

-- ---------------------------------------------------------------------------
-- Printing all results of a computation in a depth-first manner
-- ---------------------------------------------------------------------------

-- Evaluate a nondeterministic expression (thus, requiring some IDSupply)
-- and print all results in depth-first order.
-- The first argument is the operation to print a result (e.g., Prelude.print).
prdfs :: (Show a, NormalForm a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
prdfs prt mainexp = do
  s <- initSupply
  printValsDFS False prt (id $!! (mainexp s))

printValsDFS :: (Show a, NormalForm a) => Bool -> (a -> IO ()) -> a -> IO ()
printValsDFS fb cont a = do
  trace $ "\n\n-- \n\nprintValsDFS " ++ take 200 (show a)
  printValsDFS' fb cont (try a)

printValsDFS' :: (Show a, NormalForm a) => Bool -> (a -> IO ()) -> Try a -> IO ()
printValsDFS' _  _    Fail           = return ()
printValsDFS' fb cont (Val v)        = searchNF (printValsDFS fb) cont v
printValsDFS' fb cont (Choice i x y) = lookupChoice i >>= choose
  where
    choose ChooseLeft  = printValsDFS fb cont x
    choose ChooseRight = printValsDFS fb cont y
    choose NoChoice    = -- doWithChoices_ fb i
--                            [ (ChooseLeft , printValsDFS True cont x)
--                            , (ChooseRight, printValsDFS fb   cont y)
--                            ]
--  TODO: reactivate the implementation above, when performant
      if fb
        then do
          newChoiceOpt True ChooseLeft  x
          newChoiceOpt True ChooseRight y
          setChoice i NoChoice
        else do
          newChoiceOpt True ChooseLeft   x
          newChoiceOpt False ChooseRight y
     where
      newChoice fbt c a = do
        reset <- setUnsetChoice i c
        printValsDFS fbt cont a
        reset

      newChoiceOpt fbt c a = do
        -- Assumption 1: Binary choices can only be set to one of
        -- [NoChoice, ChooseLeft, ChooseRight], therefore the reset action may
        -- be ignored in between
        -- Assumption 2: Furthermore, binary Choices can not be chained, so
        -- setChoiceRaw may be used
        setChoice i c
        printValsDFS fbt cont a
    choose c           = error $ "Basics.printValsDFS'.choose: " ++ show c




printValsDFS' fb cont (Free i xs)   = lookupChoiceID i >>= choose
  where
    choose (LazyBind cs, _) = processLazyBind fb cs i xs (printValsDFS fb cont)
    choose (ChooseN c _, _) = printValsDFS fb cont (xs !! c)
    choose (NoChoice   , j) = cont $ choicesCons j xs

printValsDFS' fb cont (Narrowed i@(NarrowedID pns _) xs) = lookupChoiceID i >>= choose
  where
    choose (LazyBind cs, _) = processLazyBind fb cs i xs (printValsDFS fb cont)
    choose (ChooseN c _, _) = printValsDFS fb cont (xs !! c)
    choose (NoChoice   , j) =
--       doWithChoices_ fb i $ zipWithButLast mkChoice mkLastChoice [0 ..] xs

--     mkChoice n x = (ChooseN n (-1), printValsDFS True cont x)
--     mkLastChoice n x = (ChooseN n (-1), printValsDFS fb   cont x)
--  TODO: reactivate the implementation above, when performant
     if fb
       then do
         foldr1 (>>) $ zipWith3 (newChoice True) [0 ..] xs pns
         setChoice i NoChoice
       else foldr1 (>>) $ zipWithButLast3 (newChoice True) (newChoice False) [0 ..] xs pns
      where

       newChoice fbt n a pn = do
         setChoice i $ ChooseN n pn
         printValsDFS fbt cont a

    choose c           = error $ "Basics.printValsDFS'.choose: " ++ show c

printValsDFS' fb cont (Guard cs e) = solves cs >>= traverse fb
  where
    traverse _     FailST               = return ()
    traverse True  (SuccessST reset)    = printValsDFS True  cont e >> reset
    traverse False (SuccessST _    )    = printValsDFS False cont e
    traverse True  (ChoiceST reset l r) = do
      l >>= traverse True
      r >>= traverse True
      reset
    traverse False (ChoiceST _     l r) = do
      l >>= traverse True
      r >>= traverse False
    traverse True  (ChoicesST reset cs) = do
      mapM_ (>>= traverse True) cs
      reset
    traverse False (ChoicesST _     cs) = do
      mapMButLast_ (>>= traverse True) (>>= traverse False) cs

--     traverse Nothing = return ()
--     traverse (Just reset) =  if fb then (printValsDFS fb . try) $!< e >> reset
--                                    else (printValsDFS fb . try) $!< e

processLazyBind :: NonDet a => Bool -> [Constraint] -> ID -> [a] -> (a -> IO ()) -> IO ()
processLazyBind True cs i xs search = do
  reset <- setUnsetChoice i NoChoice
  search $ guardCons cs $ choicesCons i xs
  reset
processLazyBind False cs i xs search = do
  setChoice i NoChoice
  search $ guardCons cs $ choicesCons i xs

zipWithButLast3 :: (a -> b -> c -> d) -> (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWithButLast3 _ _     []     _      _      = []
zipWithButLast3 _ _      _     []     _      = []
zipWithButLast3 _ _      _     _      []     = []
zipWithButLast3 _ lastf (a:[]) (b:_ ) (c:_)  = lastf a b c : []
zipWithButLast3 _ lastf (a:_ ) (b:[]) (c:_)  = lastf a b c : []
zipWithButLast3 _ lastf (a:_ ) (b:_ ) (c:[]) = lastf a b c : []
zipWithButLast3 f lastf (a:as) (b:bs) (c:cs) = f a b c : zipWithButLast3 f lastf as bs cs

mapMButLast_ :: (a -> IO b) -> (a -> IO b) -> [a] -> IO ()
mapMButLast_ f lastf = sequence_ . mapButLast f lastf

mapButLast :: (a -> b) -> (a -> b) -> [a] -> [b]
mapButLast _ _     []     = []
mapButLast _ lastf [x]    = [lastf x]
mapButLast f lastf (x:xs) = f x : mapButLast f lastf xs

-- Attempt to gain more abstraction during search

-- doWithChoice :: Bool -> ID -> Choice -> IO a -> IO a
-- doWithChoice True i c act = do
--   reset <- setUnsetChoice i c
--   retVal <- act
--   reset
--   return retVal
-- doWithChoice False i c act = do
--   setChoice i c
--   act

-- doWithChoices :: Bool -> ID -> [(Choice, IO a)] -> IO [a]
-- doWithChoices _ _ []     = return []
-- doWithChoices True i ((c, act): cacts) = do
--   reset   <- setUnsetChoice i c
--   retVal  <- act
--   retVals <- mapM (uncurry (doWithChoice False i)) cacts
--   reset
--   return (retVal:retVals)
-- doWithChoices False i cacts = mapM1 (uncurry (doWithChoice False i)) cacts

-- doWithChoices_ :: Bool -> ID -> [(Choice, IO a)] -> IO ()
-- doWithChoices_ _ _ []     = return ()
-- doWithChoices_ True i ((c, act): cacts) = do
--   reset <- setUnsetChoice i c
--   _     <- act
--   mapM_ (uncurry (doWithChoice False i)) cacts
--   reset
-- doWithChoices_ False i cacts = mapM1_ (uncurry (doWithChoice False i)) cacts

-- ---------------------------------------------------------------------------
-- Depth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- Print all values of an expression in a depth-first manner.
-- The first argument is the operation to print a result (e.g., Prelude.print).
printDFS :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printDFS prt mainexp = computeWithDFS mainexp >>= printAllValues prt

-- Print one value of an expression in a depth-first manner:
printDFS1 :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printDFS1 prt mainexp = computeWithDFS mainexp >>= printOneValue prt

-- Print all values on demand of an expression in a depth-first manner:
printDFSi :: (NormalForm a, Show a) =>
             MoreDefault -> (a -> IO ()) -> (IDSupply -> a) -> IO ()
printDFSi ud prt mainexp = computeWithDFS mainexp >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal in a depth-first manner:
computeWithDFS :: (NormalForm a, Show a) => (IDSupply -> a) -> IO (IOList a)
computeWithDFS mainexp = initSupply >>=
  \s -> searchDFS (`mcons` mnil) (id $!! (mainexp s))

searchDFS :: (Show a, NormalForm a) => (a -> IO (IOList b)) -> a -> IO (IOList b)
searchDFS cont a = searchDFS' cont (try a)

searchDFS' :: (Show a, NormalForm a) => (a -> IO (IOList b)) -> Try a -> IO (IOList b)
searchDFS' cont Fail             = mnil
searchDFS' cont (Val v)          = searchNF searchDFS cont v
searchDFS' cont (Choice i x1 x2) = lookupChoice i >>= choose
  where
    choose ChooseLeft  = searchDFS cont x1
    choose ChooseRight = searchDFS cont x2
    choose NoChoice    = newChoice ChooseLeft x1 +++ newChoice ChooseRight x2

    newChoice c x = do
      reset <- setUnsetChoice i c
      searchDFS cont x |< reset

searchDFS' cont (Free i xs) = lookupChoiceID i >>= choose
  where
    choose (LazyBind cs, _) = processLB cs
    choose (ChooseN c _, _) = searchDFS cont (xs !! c)
    choose (NoChoice   , j) = cont $ choicesCons j xs
    choose c                = error $ "Basics.searchDFS'.choose: " ++ show c

    processLB cs = do
      reset <- setUnsetChoice i NoChoice
      searchDFS cont (guardCons cs $ choicesCons i xs) |< reset

searchDFS' cont (Narrowed i@(NarrowedID pns _) xs) = lookupChoice i >>= choose
  where
    choose (LazyBind cs) = processLB cs
    choose (ChooseN c _) = searchDFS cont (xs !! c)
    choose NoChoice      = foldr1 (+++) $ zipWith3 newChoice [0 ..] xs pns
    choose c             = error $ "Basics.searchDFS'.choose: " ++ show c

    processLB cs = do
      reset <- setUnsetChoice i NoChoice
      searchDFS cont (guardCons cs $ choicesCons i xs) |< reset

    newChoice n x pn = do
      reset <- setUnsetChoice i (ChooseN n pn)
      searchDFS cont x |< reset

searchDFS' cont (Guard cs e) = solves cs >>= traverse
  where
    traverse FailST               = mnil
    traverse (SuccessST reset)    = searchDFS cont e |< reset
    traverse (ChoiceST reset l r) =
      ((l >>= traverse) +++ (r >>= traverse)) |< reset
    traverse (ChoicesST reset cs) =
      foldr1 (+++) (map (>>= traverse) cs) |< reset

-- searchDFS (Guard cs e) = do
--   mreset <- solves cs
--   case mreset of
--     Nothing    -> mnil
--     Just reset -> (searchDFS . try) $!< e |< reset

-- ---------------------------------------------------------------------------
-- Breadth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- Print all values of a non-deterministic goal in a breadth-first manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printBFS :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printBFS prt mainexp = computeWithBFS mainexp >>= printAllValues prt

-- Print first value of a non-deterministic goal in a breadth-first manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printBFS1 :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printBFS1 prt mainexp = computeWithBFS mainexp >>= printOneValue prt

-- Print all values of a non-deterministic goal in a breadth-first manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printBFSi :: (NormalForm a, Show a) =>
             MoreDefault -> (a -> IO ()) -> (IDSupply -> a) -> IO ()
printBFSi ud prt mainexp = computeWithBFS mainexp >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal in a breadth-first manner:
computeWithBFS :: NormalForm a => (IDSupply -> a) -> IO (IOList a)
computeWithBFS mainexp =
  initSupply >>= \s -> searchBFS (try (id $!! (mainexp s)))

searchBFS :: (NormalForm a, NonDet a) => Try a -> IO (IOList a)
searchBFS x = bfs [] [] (return ()) (return ()) x
  where
    -- bfs searches the levels in alternating order, left to right and then
    -- right to left, TODO is this behavior desired?
    -- xs is the list of values to be processed in this level
    -- ys is the list of values to be processed in the next level
    bfs xs ys _   reset Fail           = reset >> next xs ys
    bfs xs ys _   reset (Val v)        = reset >> mcons v (next xs ys)
    bfs xs ys set reset (Choice i x y) = set   >> lookupChoice i >>= choose

     where
        choose ChooseLeft  = (bfs xs ys (return ()) reset . try) $!< x
        choose ChooseRight = (bfs xs ys (return ()) reset . try) $!< y
        choose NoChoice    = do
          reset
          next xs ((newSet ChooseLeft , newReset, x) :
                   (newSet ChooseRight, newReset, y) : ys)

        newSet c = set   >> setChoice i c
        newReset = reset >> setChoice i NoChoice

    bfs xs ys set reset (Narrowed i cs) = set   >> lookupChoice i >>= choose

     where
        choose (ChooseN c _) = (bfs xs ys (return ()) reset . try) $!< (cs !! c)
        choose NoChoice    = do
          reset
          next xs ((zipWith newChoice [0..] cs) ++ ys)
        newChoice n x = (newSet n, newReset, x)
        newSet n = set   >> setChoice i (ChooseN n errChoice)
        newReset = reset >> setChoice i NoChoice
        errChoice = error "propagation number used within non-free Choice"

    bfs xs ys set reset (Free i cs) = lookupChoice i >>= choose
      where
        choose (ChooseN c _) = (bfs xs ys (return ()) reset . try) $!< (cs !! c)
        choose NoChoice      = reset >> mcons (choicesCons i cs) (next xs ys)
{-
    bfs xs ys set reset (Guard cs e) = do
      mreset <- solves cs
      case mreset of
       Nothing    -> reset >> next xs ys
       Just newReset -> bfs xs ys set (newReset >> reset) -- (searchDFS . try) $!< e |< reset
-}
    next []  []                 = mnil
    next []  ((set,reset,y):ys) = (bfs ys [] set reset . try) $!< y
    next ((set,reset,x):xs) ys  = (bfs xs ys set reset . try) $!< x

-- ---------------------------------------------------------------------------
-- Iterative depth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- A function to increase the depth for the iterative deepening strategy
-- (here: double the depth after reaching the depth bound)
incrDepth4IDFS n = n*2

-- Print all values of an expression with iterative deepening where
-- the first argument is the initial depth size which will be increased
-- by function incrDepth4IDFS in each iteration:
-- The second argument is the operation to print a result (e.g., Prelude.print).
printIDS :: (NormalForm a, Show a) => Int -> (a -> IO ())
         -> (IDSupply -> a) -> IO ()
printIDS initdepth prt mainexp =
  computeWithIDS initdepth mainexp >>= printAllValues prt

-- Print one value of an expression with iterative deepening:
printIDS1 :: (NormalForm a, Show a) => Int -> (a -> IO ())
          -> (IDSupply -> a) -> IO ()
printIDS1 initdepth prt mainexp =
  computeWithIDS initdepth mainexp >>= printOneValue prt

-- Print all values on demand of an expression with iterative deepening:
printIDSi :: (NormalForm a, Show a) => MoreDefault -> Int -> (a -> IO ())
          -> (IDSupply -> a) -> IO ()
printIDSi ud initdepth prt mainexp =
  computeWithIDS initdepth mainexp >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal with a iterative
-- deepening strategy:
computeWithIDS :: (NormalForm a, Show a) => Int -> (IDSupply -> a)
                                         -> IO (IOList a)
computeWithIDS initdepth goal = initSupply >>= \s -> iter s 0 initdepth
 where
   iter s olddepth newdepth = startIDS (id $!! goal s) olddepth newdepth
                              ++++ iter s newdepth (incrDepth4IDFS newdepth)

-- start iterative deepening for a given depth intervall
startIDS :: (Show a,NonDet a) => a -> Int -> Int -> IO (IOList a)
startIDS exp olddepth newdepth = idsHNF newdepth exp
 where
 idsHNF n x = case try x of
  Val v -> if n<newdepth-olddepth then mcons x mnil else mnil
  Fail  -> mnil
  Choice i x1 x2 -> do
    c <- lookupChoice i
    case c of
      ChooseLeft  -> idsHNF n x1
      ChooseRight -> idsHNF n x2
      NoChoice -> if n > 0
                  then choose ChooseLeft x1 +++ choose ChooseRight x2
                  else abort
     where
      choose c x = do
       setChoice i c
       idsHNF (n - 1) x |< setChoice i NoChoice

-- ---------------------------------------------------------------------------
-- Parallel search by mapping search results into monadic structure
-- ---------------------------------------------------------------------------

-- Print all values of an expression in a parallel manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printPar :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printPar prt mainexp = computeWithPar mainexp >>= printAllValues prt

-- Print one value of an expression in a parallel manner:
printPar1 :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printPar1 prt mainexp = computeWithPar mainexp >>= printOneValue prt

-- Print all values on demand of an expression in a parallel manner:
printPari :: (NormalForm a, Show a) =>
             MoreDefault -> (a -> IO ()) -> (IDSupply -> a) -> IO ()
printPari ud prt mainexp = computeWithPar mainexp >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal in a parallel manner:
computeWithPar :: NormalForm a => (IDSupply -> a) -> IO (IOList a)
computeWithPar mainexp = do
  s <- initSupply
  list2iolist (parSearch (searchMPlus (mainexp s)))

type SetOfChoices = Data.Map.Map Integer Choice

lookupChoiceRaw' :: Monad m => ID ->  StateT SetOfChoices m Choice
lookupChoiceRaw' r = do
   set <- get
   maybe (return NoChoice) return (Data.Map.lookup (mkInt r) set)

lookupChoice' :: Monad m => ID ->  StateT SetOfChoices m Choice
lookupChoice' r = fst `liftM` lookupChoiceID' r
lookupID' :: Monad m => ID ->  StateT SetOfChoices m ID
lookupID' r = snd `liftM`lookupChoiceID' r

lookupChoiceID' :: Monad m => ID -> StateT SetOfChoices m (Choice,ID)
lookupChoiceID' r = do
  cr <- lookupChoiceRaw' r
  case cr of
   BoundTo j _ -> lookupChoiceID' j
   BindTo j -> do
     (cj,k) <- lookupChoiceID' j
     case cj of
       ChooseN n pn -> do
          propagateBind' r j pn
          return (cj,k)
       c            -> return (c,k)
   c        -> return (c,r)

setChoiceRaw' :: Monad m => ID -> Choice -> StateT SetOfChoices m ()
setChoiceRaw' r c = modify (Data.Map.insert (mkInt r) c)

setChoice' :: Monad m => ID -> Choice -> StateT SetOfChoices m ()
setChoice' r (BindTo j) | mkInt r == mkInt j = return ()
setChoice' r c =  lookupChoiceRaw' r >>= unchain
 where
   unchain (BindTo k) = do
     setChoice' k c
     case c of
       (ChooseN _ pNum) -> propagateBind' r k pNum
       _                -> return ()
   unchain (BoundTo k _) =
     error "setChoice'.unchain: bound Variable should not be rebound"
   unchain oldChoice =
     case c of
       BindTo j -> do
         lastId <- lookupID' j
         if mkInt lastId == mkInt r
            then return ()
            else setChoiceRaw' r c
       _ -> setChoiceRaw' r c

propagateBind' i j pn = do
  zipWithM_ (\childr childj -> setChoice' childr (BindTo childj))
            (nextNIDs i pn) (nextNIDs j pn)
  setChoiceRaw' i (BoundTo j pn)

solves' :: MonadPlus m => [Constraint] -> StateT SetOfChoices m ()
solves' []     = return ()
solves' (c:cs) = solve c >> solves' cs
 where
  solve Unsolvable = mzero
  solve (i :=: cc) = lookupChoice' i >>= choose cc
    where
     -- store lazy binds for later use
     choose (LazyBind  _) NoChoice      = setChoice' i cc
     -- solve stored lazy binds when they are needed
     choose _             (LazyBind cs) = setChoice' i cc >> solves' cs
     choose (LazyBind cs) _             = solves' cs
     choose (BindTo j)    ci            = lookupChoice' j >>= check i j ci
     choose c             NoChoice      = setChoice' i c
     choose c             ci            = guard (c == ci)

     -- Check whether i can be bound to j and do so if possible
     check :: MonadPlus m => ID -> ID -> Choice -> Choice -> StateT SetOfChoices m ()
     check i j _        (LazyBind cs)
       = setChoice' j (BindTo i) >> solves' cs
     check i j NoChoice _
       = setChoice' i (BindTo j)
     check i j _        NoChoice
       = setChoice' j (BindTo i)
     check i j (ChooseN iN ip) (ChooseN jN jp) =
       if iN == jN && ip == jp
       then solves' (zipWith (\childi childj -> childi :=: BindTo childj)
                     (nextNIDs i ip) (nextNIDs j ip))
       else mzero
     check _ _ ci       cj                     = guard (ci == cj)
  solve (ConstraintChoice i lcs rcs) = lookupChoice' i >>= chooseCC
   where
    chooseCC ChooseLeft  = solves' lcs
    chooseCC ChooseRight = solves' rcs
    chooseCC NoChoice    = (setChoice' i ChooseLeft >> solves' lcs)
                           `mplus`
                           (setChoice' i ChooseRight >> solves' rcs)
    chooseCC c           = error $ "solves'.solve.chooseCC: " ++ show c
  solve (ConstraintChoices i css) = lookupChoice' i >>= chooseCCs
   where
    chooseCCs (ChooseN c _) = solves' (css !! c)
    chooseCCs NoChoice      = msum $ zipWith mkChoice [0 ..] css
    chooseCCs c             = error $ "ID.solve.chooseCCs: " ++ show c

    mkChoice n cs =  setChoice' i (ChooseN n (-1)) >> solves' cs




-- Collect results of a non-deterministic computation in a monadic structure.

searchMPlus :: (NormalForm a, MonadPlus m) => a -> m a
searchMPlus x = evalStateT (searchMPlus' return (id $!! x)) Data.Map.empty

searchMPlus' :: (NormalForm a, MonadPlus m) =>
                (a -> StateT SetOfChoices m b)
                -> a
                -> StateT SetOfChoices m b
searchMPlus' cont = searchMPlus'' cont . try

searchMPlus'' :: (NormalForm a, MonadPlus m) =>
                (a -> StateT SetOfChoices m b)
                -> Try a
                -> StateT SetOfChoices m b
searchMPlus'' _   Fail           = mzero
searchMPlus'' cont (Val v)        = searchNF searchMPlus' cont v
searchMPlus'' cont (Choice i x y) = lookupChoice' i >>= choose
  where
    choose ChooseLeft  = searchMPlus' cont x
    choose ChooseRight = searchMPlus' cont y
    choose NoChoice    = (setChoice' i ChooseLeft  >> searchMPlus' cont x)
                         `mplus`
                         (setChoice' i ChooseRight >> searchMPlus' cont y)
searchMPlus'' cont (Narrowed i@(NarrowedID pns _)  branches) =
   lookupChoice' i >>= choose
  where
    choose (ChooseN c _) = searchMPlus' cont (branches !! c)
    choose NoChoice      =
      msum $ zipWith3 (\n c pn -> pick n pn >> searchMPlus' cont c) [0..] branches pns
    choose (LazyBind cs) = processLazyBind' i cont cs branches
    pick c pn = setChoice' i (ChooseN c pn)

searchMPlus'' cont (Free i branches) = lookupChoice' i >>= choose
  where
    choose (ChooseN c _) = searchMPlus' cont (branches !! c)
    choose NoChoice      = cont $ choicesCons i branches
    choose (LazyBind cs) = processLazyBind' i cont cs branches

searchMPlus'' cont  (Guard cs e) =
  solves' cs >> searchMPlus' cont e


processLazyBind' i cont cs branches = do
  setChoice' i NoChoice
  searchMPlus' cont (guardCons cs (choicesCons i branches))
----------------------------------------------------------------------
-- Auxiliary Functions
----------------------------------------------------------------------

-- Operation to print the result of the main goal with bindings of free
-- variables in the goal. Since the formatting is defined in the Curry
-- module lib/ShowBindings, we strip here only the surrounding quotes.
printWithBindings :: Show a => a -> IO ()
printWithBindings x = printWithoutLastChar (tail (show x))
 where printWithoutLastChar [] = putChar '\n'
       printWithoutLastChar [_] = putChar '\n'
       printWithoutLastChar (c:cs) = putChar c >> printWithoutLastChar cs

-- mapM1 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM1 f as = sequence1 (map f as)

-- mapM1_ :: Monad m => (a -> m b) -> [a] -> m ()
-- mapM1_ f as = sequence1_ (map f as)

-- sequence1 :: Monad m => [m a] -> m [a]
-- sequence1 [act] = act >>= return . (:[])
-- sequence1 (a:as) = a >>= \a' -> sequence1 as >>= return .(a':)


-- sequence1_ :: Monad m => [m a] -> m ()
-- sequence1_ [act] = act >> return ()
-- sequence1_ (a:as) = a >> sequence1_ as