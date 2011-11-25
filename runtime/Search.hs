{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Search where

import Control.Monad
import Control.Monad.State.Strict
import Control.Parallel.TreeSearch
import qualified Data.Map as Map

import ID
import IDSupply
import MonadList
import PrimTypes
import Solver (solves)
import Types

toIO :: C_IO a -> ConstStore -> IO a
toIO (C_IO io) _ = io
toIO Fail_C_IO _ = error "toIO: failed"
toIO (Choice_C_IO _ _ _) _ = error "toIO: Non-determinism in IO occured"
toIO (Guard_C_IO constraints e) cs = do
  mbSolution <- solves (getConstrList constraints) e
  case mbSolution of
    Nothing       -> error "toIO (Guard): failed"
    Just (_, val) -> do
      -- adds the constraint to the global constraint store
      -- to make it available to subsequent operations
      -- this is safe because no backtracking is done in IO
      addToGlobalCs constraints
      toIO val $! (combConstr constraints cs)

-- TODO: lookup value in constraint map and global map?
toIO (Choices_C_IO (ChoiceID _) _) _ = error "choices with ChoiceID"
toIO (Choices_C_IO i@(NarrowedID _ _) choices) cs = followToIO i choices cs
toIO (Choices_C_IO i@(FreeID _ _) choices) cs = do
  -- bindings of free variables are looked up first in the local
  -- constraint store and then in the global constraint store
  lookupCs cs i (flip toIO cs) tryGlobal
 where
  tryGlobal = do
    csg <- lookupGlobalCs
    lookupCs csg i (flip toIO cs) (followToIO i choices cs)

followToIO :: ID -> [C_IO a] -> ConstStore -> IO a
followToIO i choices cs =  do
  c <- lookupChoice i
  case c of
    ChooseN idx _ -> toIO (choices !! idx) cs
    NoChoice -> error "toIO (Choices): Non-determinism in IO occured"
    LazyBind constraints -> toIO (guardCons (StructConstr constraints) (choicesCons i choices)) cs
    _ -> error $ "followToIO: " ++ show c

fromIO :: IO a -> C_IO a
fromIO io = C_IO io

-- ---------------------------------------------------------------------------
-- Simple evaluation without search
-- ---------------------------------------------------------------------------

type NonDetExpr a = IDSupply -> ConstStore -> a

eval :: Show a => (IDSupply -> a) -> IO ()
eval goal = initSupply >>= print . goal

evalD :: Show a => (ConstStore -> a) -> IO ()
evalD goal = print (goal emptyCs)

-- TODO: switch back to computeWithDFS
evalIO :: NormalForm a => (IDSupply -> ConstStore -> C_IO a) -> IO ()
evalIO goal = initSupply >>= \supp -> toIO  (goal supp emptyCs) emptyCs >> return ()

evalDIO :: NormalForm a => (ConstStore -> C_IO a) -> IO ()
evalDIO goal = toIO (goal emptyCs) emptyCs >> return ()

-- evalIO goal = computeWithDFS goal >>= execIOList
-- execIOList :: IOList (C_IO a) -> IO ()
-- execIOList MNil                 = return ()
-- execIOList (MCons xact getRest) = toIO xact emptyCs >> getRest >>= execIOList
-- execIOList (WithReset l _)      = l >>= execIOList
-- execIOList Abort                = return ()

-- ---------------------------------------------------------------------------
-- Evaluate a nondeterministic expression (thus, requiring some IDSupply)
-- and print the choice structure of all results.

prtChoices :: (Show a, NormalForm a) => NonDetExpr a -> IO ()
prtChoices mainexp = do
  s <- initSupply
  let ndvalue = (const $!! (mainexp s)) emptyCs
  putStrLn (show ndvalue)

-- ---------------------------------------------------------------------------
-- Printing all results of a computation in a depth-first manner
-- ---------------------------------------------------------------------------

-- Evaluate a nondeterministic expression (thus, requiring some IDSupply)
-- and print all results in depth-first order.
-- The first argument is the operation to print a result (e.g., Prelude.print)
-- which is internally expanded to apply the constructors encountered during
-- search.
prdfs :: NormalForm a => (a -> IO ()) -> NonDetExpr a -> IO ()
prdfs cont goal = initSupply >>= (printValsDFS cont . flip (const $!!) emptyCs . flip goal emptyCs)

printValsDFS :: NormalForm a => (a -> IO ()) -> a -> IO ()
printValsDFS cont = match prChoice prNarrowed prFree skip prGuard prVal
  where
  skip  = return ()
  prVal = searchNF printValsDFS cont
  prChoice i x y = lookupChoice i >>= choose
    where
    choose ChooseLeft  = printValsDFS cont x
    choose ChooseRight = printValsDFS cont y
    choose NoChoice    = do
      newChoice ChooseLeft  x
      newChoice ChooseRight y
      setChoice i NoChoice
      where
        -- Assumption 1: Binary choices can only be set to one of
        -- [NoChoice, ChooseLeft, ChooseRight], therefore the reset action may
        -- be ignored in between
      newChoice c a = setChoice i c >> printValsDFS cont a
    choose c           = error $ "Basics.printValsDFS.choose: " ++ show c

  prFree i xs   = lookupChoiceID i >>= choose
    where
    choose (LazyBind cs, _) = processLazyBind cs i xs (printValsDFS cont)
    choose (ChooseN c _, _) = printValsDFS cont (xs !! c)
    choose (NoChoice   , j) = cont $ choicesCons j xs
    choose c                = error $ "Basics.printValsDFS.choose: " ++ show c

  prNarrowed i@(NarrowedID pns _) xs = lookupChoice i >>= choose
    where
    choose (LazyBind cs) = processLazyBind cs i xs (printValsDFS cont)
    choose (ChooseN c _) = printValsDFS cont (xs !! c)
    choose NoChoice      = do
      foldr1 (>>) $ zipWith3 newChoice [0 ..] xs pns
      setChoice i NoChoice
      where
      newChoice n a pn = setChoice i (ChooseN n pn) >> printValsDFS cont a
    choose c           = error $ "Basics.printValsDFS.choose: " ++ show c
  prNarrowed i _ = error $ "prDFS: Bad narrowed ID " ++ show i

  prGuard cs e = solves (getConstrList cs) e >>= \mbSolution -> case mbSolution of
    Nothing          -> skip
    Just (reset, e') -> printValsDFS cont e' >> reset

processLazyBind :: NonDet a => [Constraint] -> ID -> [a] -> (a -> IO ()) -> IO ()
processLazyBind cs i xs search = do
  reset <- setUnsetChoice i NoChoice
  search $ guardCons (StructConstr cs) $ choicesCons i xs
  reset

-- ---------------------------------------------------------------------------
-- Depth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- Print all values of an expression in a depth-first manner.
-- The first argument is the operation to print a result (e.g., Prelude.print).
printDFS :: NormalForm a => (a -> IO ()) -> NonDetExpr a -> IO ()
printDFS prt goal = computeWithDFS goal >>= printAllValues prt

-- Print one value of an expression in a depth-first manner:
printDFS1 :: NormalForm a => (a -> IO ()) -> NonDetExpr a -> IO ()
printDFS1 prt goal = computeWithDFS goal >>= printOneValue prt

-- Print all values on demand of an expression in a depth-first manner:
printDFSi :: NormalForm a => MoreDefault -> (a -> IO ()) -> NonDetExpr a -> IO ()
printDFSi ud prt goal = computeWithDFS goal >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal in a depth-first manner:
computeWithDFS :: NormalForm a => NonDetExpr a -> IO (IOList a)
computeWithDFS goal = initSupply >>=
  searchDFS msingleton . flip (const $!!) emptyCs . flip goal emptyCs

searchDFS :: NormalForm a => (a -> IO (IOList b)) -> a -> IO (IOList b)
searchDFS cont a = match dfsChoice dfsNarrowed dfsFree mnil dfsGuard dfsVal a
  where
  dfsVal = searchNF searchDFS cont
  dfsChoice i x1 x2 = lookupChoice i >>= choose
    where
    choose ChooseLeft  = searchDFS cont x1
    choose ChooseRight = searchDFS cont x2
    choose NoChoice    = newChoice ChooseLeft x1 +++ newChoice ChooseRight x2
    choose c                = error $ "Basics.searchDFS.choose: " ++ show c

    newChoice c x = do
      reset <- setUnsetChoice i c
      searchDFS cont x |< reset

  dfsFree i xs = lookupChoiceID i >>= choose
    where
    choose (LazyBind cs, _) = processLB cs
    choose (ChooseN c _, _) = searchDFS cont (xs !! c)
    choose (NoChoice   , j) = cont $ choicesCons j xs
    choose c                = error $ "Basics.searchDFS.choose: " ++ show c

    processLB cs = do
      reset <- setUnsetChoice i NoChoice
      searchDFS cont (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

  dfsNarrowed i@(NarrowedID pns _) xs = lookupChoice i >>= choose
    where
    choose (LazyBind cs) = processLB cs
    choose (ChooseN c _) = searchDFS cont (xs !! c)
    choose NoChoice      = foldr1 (+++) $ zipWith3 newChoice [0 ..] xs pns
    choose c             = error $ "Basics.searchDFS'.choose: " ++ show c

    processLB cs = do
      reset <- setUnsetChoice i NoChoice
      searchDFS cont (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

    newChoice n x pn = do
      reset <- setUnsetChoice i (ChooseN n pn)
      searchDFS cont x |< reset
  dfsNarrowed i _ = error $ "searchDFS: Bad narrowed ID " ++ show i

  dfsGuard cs e = solves (getConstrList cs) e >>= \mbSolution -> case mbSolution of
    Nothing          -> mnil
    Just (reset, e') -> searchDFS cont e' |< reset

-- ---------------------------------------------------------------------------
-- Breadth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- Print all values of a non-deterministic goal in a breadth-first manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printBFS :: NormalForm a => (a -> IO ()) -> NonDetExpr a -> IO ()
printBFS prt goal = computeWithBFS goal >>= printAllValues prt

-- Print first value of a non-deterministic goal in a breadth-first manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printBFS1 :: NormalForm a => (a -> IO ()) -> NonDetExpr a -> IO ()
printBFS1 prt goal = computeWithBFS goal >>= printOneValue prt

-- Print all values of a non-deterministic goal in a breadth-first manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printBFSi :: NormalForm a => MoreDefault -> (a -> IO ()) -> NonDetExpr a -> IO ()
printBFSi ud prt goal = computeWithBFS goal >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal in a breadth-first manner:
computeWithBFS :: NormalForm a => NonDetExpr a -> IO (IOList a)
computeWithBFS goal = initSupply >>=
  searchBFS msingleton . flip (const $!!) emptyCs . flip goal emptyCs

searchBFS :: NormalForm a => (a -> IO (IOList b)) -> a -> IO (IOList b)
searchBFS act x = bfs act [] [] (return ()) (return ()) x
  where
  -- bfs searches the levels in alternating order, left to right and then
  -- right to left, TODO is this behavior desired?
  -- xs is the list of values to be processed in this level
  -- ys is the list of values to be processed in the next level
  bfs cont xs ys set reset =
    match bfsChoice bfsNarrowed bfsFree bfsFail bfsGuard bfsVal
    where
    bfsFail  = reset >> next cont xs ys
    bfsVal v = (set >> searchNF searchBFS cont v) +++ (reset >> next cont xs ys)
    bfsChoice i a b = set >> lookupChoice i >>= choose
      where
      choose ChooseLeft  = bfs cont xs ys (return ()) reset a
      choose ChooseRight = bfs cont xs ys (return ()) reset b
      choose NoChoice    = do
        reset
        next cont xs ((newSet ChooseLeft , newReset, a) :
                (newSet ChooseRight, newReset, b) : ys)
      choose c             = error $ "Basics.searchDFS: Bad choice " ++ show c

      newSet c = set   >> setChoice i c
      newReset = reset >> setChoice i NoChoice

    bfsNarrowed i@(NarrowedID pns _) cs = set >> lookupChoice i >>= choose
      where
      choose (LazyBind cns) = processLB cns
      choose (ChooseN c _) = bfs cont xs ys (return ()) reset (cs !! c)
      choose NoChoice      = do
        reset
        next cont xs (zipWith3 newChoice [0..] cs pns ++ ys)
      choose c             = error $ "Basics.searchDFS: Bad choice " ++ show c

      newChoice n y pn = ( set >> setChoice i (ChooseN n pn)
                         , reset >> setChoice i NoChoice
                         , y)

      processLB cns = do
        newReset <- setUnsetChoice i NoChoice
        bfs cont xs ys (return ()) (reset >> newReset) (guardCons (StructConstr cns) (choicesCons i cs))
    bfsNarrowed i _ = error $ "searchBFS: Bad narrowed ID " ++ show i

    bfsFree i cs = set >> lookupChoice i >>= choose
      where
      choose (LazyBind cns) = processLB cns
      choose (ChooseN c _) = bfs cont xs ys (return ()) reset (cs !! c)
      choose NoChoice      = reset >> cont (choicesCons i cs) +++ (next cont xs ys)
      choose c             = error $ "Basics.searchBFS.choose: " ++ show c

      processLB cns = do
        newReset <- setUnsetChoice i NoChoice
        bfs cont xs ys (return ()) (reset >> newReset) (guardCons (StructConstr cns) (choicesCons i cs))

    bfsGuard cs e = set >> solves (getConstrList cs) e >>= \mbSolution -> case mbSolution of
      Nothing            -> reset >> next cont xs ys
      Just (newReset, a) -> bfs cont xs ys (return ()) (newReset >> reset) a

    next _     []  []                   = mnil
    next cont' []  ((setB,resetB,b):bs) = bfs cont' bs [] setB resetB b
    next cont' ((setA,resetA,a):as) bs  = bfs cont' as bs setA resetA a

-- ---------------------------------------------------------------------------
-- Iterative depth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- A function to increase the depth for the iterative deepening strategy
-- (here: double the depth after reaching the depth bound)
incrDepth4IDFS :: Int -> Int
incrDepth4IDFS n = n*2

-- Print all values of an expression with iterative deepening where
-- the first argument is the initial depth size which will be increased
-- by function incrDepth4IDFS in each iteration:
-- The second argument is the operation to print a result (e.g., Prelude.print).
printIDS :: NormalForm a => Int -> (a -> IO ()) -> NonDetExpr a -> IO ()
printIDS initdepth prt goal = computeWithIDS initdepth goal >>= printAllValues prt

-- Print one value of an expression with iterative deepening:
printIDS1 :: NormalForm a => Int -> (a -> IO ()) -> NonDetExpr a -> IO ()
printIDS1 initdepth prt goal = computeWithIDS initdepth goal >>= printOneValue prt

-- Print all values on demand of an expression with iterative deepening:
printIDSi :: NormalForm a => MoreDefault -> Int -> (a -> IO ()) -> NonDetExpr a -> IO ()
printIDSi ud initdepth prt goal = computeWithIDS initdepth goal >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal with a iterative
-- deepening strategy:
computeWithIDS :: NormalForm a => Int -> NonDetExpr a -> IO (IOList a)
computeWithIDS initdepth goal = initSupply >>= \s -> iter s 0 initdepth
  where
  iter s olddepth newdepth = startIDS ((const $!!) (goal s emptyCs) emptyCs) olddepth newdepth
                              ++++ iter s newdepth (incrDepth4IDFS newdepth)

-- start iterative deepening for a given depth intervall
startIDS :: (Show a,NonDet a) => a -> Int -> Int -> IO (IOList a)
startIDS goal olddepth newdepth = idsHNF newdepth goal
  where
  idsHNF n x = match idsChoice idsNarrowed idsFree mnil idsGuard idsVal x
    where
    idsVal v = if n < newdepth - olddepth then msingleton v else mnil
    idsChoice i x1 x2 = do
      c <- lookupChoice i
      case c of
        ChooseLeft  -> idsHNF n x1
        ChooseRight -> idsHNF n x2
        NoChoice    -> if n > 0
                        then choose ChooseLeft x1 +++ choose ChooseRight x2
                        else abort
        _           -> error $ "startIDS: Bad choice " ++ show c
      where
      choose c y = do
      setChoice i c
      idsHNF (n - 1) y |< setChoice i NoChoice
  idsNarrowed = error "IDS not implemented for free variables"
  idsFree     = error "IDS not implemented for free variables"
  idsGuard    = error "IDS not implemented for equational Constraints"

-- ---------------------------------------------------------------------------
-- Parallel search by mapping search results into monadic structure
-- ---------------------------------------------------------------------------

-- Print all values of an expression in a parallel manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printPar :: NormalForm a => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printPar prt mainexp = computeWithPar mainexp >>= printAllValues prt

-- Print one value of an expression in a parallel manner:
printPar1 :: NormalForm a => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printPar1 prt mainexp = computeWithPar mainexp >>= printOneValue prt

-- Print all values on demand of an expression in a parallel manner:
printPari :: NormalForm a => MoreDefault -> (a -> IO ()) -> (IDSupply -> a) -> IO ()
printPari ud prt mainexp = computeWithPar mainexp >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal in a parallel manner:
computeWithPar :: NormalForm a => (IDSupply -> a) -> IO (IOList a)
computeWithPar mainexp = initSupply >>=
  list2iolist . parSearch . flip searchMPlus emptyCs . mainexp

-- ---------------------------------------------------------------------------
-- Generic search using MonadPlus as the result monad
-- ---------------------------------------------------------------------------

type SetOfChoices = Map.Map Integer Choice

instance Monad m => Store (StateT SetOfChoices m) where
  getChoiceRaw u        = gets $ Map.findWithDefault defaultChoice (mkInteger u)
  setChoiceRaw u c
    | isDefaultChoice c = modify $ Map.delete (mkInteger u)
    | otherwise         = modify $ Map.insert (mkInteger u) c
  unsetChoiceRaw u      = modify $ Map.delete (mkInteger u)

-- Collect results of a non-deterministic computation in a monadic structure.
searchMPlus :: (MonadPlus m, NormalForm a) => a -> ConstStore -> m a
searchMPlus x store = evalStateT
                      (searchMPlus' return ((const $!!) x store))
                      (Map.empty :: SetOfChoices)

searchMPlus' :: (NormalForm a, MonadPlus m, Store m) => (a -> m b) -> a -> m b
searchMPlus' cont = match smpChoice smpNarrowed smpFree mzero smpGuard smpVal
  where
  smpVal v = searchNF searchMPlus' cont v
  smpChoice i x y = lookupChoice i >>= choose
    where
    choose ChooseLeft  = searchMPlus' cont x
    choose ChooseRight = searchMPlus' cont y
    choose NoChoice    = (setChoice i ChooseLeft  >> searchMPlus' cont x)
                         `mplus`
                         (setChoice i ChooseRight >> searchMPlus' cont y)
    choose c           = error $ "searchMPlus: Bad choice " ++ show c

  smpNarrowed i@(NarrowedID pns _)  bs = lookupChoice i >>= choose
    where
    choose (ChooseN c _) = searchMPlus' cont (bs !! c)
    choose NoChoice      =
      msum $ zipWith3 (\n c pn -> pick n pn >> searchMPlus' cont c) [0..] bs pns
    choose (LazyBind cs) = processLazyBind' i cont cs bs
    choose c             = error $ "searchMPlus: Bad choice " ++ show c

    pick c pn = setChoice i (ChooseN c pn)
  smpNarrowed i _ = error $ "searchMPlus: Bad narrowed ID " ++ show i

  smpFree i branches = lookupChoice i >>= choose
    where
    choose (ChooseN c _) = searchMPlus' cont (branches !! c)
    choose NoChoice      = cont $ choicesCons i branches
    choose (LazyBind cs) = processLazyBind' i cont cs branches
    choose c             = error $ "searchMPlus: Bad choice " ++ show c

  smpGuard cs e = solves (getConstrList cs) e >>= \mbSolution -> case mbSolution of
    Nothing      -> mzero
    Just (_, e') -> searchMPlus' cont e'

processLazyBind' :: (NormalForm a, MonadPlus m, Store m)
                 => ID -> (a -> m b) -> [Constraint] -> [a] -> m b
processLazyBind' i cont cs xs = do
  setChoice i NoChoice
  searchMPlus' cont (guardCons (StructConstr cs) (choicesCons i xs))
