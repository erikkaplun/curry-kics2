{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Search where

import Control.Monad
import Control.Monad.State.Strict
import Control.Parallel.TreeSearch
import Data.List (intercalate)
import qualified Data.Map as Map

import ConstStore
import ID
import IDSupply
import MonadList
import PrimTypes
import Solver (solves)
import Types

toIO :: C_IO a -> ConstStore -> IO a
toIO (C_IO io)           _     = io
toIO Fail_C_IO           _     = error "toIO: failed"
toIO (Choice_C_IO _ _ _) _     = error "toIO: Non-determinism in IO occured"
toIO (Guard_C_IO cs e)   store = do
  mbSolution <- solves (getConstrList cs) e
  case mbSolution of
    Nothing       -> error "toIO (Guard): failed"
    Just (_, val) -> do
      -- add the constraint to the global constraint store to make it
      -- available to subsequent operations.
      -- This is valid because no backtracking is done in IO
      addToGlobalCs cs
      toIO val (cs `addCs` store)

-- TODO@fre: lookup value in constraint map and global map?
toIO (Choices_C_IO   (ChoiceID     _)  _) _  = error "choices with ChoiceID"
toIO (Choices_C_IO i@(NarrowedID _ _) xs) cs = followToIO i xs cs
toIO (Choices_C_IO i@(FreeID     _ _) xs) cs = do
  -- bindings of free variables are looked up first in the local
  -- constraint store and then in the global constraint store
  lookupCs cs i (flip toIO cs) tryGlobal
 where
  tryGlobal = do
    csg <- lookupGlobalCs
    lookupCs csg i (flip toIO cs) (followToIO i xs cs)

followToIO :: ID -> [C_IO a] -> ConstStore -> IO a
followToIO i xs store =  do
  c <- lookupChoice i
  case c of
    ChooseN idx _ -> toIO (xs !! idx) store
    NoChoice      -> error "toIO (Choices): Non-determinism in IO occured"
    LazyBind cs   -> toIO (guardCons (StructConstr cs) (choicesCons i xs)) store
    _             -> error $ "followToIO: " ++ show c

fromIO :: IO a -> C_IO a
fromIO io = C_IO io

-- |This function is used to print the main expression with the bindings of
-- free variables.
--
-- The REPL translates an expression "exp where x1,...,xn free" into the main
-- expression (exp,["x1",...,"xn"],x1,...,xn).
-- The normal form of this expression is then printed in a nicely readable
-- form.
printWithBindings :: Show a => [(String, String)] -> a -> IO ()
printWithBindings []       result = print result
printWithBindings bindings result = putStrLn $
  "{" ++ intercalate ", " (map (\(n, v) -> n ++ " = " ++ v) bindings) ++ "} "
  ++ show result

-- ---------------------------------------------------------------------------
-- Simple evaluation without search or normal form computation
-- ---------------------------------------------------------------------------

type DetExpr    a =             ConstStore -> a
type NonDetExpr a = IDSupply -> ConstStore -> a

getNormalForm :: NormalForm a => NonDetExpr a -> IO a
getNormalForm goal = do
  s <- initSupply
  return $ const $!! goal s emptyCs $ emptyCs

 -- |Evaluate a deterministic expression without search
evalD :: Show a => DetExpr a -> IO ()
evalD goal = print (goal emptyCs)

 -- |Evaluate a non-deterministic expression without search
eval :: Show a => NonDetExpr a -> IO ()
eval goal = initSupply >>= \s -> print (goal s emptyCs)

evalDIO :: NormalForm a => DetExpr (C_IO a) -> IO ()
evalDIO goal = toIO (goal emptyCs) emptyCs >> return ()

-- TODO: switch back to computeWithDFS
evalIO :: NormalForm a => NonDetExpr (C_IO a) -> IO ()
evalIO goal = initSupply >>= \s -> toIO (goal s emptyCs) emptyCs >> return ()

-- evalIO goal = computeWithDFS goal >>= execIOList
-- execIOList :: IOList (C_IO a) -> IO ()
-- execIOList MNil                 = return ()
-- execIOList (MCons xact getRest) = toIO xact emptyCs >> getRest >>= execIOList
-- execIOList (WithReset l _)      = l >>= execIOList
-- execIOList Abort                = return ()

-- ---------------------------------------------------------------------------
-- Evaluate a nondeterministic expression (thus, requiring some IDSupply)
-- and print the choice structure of all results.

prtChoices :: NormalForm a => NonDetExpr a -> IO ()
prtChoices goal = getNormalForm goal >>= print

-- ---------------------------------------------------------------------------
-- Printing all results of a computation in a depth-first manner
-- ---------------------------------------------------------------------------

-- |Evaluate a non-deterministic expression (thus, requiring some IDSupply)
-- and print all results in depth-first order.
--
-- The first argument is the operation to print a result (e.g., Prelude.print)
-- which is internally expanded to apply the constructors encountered during
-- search.
prdfs :: NormalForm a => (a -> IO ()) -> NonDetExpr a -> IO ()
prdfs cont goal = getNormalForm goal >>= printValsDFS False cont

-- The first argument backTrack indicates whether backtracking is needed
printValsDFS :: NormalForm a => Bool -> (a -> IO ()) -> a -> IO ()
printValsDFS backTrack cont =
  match prChoice prNarrowed prFree prFail prGuard prVal
  where
  prFail = return ()
  prVal = searchNF (printValsDFS backTrack) cont
  prChoice i x y = lookupChoice i >>= choose
    where
    choose ChooseLeft  = printValsDFS backTrack cont x
    choose ChooseRight = printValsDFS backTrack cont y
    choose NoChoice    =
     if backTrack
     then do
      newChoice True ChooseLeft  x
      newChoice True ChooseRight y
      setChoice i NoChoice
     else do
      newChoice True  ChooseLeft x
      newChoice False ChooseRight y
      where
        -- Assumption 1: Binary choices can only be set to one of
        -- [NoChoice, ChooseLeft, ChooseRight], therefore the reset action may
        -- be ignored in between
      newChoice bt c a = setChoice i c >> printValsDFS bt cont a
    choose c           = error $ "Basics.printValsDFS.choose: " ++ show c

  prFree i xs   = lookupChoiceID i >>= choose
    where
    choose (LazyBind cs, _) = processLazyBind backTrack cs i xs
                                              (printValsDFS backTrack cont)
    choose (ChooseN c _, _) = printValsDFS backTrack cont (xs !! c)
    choose (NoChoice   , j) = cont $ choicesCons j xs
    choose c                = error $ "Basics.printValsDFS.choose: " ++ show c

  prNarrowed i@(NarrowedID pns _) xs = lookupChoice i >>= choose
    where
    choose (LazyBind cs) = processLazyBind backTrack cs i xs (printValsDFS backTrack cont)
    choose (ChooseN c _) = printValsDFS backTrack cont (xs !! c)
    choose NoChoice      =
      if backTrack then do
       foldr1 (>>) $ zipWith3 (newChoice True) [0 ..] xs pns
       setChoice i NoChoice
      else do
       foldr1 (>>) $ zipWithButLast3 (newChoice True) (newChoice False) [0 ..] xs pns
      where
      newChoice bt n a pn = setChoice i (ChooseN n pn) >> printValsDFS bt cont a
    choose c            = error $ "Basics.printValsDFS.choose: " ++ show c
  prNarrowed i _ = error $ "prDFS: Bad narrowed ID " ++ show i

  prGuard cs e = solves (getConstrList cs) e >>= \mbSolution -> case mbSolution of
    Nothing          -> return ()
    Just (reset, e') | backTrack -> printValsDFS True  cont e' >> reset
                     | otherwise -> printValsDFS False cont e'

processLazyBind :: NonDet a => Bool -> [Constraint] -> ID -> [a] -> (a -> IO ()) -> IO ()
processLazyBind True cs i xs search = do
  reset <- setUnsetChoice i NoChoice
  search $ guardCons (StructConstr cs) $ choicesCons i xs
  reset
processLazyBind False cs i xs search = do
  setChoice i NoChoice
  search $ guardCons (StructConstr cs) $ choicesCons i xs

-- |Apply the first ternary function to the zipping of three lists, but
-- take the second function for the last triple.
zipWithButLast3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
                -> [a] -> [b] -> [c] -> [d]
zipWithButLast3 _ _     []     _      _   = []
zipWithButLast3 _ _      _     []     _   = []
zipWithButLast3 _ _      _     _      []  = []
zipWithButLast3 _ f' (a:[]) (b:_ ) (c:_)  = f' a b c : []
zipWithButLast3 _ f' (a:_ ) (b:[]) (c:_)  = f' a b c : []
zipWithButLast3 _ f' (a:_ ) (b:_ ) (c:[]) = f' a b c : []
zipWithButLast3 f f' (a:as) (b:bs) (c:cs) = f  a b c :
                                            zipWithButLast3 f f' as bs cs
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
computeWithDFS goal = getNormalForm goal >>= searchDFS msingleton

searchDFS :: NormalForm a => (a -> IO (IOList b)) -> a -> IO (IOList b)
searchDFS cont a = match dfsChoice dfsNarrowed dfsFree mnil dfsGuard dfsVal a
  where
  dfsVal = searchNF searchDFS cont
  dfsChoice i x1 x2 = lookupChoice i >>= choose
    where
    choose ChooseLeft  = searchDFS cont x1
    choose ChooseRight = searchDFS cont x2
    choose NoChoice    = newChoice ChooseLeft x1 +++ newChoice ChooseRight x2
    choose c           = error $ "Basics.searchDFS.choose: " ++ show c

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
computeWithBFS goal = getNormalForm goal >>= searchBFS msingleton

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
      choose (ChooseN c _)  = bfs cont xs ys (return ()) reset (cs !! c)
      choose NoChoice       = do
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
computeWithIDS initDepth goal = getNormalForm goal >>= iter 0 initDepth
  where
  iter oldDepth newDepth expr = startIDS oldDepth newDepth expr
       ++++ iter newDepth (incrDepth4IDFS newDepth) expr
--   initSupply >>= \s -> iter s 0 initdepth
--   where
--   iter s olddepth newdepth = startIDS ((const $!!) (goal s emptyCs) emptyCs) olddepth newdepth
--                               ++++ iter s newdepth (incrDepth4IDFS newdepth)

-- start iterative deepening for a given depth interval
startIDS :: NormalForm a => Int -> Int -> a -> IO (IOList a)
startIDS olddepth newdepth goal = idsHNF newdepth goal
  where
  idsHNF n = match idsChoice idsNarrowed idsFree mnil idsGuard idsVal
    where
    idsVal v | n < newdepth - olddepth = msingleton v
            | otherwise               = mnil

    idsChoice i x1 x2 = lookupChoice i >>= choose
      where
      choose ChooseLeft  = idsHNF n x1
      choose ChooseRight = idsHNF n x2
      choose NoChoice
        | n > 0          = newChoice ChooseLeft x1 +++ newChoice ChooseRight x2
        | otherwise      = abort
        where
        newChoice c y = do
          reset <- setUnsetChoice i c
          idsHNF (n - 1) y |< reset
      choose c           = error $ "idsHNF: Bad choice " ++ show c

    idsFree i xs = lookupChoiceID i >>= choose
      where
      choose (LazyBind cs, _) = processLB cs
      choose (ChooseN c _, _) = idsHNF n (xs !! c)
      choose (NoChoice   , j) = msingleton $ choicesCons j xs
      choose c                = error $ "Basics.searchDFS.choose: " ++ show c

      processLB cs = do
        reset <- setUnsetChoice i NoChoice
        idsHNF n (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

    idsNarrowed i@(NarrowedID pns _) xs = lookupChoice i >>= choose
      where
      choose (LazyBind cs) = processLB cs
      choose (ChooseN c _) = idsHNF n (xs !! c)
      choose NoChoice      = foldr1 (+++) $ zipWith3 newChoice [0 ..] xs pns
      choose c             = error $ "idsHNF.choose: " ++ show c

      processLB cs = do
        reset <- setUnsetChoice i NoChoice
        idsHNF n (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

      newChoice m y pn = do
        reset <- setUnsetChoice i (ChooseN m pn)
        idsHNF (n - 1) y |< reset
    idsNarrowed i _ = error $ "idsHNF: Bad narrowed ID " ++ show i

    idsGuard cs e = solves (getConstrList cs) e >>= \mbSolution -> case mbSolution of
      Nothing          -> mnil
      Just (reset, e') -> idsHNF n e' |< reset

-- ---------------------------------------------------------------------------
-- Parallel search by mapping search results into monadic structure
-- ---------------------------------------------------------------------------

-- Print all values of an expression in a parallel manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printPar :: NormalForm a => (a -> IO ()) -> NonDetExpr a -> IO ()
printPar prt goal = computeWithPar goal >>= printAllValues prt

-- Print one value of an expression in a parallel manner:
printPar1 :: NormalForm a => (a -> IO ()) -> NonDetExpr a -> IO ()
printPar1 prt goal = computeWithPar goal >>= printOneValue prt

-- Print all values on demand of an expression in a parallel manner:
printPari :: NormalForm a => MoreDefault -> (a -> IO ()) -> NonDetExpr a -> IO ()
printPari ud prt goal = computeWithPar goal >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal in a parallel manner:
computeWithPar :: NormalForm a => NonDetExpr a -> IO (IOList a)
computeWithPar goal = getNormalForm goal >>=
  list2iolist . parSearch . flip evalStateT (Map.empty :: SetOfChoices)
                          . searchMPlus' return

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
