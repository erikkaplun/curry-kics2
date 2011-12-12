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
import Debug

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
  c <- lookupDecision i
  case c of
    ChooseN idx _ -> toIO (xs !! idx) store
    NoDecision      -> error "toIO (Choices): Non-determinism in IO occured"
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

-- Evaluate a nondeterministic expression (thus, requiring some IDSupply)
-- and print the choice structure of all results as a tree.
prtChoiceTree :: NormalForm a => NonDetExpr a -> IO ()
prtChoiceTree goal = getNormalForm goal >>= putStrLn . showChoiceTree

showChoiceTree :: (Show a, NonDet a) => a -> String
showChoiceTree = ($[]) . showsTree [] "" . try
  where
  -- showsTree n l <tryVal>, where
  --   * @l@ is a stack of flags whether we show the last alternative
  --         of the respective level (for drawing aesthetical corners)
  --   * @k@ is the key for the decision (L, R or the constructor index)
  showsTree l k (Val v)
    = indent l k . showString "Val " . shows v . nl
  showsTree l k Fail
    = indent l k . showChar '!' . nl
  showsTree l k (Choice  i x y)
    = indent l k . shows i . nl
    . showsChildren l [("L", try x), ("R", try y)]
  showsTree l k (Narrowed i xs)
    = indent l k . shows i . nl
    . showsChildren l (zip (map show [(0 :: Int) ..]) (map try xs))
  showsTree l k (Free     i xs)
    = indent l k . shows i . nl
    . showsChildren l (zip (map show [(0 :: Int) ..]) (map try xs))
  showsTree l k (Guard    cs e)
    = indent l k . shows cs . nl
    . showsChildren l [("", try e)]

  indent []      _ = id
  indent (hd:tl) k = showString (concatMap showLines $ reverse tl)
                   . showChar (if hd then llc else lmc)
                   . showString (hbar:hbar:" ")
                   . showKey k
    where showLines inLast = (if inLast then ' ' else vbar) : "       "

  showKey "" = id
  showKey k  = showString k . showString ": "

  vbar = '\x2502'     -- vertical bar
  hbar = '\x2500'     -- horizontal bar
  llc  = '\x2514'     -- left lower corner
  lmc  = '\x251c'     -- left middle corner
  nl   = showChar '\n'-- newline :)

  showsChildren _ []          = id
  showsChildren l [(k,v)]     = showsTree (True :l) k v
  showsChildren l ((k,v):kvs) = showsTree (False:l) k v . showsChildren l kvs

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
printValsDFS backTrack cont goal = do
  trace $ "prdfs: " ++ take 200 (show goal)
  match prChoice prNarrowed prFree prFail prGuard prVal goal
  where
  prFail         = return ()
  prVal v        = searchNF (printValsDFS backTrack) cont v
  prChoice i x y = lookupDecision i >>= follow
    where
    follow ChooseLeft  = printValsDFS backTrack cont x
    follow ChooseRight = printValsDFS backTrack cont y
    follow NoDecision  = if backTrack then do decide True ChooseLeft  x
                                              decide True ChooseRight y
                                              setDecision i NoDecision
                                      else do decide True  ChooseLeft x
                                              decide False ChooseRight y
      -- Assumption 1: Binary choices can only be set to one of
      -- [NoDecision, ChooseLeft, ChooseRight], therefore the reset action may
      -- be ignored in between
      where decide bt c a = setDecision i c >> printValsDFS bt cont a
    follow c           = error $ "Search.prChoice: " ++ show c

  prFree i xs   = lookupDecisionID i >>= follow
    where
    follow (LazyBind cs, _) = processLB backTrack cs i xs
    follow (ChooseN c _, _) = printValsDFS backTrack cont (xs !! c)
    follow (NoDecision , j) = cont $ choicesCons j xs
    follow c                = error $ "Search.prFree: " ++ show c

  prNarrowed i@(NarrowedID pns _) xs = lookupDecision i >>= follow
    where
    follow (LazyBind cs) = processLB backTrack cs i xs
    follow (ChooseN c _) = printValsDFS backTrack cont (xs !! c)
    follow NoDecision
      | backTrack        = do
        foldr1 (>>) $ zipWith3 (decide True) [0 ..] xs pns
        setDecision i NoDecision
      | otherwise        = foldr1 (>>) $
        zipWithButLast3 (decide True) (decide False) [0 ..] xs pns
      where decide bt n a pn = setDecision i (ChooseN n pn) >> printValsDFS bt cont a
    follow c           = error $ "Search.prNarrowed: Bad choice " ++ show c
  prNarrowed i _ = error $ "Search.prNarrowed: Bad narrowed ID " ++ show i

  prGuard cs e = solves (getConstrList cs) e >>= \mbSltn -> case mbSltn of
    Nothing                      -> return ()
    Just (reset, e') | backTrack -> printValsDFS True  cont e' >> reset
                     | otherwise -> printValsDFS False cont e'

  processLB True cs i xs = do
    reset <- setUnsetDecision i NoDecision
    printValsDFS backTrack cont
      (guardCons (StructConstr cs) $ choicesCons i xs)
    reset
  processLB False cs i xs = do
    setDecision i NoDecision
    printValsDFS backTrack cont
      (guardCons (StructConstr cs) $ choicesCons i xs)

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
searchDFS act goal = do
  trace $ "Corresponding search tree:\n" ++ showChoiceTree goal
  dfs act goal
  where
  dfs cont x = do
    trace $ "dfs: " ++ take 200 (show x)
    match dfsChoice dfsNarrowed dfsFree dfsFail dfsGuard dfsVal x
    where
    dfsFail           = mnil
    dfsVal v          = searchNF searchDFS cont v

    dfsChoice i x1 x2 = lookupDecision i >>= follow
      where
      follow ChooseLeft  = dfs cont x1
      follow ChooseRight = dfs cont x2
      follow NoDecision  = decide i ChooseLeft x1 +++ decide i ChooseRight x2
      follow c           = error $ "Search.dfsChoice: Bad choice " ++ show c

    dfsFree i xs = lookupDecisionID i >>= follow
      where
      follow (LazyBind cs, _) = processLB i cs xs
      follow (ChooseN c _, _) = dfs cont (xs !! c)
      follow (NoDecision , j) = cont $ choicesCons j xs
      follow c                = error $ "Search.dfsFree: Bad choice " ++ show c

    dfsNarrowed i@(NarrowedID pns _) xs = lookupDecision i >>= follow
      where
      follow (LazyBind cs) = processLB i cs xs
      follow (ChooseN c _) = dfs cont (xs !! c)
      follow NoDecision    = foldr1 (+++) $
        zipWith3 (\m pm y -> decide i (ChooseN m pm) y) [0 ..] pns xs
      follow c             = error $ "Search.dfsNarrowed: Bad choice " ++ show c
    dfsNarrowed i _ = error $ "Search.dfsNarrowed: Bad narrowed ID " ++ show i

    dfsGuard cs e = solves (getConstrList cs) e >>= \mbSltn -> case mbSltn of
      Nothing          -> mnil
      Just (reset, e') -> dfs cont e' |< reset

    processLB i cs xs = do
      reset <- setUnsetDecision i NoDecision
      dfs cont (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

    decide i c y = do
      reset <- setUnsetDecision i c
      dfs cont y |< reset

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
searchBFS act goal = do
  trace $ "Corresponding search tree:\n" ++ showChoiceTree goal
  bfs act [] [] (return ()) (return ()) goal
  where
  -- xs is the list of values to be processed in this level
  -- ys is the list of values to be processed in the next level
  bfs cont xs ys set reset x = do
    trace $ "bfs: " ++ take 200 (show x)
    match bfsChoice bfsNarrowed bfsFree bfsFail bfsGuard bfsVal x
    where
    bfsFail         = reset >> next cont xs ys
    bfsVal v        = (searchNF searchBFS cont v) +++ (reset >> next cont xs ys) -- TODO: Check this!
    bfsChoice i a b = set >> lookupDecision i >>= follow
      where
      follow ChooseLeft  = bfs cont xs ys set reset a
      follow ChooseRight = bfs cont xs ys set reset b
      follow NoDecision  = do
        reset
        next cont xs (decide i ChooseLeft a : decide i ChooseRight b : ys)
      follow c           = error $ "Search.bfsChoice: Bad choice " ++ show c

    bfsNarrowed i@(NarrowedID pns _) zs = set >> lookupDecision i >>= follow
      where
      follow (LazyBind cs) = processLB i cs zs
      follow (ChooseN c _) = bfs cont xs ys set reset (zs !! c)
      follow NoDecision    = reset >> next cont xs
        (zipWith3 (\n pn y -> decide i (ChooseN n pn) y) [0..] pns zs ++ ys)
      follow c             = error $ "Search.bfsNarrowed: Bad choice " ++ show c
    bfsNarrowed i _ = error $ "Search.bfsNarrowed: Bad narrowed ID " ++ show i

    bfsFree i zs = set >> lookupDecisionID i >>= follow
      where
      follow (LazyBind cs, _) = processLB i cs zs
      follow (ChooseN c _, _) = bfs cont xs ys set reset (zs !! c)
      follow (NoDecision , j) = reset >> (cont (choicesCons j zs) +++ (next cont xs ys))
      follow c                = error $ "Search.bfsFree: Bad choice " ++ show c

    bfsGuard cs e = set >> solves (getConstrList cs) e >>= \mbSltn -> case mbSltn of
      Nothing            -> reset >> next cont xs ys
      Just (newReset, a) -> bfs cont xs ys set (newReset >> reset) a

    next _     []                   [] = mnil
    next cont' []                   bs = next cont' (reverse bs) []
    next cont' ((setA,resetA,a):as) bs = bfs cont' as bs setA resetA a
--     next cont' []  ((setB,resetB,b):bs) = bfs cont' bs [] setB resetB b

    processLB i cs zs = do
      newReset <- setUnsetDecision i NoDecision
      bfs cont xs ys set (reset >> newReset)
        (guardCons (StructConstr cs) (choicesCons i zs))

    decide i c y = ( setDecision i c          >> set
                   , setDecision i NoDecision >> reset
                   , y)

-- tracing
traceLookup :: Show a => (ID -> IO a) -> ID -> IO a
traceLookup lookUp i = do
  d <- lookUp i
  trace $ "lookup " ++ show i ++ " -> " ++ show d
  return d

traceDecision :: (ID -> Decision -> IO a) -> ID -> Decision -> IO a
traceDecision set i c = do
  reset <- set i c
  trace $ "set " ++ show i ++ " -> " ++ show c
  return reset

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
  iter oldDepth newDepth expr = startIDS oldDepth newDepth msingleton expr
       ++++ iter newDepth (incrDepth4IDFS newDepth) expr
--   initSupply >>= \s -> iter s 0 initdepth
--   where
--   iter s olddepth newdepth = startIDS ((const $!!) (goal s emptyCs) emptyCs) olddepth newdepth
--                               ++++ iter s newdepth (incrDepth4IDFS newdepth)

-- start iterative deepening for a given depth interval
startIDS :: NormalForm a => Int -> Int -> (a -> IO (IOList b)) -> a -> IO (IOList b)
startIDS olddepth newdepth act goal = do
  trace $ "Corresponding search tree:\n" ++ showChoiceTree goal
  ids newdepth act goal
  where
  ids n cont x = do
    trace $ "ids: " ++ take 200 (show x)
    match idsChoice idsNarrowed idsFree idsFail idsGuard idsVal x
    where
    idsFail = mnil
    idsVal v | n <= newdepth - olddepth = searchNF (startIDS olddepth n) cont v
             | otherwise                = mnil

    idsChoice i x1 x2 = lookupDecision i >>= follow
      where
      follow ChooseLeft  = ids n cont x1
      follow ChooseRight = ids n cont x2
      follow NoDecision  = checkDepth
                         $ decide i ChooseLeft x1 +++ decide i ChooseRight x2
      follow c           = error $ "Search.idsChoice: Bad choice " ++ show c

    idsFree i xs = lookupDecisionID i >>= follow
      where
      follow (LazyBind cs, _) = processLB i cs xs
      follow (ChooseN c _, _) = ids n cont (xs !! c)
      follow (NoDecision , j) = cont $ choicesCons j xs
      follow c                = error $ "Search.idsFree: Bad choice " ++ show c

    idsNarrowed i@(NarrowedID pns _) xs = lookupDecision i >>= follow
      where
      follow (LazyBind cs) = processLB i cs xs
      follow (ChooseN c _) = ids n cont (xs !! c)
      follow NoDecision    = checkDepth $ foldr1 (+++) $
        zipWith3 (\m pm y -> decide i (ChooseN m pm) y) [0 ..] pns xs
      follow c             = error $ "Search.idsNarrowed: Bad choice " ++ show c
    idsNarrowed i _ = error $ "Search.idsNarrowed: Bad narrowed ID " ++ show i

    idsGuard cs e = solves (getConstrList cs) e >>= \mbSltn -> case mbSltn of
      Nothing          -> mnil
      Just (reset, e') -> ids n cont e' |< reset

    checkDepth deeper = if (n > 0) then deeper else abort

    processLB i cs xs = do
      reset <- setUnsetDecision i NoDecision
      ids n cont (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

    decide i c y = do
      reset <- setUnsetDecision i c
      ids (n - 1) cont y |< reset

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
  fromList . parSearch . flip evalStateT (Map.empty :: SetOfDecisions)
                       . searchMPlus' return

-- ---------------------------------------------------------------------------
-- Generic search using MonadPlus as the result monad
-- ---------------------------------------------------------------------------

type SetOfDecisions = Map.Map Integer Decision

instance Monad m => Store (StateT SetOfDecisions m) where
  getDecisionRaw u        = gets
                          $ Map.findWithDefault defaultDecision (mkInteger u)
  setDecisionRaw u c
    | isDefaultDecision c = modify $ Map.delete (mkInteger u)
    | otherwise           = modify $ Map.insert (mkInteger u) c
  unsetDecisionRaw u      = modify $ Map.delete (mkInteger u)

-- Collect results of a non-deterministic computation in a monadic structure.
searchMPlus :: (MonadPlus m, NormalForm a) => a -> ConstStore -> m a
searchMPlus x store = evalStateT
                      (searchMPlus' return ((const $!!) x store))
                      (Map.empty :: SetOfDecisions)

searchMPlus' :: (NormalForm a, MonadPlus m, Store m) => (a -> m b) -> a -> m b
searchMPlus' cont = match smpChoice smpNarrowed smpFree smpFail smpGuard smpVal
  where
  smpFail         = mzero
  smpVal v        = searchNF searchMPlus' cont v

  smpChoice i x y = lookupDecision i >>= follow
    where
    follow ChooseLeft  = searchMPlus' cont x
    follow ChooseRight = searchMPlus' cont y
    follow NoDecision  = decide i ChooseLeft x `mplus` decide i ChooseRight y
    follow c           = error $ "Search.smpChoice: Bad choice " ++ show c

  smpNarrowed i@(NarrowedID pns _) xs = lookupDecision i >>= follow
    where
    follow (LazyBind cs) = processLB i cs xs
    follow (ChooseN c _) = searchMPlus' cont (xs !! c)
    follow NoDecision    = msum $
      zipWith3 (\m pm y -> decide i (ChooseN m pm) y) [0..] pns xs
    follow c             = error $ "Search.smpNarrowed: Bad choice " ++ show c
  smpNarrowed i _ = error $ "Search.smpNarrowed: Bad narrowed ID " ++ show i

  smpFree i xs = lookupDecisionID i >>= follow
    where
    follow (LazyBind cs, _) = processLB i cs xs
    follow (ChooseN c _, _) = searchMPlus' cont (xs !! c)
    follow (NoDecision , j) = cont $ choicesCons j xs
    follow c                = error $ "Search.smpFree: Bad choice " ++ show c

  smpGuard cs e = solves (getConstrList cs) e >>= \mbSltn -> case mbSltn of
    Nothing      -> mzero
    Just (_, e') -> searchMPlus' cont e'

  decide i c y = do
    setDecision i c
    searchMPlus' cont y

  processLB i cs xs = do
    setDecision i NoDecision
    searchMPlus' cont (guardCons (StructConstr cs) (choicesCons i xs))
