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
prtChoiceTree goal = getNormalForm goal >>= putStrLn . showTree
  where
  showTree = ($[]) . showsTree 0 [] . try
  -- showsTree n l <tryVal>, where
  --   * n is the number of indentations (one constructor)
  --   * l is a stack of flags whether we show the last alternative
  --     of the respective level (just for drawing nice corners)
  showsTree n l (Val v)         = indent n l . shows v . nl
  showsTree n l Fail            = indent n l . showChar '!' . nl
  showsTree n l (Choice  i x y) = indent n l . shows i . nl
                                . showsChildren (n+1) l [try x, try y]
  showsTree n l (Narrowed i xs) = indent n l . shows i . nl
                                . showsChildren (n+1) l (map try xs)
  showsTree n l (Free     i xs) = indent n l . shows i . nl
                                . showsChildren (n+1) l (map try xs)
  showsTree n l (Guard    cs e) = indent n l . shows cs . nl
                                . showsChildren (n+1) l [try e]

  indent 0 _       = id
  indent n (hd:tl) = showString (concatMap showLines $ reverse tl)
                   . showChar (if hd then llc else lmc)
                   . showString (hbar:hbar:" ")
                     where showLines b = (if b then ' ' else vbar) : "   "

  vbar = '\x2502'     -- vertical bar
  hbar = '\x2500'     -- horizontal bar
  llc  = '\x2514'     -- left lower corner
  lmc  = '\x251c'     -- left middle corner
  nl   = showChar '\n'-- newline :)

  showsChildren n l []     = id
  showsChildren n l [x]    = showsTree n (True :l) x
  showsChildren n l (x:xs) = showsTree n (False:l) x . showsChildren n l xs


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
printValsDFS backTrack cont x = do
  trace $ "prdfs: " ++ take 200 (show x)
  match prChoice prNarrowed prFree prFail prGuard prVal x
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
    follow c           = error $ "Basics.printValsDFS.follow: " ++ show c

  prFree i xs   = lookupDecisionID i >>= follow
    where
    follow (LazyBind cs, _) = processLazyBind backTrack cs i xs
                                              (printValsDFS backTrack cont)
    follow (ChooseN c _, _) = printValsDFS backTrack cont (xs !! c)
    follow (NoDecision , j) = cont $ choicesCons j xs
    follow c                = error $ "Basics.printValsDFS.follow: " ++ show c

  prNarrowed i@(NarrowedID pns _) xs = lookupDecision i >>= follow
    where
    follow (LazyBind cs) = processLazyBind backTrack cs i xs (printValsDFS backTrack cont)
    follow (ChooseN c _) = printValsDFS backTrack cont (xs !! c)
    follow NoDecision    =
      if backTrack then do
       foldr1 (>>) $ zipWith3 (decide True) [0 ..] xs pns
       setDecision i NoDecision
      else do
       foldr1 (>>) $ zipWithButLast3 (decide True) (decide False) [0 ..] xs pns
      where decide bt n a pn = setDecision i (ChooseN n pn) >> printValsDFS bt cont a
    follow c           = error $ "Basics.printValsDFS.follow: " ++ show c
  prNarrowed i _ = error $ "prDFS: Bad narrowed ID " ++ show i

  prGuard cs e = solves (getConstrList cs) e >>= \mbSltn -> case mbSltn of
    Nothing                       -> return ()
    Just (reset, e') | backTrack -> printValsDFS True  cont e' >> reset
                     | otherwise -> printValsDFS False cont e'

processLazyBind :: NonDet a => Bool -> [Constraint] -> ID -> [a] -> (a -> IO ()) -> IO ()
processLazyBind True cs i xs search = do
  reset <- setUnsetDecision i NoDecision
  search $ guardCons (StructConstr cs) $ choicesCons i xs
  reset
processLazyBind False cs i xs search = do
  setDecision i NoDecision
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
searchDFS cont x = do
  trace $ "dfs: " ++ take 200 (show x)
  match dfsChoice dfsNarrowed dfsFree dfsFail dfsGuard dfsVal x
  where
  dfsFail           = mnil
  dfsVal v          = searchNF searchDFS cont v
  dfsChoice i x1 x2 = lookupDecision i >>= choose
    where
    choose ChooseLeft  = searchDFS cont x1
    choose ChooseRight = searchDFS cont x2
    choose NoDecision    = newDecision ChooseLeft x1 +++ newDecision ChooseRight x2
    choose c           = error $ "Basics.searchDFS.choose: " ++ show c

    newDecision c x = do
      reset <- setUnsetDecision i c
      searchDFS cont x |< reset

  dfsFree i xs = lookupDecisionID i >>= choose
    where
    choose (LazyBind cs, _) = processLB cs
    choose (ChooseN c _, _) = searchDFS cont (xs !! c)
    choose (NoDecision   , j) = cont $ choicesCons j xs
    choose c                = error $ "Basics.searchDFS.choose: " ++ show c

    processLB cs = do
      reset <- setUnsetDecision i NoDecision
      searchDFS cont (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

  dfsNarrowed i@(NarrowedID pns _) xs = lookupDecision i >>= choose
    where
    choose (LazyBind cs) = processLB cs
    choose (ChooseN c _) = searchDFS cont (xs !! c)
    choose NoDecision      = foldr1 (+++) $ zipWith3 newDecision [0 ..] xs pns
    choose c             = error $ "Basics.searchDFS'.choose: " ++ show c

    processLB cs = do
      reset <- setUnsetDecision i NoDecision
      searchDFS cont (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

    newDecision n x pn = do
      reset <- setUnsetDecision i (ChooseN n pn)
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
  bfs cont xs ys set reset x = do
    trace $ "bfs: " ++ take 200 (show x)
    match bfsChoice bfsNarrowed bfsFree bfsFail bfsGuard bfsVal x
    where
    bfsFail         = reset >> next cont xs ys
    bfsVal v        = (set >> searchNF searchBFS cont v)
                      +++ (reset >> next cont xs ys)
    bfsChoice i a b = set >> lookupDecision i >>= follow
      where
      follow ChooseLeft  = bfs cont xs ys (return ()) reset a
      follow ChooseRight = bfs cont xs ys (return ()) reset b
      follow NoDecision  = do
        reset
        next cont xs (decide ChooseLeft a : decide ChooseRight b : ys)
      follow c           = error $ "Basics.searchDFS: Bad choice " ++ show c

      decide c x = (set >> setDecision i c, reset >> setDecision i NoDecision, x)

    bfsNarrowed i@(NarrowedID pns _) cs = set >> lookupDecision i >>= follow
      where
      follow (LazyBind cns) = do
        newReset <- setUnsetDecision i NoDecision
        bfs cont xs ys (return ()) (reset >> newReset)
            (guardCons (StructConstr cns) (choicesCons i cs))
      follow (ChooseN c _)  = bfs cont xs ys (return ()) reset (cs !! c)
      follow NoDecision     = do
        reset
        next cont xs (zipWith3 decide [0..] cs pns ++ ys)
      follow c             = error $ "Basics.searchDFS: Bad choice " ++ show c

      decide n y pn = ( set   >> setDecision i (ChooseN n pn)
                      , reset >> setDecision i NoDecision
                      , y)

    bfsNarrowed i _ = error $ "searchBFS: Bad narrowed ID " ++ show i

    bfsFree i cs = set >> lookupDecision i >>= follow
      where
      follow (LazyBind cns) = processLB cns
      follow (ChooseN c _)  = bfs cont xs ys (return ()) reset (cs !! c)
      follow NoDecision     = reset >> (cont (choicesCons i cs) +++ (next cont xs ys))
      follow c              = error $ "Basics.searchBFS.choose: " ++ show c

      processLB cns = do
        newReset <- setUnsetDecision i NoDecision
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

    idsChoice i x1 x2 = lookupDecision i >>= choose
      where
      choose ChooseLeft  = idsHNF n x1
      choose ChooseRight = idsHNF n x2
      choose NoDecision
        | n > 0          = newDecision ChooseLeft x1 +++ newDecision ChooseRight x2
        | otherwise      = abort
        where
        newDecision c y = do
          reset <- setUnsetDecision i c
          idsHNF (n - 1) y |< reset
      choose c           = error $ "idsHNF: Bad choice " ++ show c

    idsFree i xs = lookupDecisionID i >>= choose
      where
      choose (LazyBind cs, _) = processLB cs
      choose (ChooseN c _, _) = idsHNF n (xs !! c)
      choose (NoDecision   , j) = msingleton $ choicesCons j xs
      choose c                = error $ "Basics.searchDFS.choose: " ++ show c

      processLB cs = do
        reset <- setUnsetDecision i NoDecision
        idsHNF n (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

    idsNarrowed i@(NarrowedID pns _) xs = lookupDecision i >>= choose
      where
      choose (LazyBind cs) = processLB cs
      choose (ChooseN c _) = idsHNF n (xs !! c)
      choose NoDecision      = foldr1 (+++) $ zipWith3 newDecision [0 ..] xs pns
      choose c             = error $ "idsHNF.choose: " ++ show c

      processLB cs = do
        reset <- setUnsetDecision i NoDecision
        idsHNF n (guardCons (StructConstr cs) $ choicesCons i xs) |< reset

      newDecision m y pn = do
        reset <- setUnsetDecision i (ChooseN m pn)
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
  list2iolist . parSearch . flip evalStateT (Map.empty :: SetOfDecisions)
                          . searchMPlus' return

-- ---------------------------------------------------------------------------
-- Generic search using MonadPlus as the result monad
-- ---------------------------------------------------------------------------

type SetOfDecisions = Map.Map Integer Decision

instance Monad m => Store (StateT SetOfDecisions m) where
  getDecisionRaw u        = gets $ Map.findWithDefault defaultDecision (mkInteger u)
  setDecisionRaw u c
    | isDefaultDecision c = modify $ Map.delete (mkInteger u)
    | otherwise         = modify $ Map.insert (mkInteger u) c
  unsetDecisionRaw u      = modify $ Map.delete (mkInteger u)

-- Collect results of a non-deterministic computation in a monadic structure.
searchMPlus :: (MonadPlus m, NormalForm a) => a -> ConstStore -> m a
searchMPlus x store = evalStateT
                      (searchMPlus' return ((const $!!) x store))
                      (Map.empty :: SetOfDecisions)

searchMPlus' :: (NormalForm a, MonadPlus m, Store m) => (a -> m b) -> a -> m b
searchMPlus' cont = match smpChoice smpNarrowed smpFree mzero smpGuard smpVal
  where
  smpVal v = searchNF searchMPlus' cont v
  smpChoice i x y = lookupDecision i >>= choose
    where
    choose ChooseLeft  = searchMPlus' cont x
    choose ChooseRight = searchMPlus' cont y
    choose NoDecision    = (setDecision i ChooseLeft  >> searchMPlus' cont x)
                         `mplus`
                         (setDecision i ChooseRight >> searchMPlus' cont y)
    choose c           = error $ "searchMPlus: Bad choice " ++ show c

  smpNarrowed i@(NarrowedID pns _)  bs = lookupDecision i >>= choose
    where
    choose (ChooseN c _) = searchMPlus' cont (bs !! c)
    choose NoDecision      =
      msum $ zipWith3 (\n c pn -> pick n pn >> searchMPlus' cont c) [0..] bs pns
    choose (LazyBind cs) = processLazyBind' i cont cs bs
    choose c             = error $ "searchMPlus: Bad choice " ++ show c

    pick c pn = setDecision i (ChooseN c pn)
  smpNarrowed i _ = error $ "searchMPlus: Bad narrowed ID " ++ show i

  smpFree i branches = lookupDecision i >>= choose
    where
    choose (ChooseN c _) = searchMPlus' cont (branches !! c)
    choose NoDecision      = cont $ choicesCons i branches
    choose (LazyBind cs) = processLazyBind' i cont cs branches
    choose c             = error $ "searchMPlus: Bad choice " ++ show c

  smpGuard cs e = solves (getConstrList cs) e >>= \mbSolution -> case mbSolution of
    Nothing      -> mzero
    Just (_, e') -> searchMPlus' cont e'

processLazyBind' :: (NormalForm a, MonadPlus m, Store m)
                 => ID -> (a -> m b) -> [Constraint] -> [a] -> m b
processLazyBind' i cont cs xs = do
  setDecision i NoDecision
  searchMPlus' cont (guardCons (StructConstr cs) (choicesCons i xs))
