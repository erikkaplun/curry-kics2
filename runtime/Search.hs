{-# LANGUAGE CPP, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Search where

import Control.Monad
import Control.Monad.State.Strict
import Data.List (intercalate)
import qualified Data.Map as Map

import Debug
import CurryException
import PrimTypes -- for C_IO
import MonadList
import Solver
import Strategies
import Types
import MonadSearch
import FailInfo    (FailInfo (failCause), defFailInfo, customFail, nondetIO)
import FailTrace   (inspectTrace)

type DetExpr    a =             Cover -> ConstStore -> a
type NonDetExpr a = IDSupply -> Cover -> ConstStore -> a
type Strategy   a = NonDetExpr a -> IO (IOList a)


debugSearch :: NormalForm a => (a -> IO ()) -> NonDetExpr a -> IO ()
debugSearch _ goal = do
  s <- initSupply
  let expr = goal s initCover emptyCs
  putStrLn $ header "Expression"
  print expr
  let nf = ((\x _ _ -> x) $!! expr) initCover emptyCs
  putStrLn $ header "Normalform"
  print nf
  putStrLn $ header "Search Tree"
  putStrLn $ showChoiceTree' nf
  let res = runStateT (searchMSearch' initCover return nf) emptyDecisionMap
  putStrLn $ header "Results"
  mapM_ showRes (dfsSearch res)

  where
    header s = s ++ '\n' : replicate (length s) '='
    showRes (r, dm) = do
      putStr "Result   : " >> print r
      putStr "#Bindings: " >> print (Map.size (decisionMap dm))
      putStr "Bindings : " >> print (decisionMap dm)

-- ---------------------------------------------------------------------------
-- Search combinators for top-level search in the IO monad
-- ---------------------------------------------------------------------------

countAll :: NormalForm a => Strategy a -> NonDetExpr a -> IO ()
countAll search goal = search goal >>= countValues

printAll :: NormalForm a => Strategy a -> NonDetExpr a -> IO ()
printAll search goal = search goal >>= printAllValues print

printOne :: NormalForm a => Strategy a -> NonDetExpr a -> IO ()
printOne search goal = search goal >>= printOneValue print

printInteractive :: NormalForm a => Strategy a -> NonDetExpr a -> IO ()
printInteractive search goal = search goal >>= printOneValue print

ioDFS :: NormalForm a => Strategy a
ioDFS goal = getNormalForm goal >>= searchDFS msingleton

ioBFS :: NormalForm a => Strategy a
ioBFS goal = getNormalForm goal >>= searchBFS msingleton

ioIDS :: NormalForm a => Int -> (Int -> Int) -> Strategy a
ioIDS initDepth incr goal = getNormalForm goal >>=
  searchIDS initDepth incr msingleton

ioIDS2 :: NormalForm a => Int -> (Int -> Int) -> Strategy a
ioIDS2 initDepth incr goal = searchIDS2 initDepth incr msingleton goal

mplusDFS :: NormalForm a => Strategy a
mplusDFS goal = getNormalForm goal >>=
  fromList . dfsSearch . searchMSearch initCover

mplusBFS :: NormalForm a => Strategy a
mplusBFS goal = getNormalForm goal >>=
  fromList . bfsSearch . searchMSearch initCover

mplusIDS :: NormalForm a => Int -> (Int -> Int) -> Strategy a
mplusIDS initDepth incr goal = getNormalForm goal >>=
  fromList . idsSearch initDepth incr . searchMSearch initCover

mplusPar :: NormalForm a => Strategy a
mplusPar goal = getNormalForm goal >>=
  fromList . parSearch . searchMSearch initCover

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

searchIO :: IDSupply -> Cover -> ConstStore -> C_IO a -> IO (Either FailInfo a)
searchIO s cd cs act = case act of
  C_IO              io -> io
  HO_C_IO        iofun -> iofun s cd cs
  Fail_C_IO     _ info -> failWith info
  Choice_C_IO  _ i _ _ -> failWith $ nondetIO (show i)
  Choices_C_IO  _ i xs -> case i of
    ChoiceID     _ -> internalError "searchIO: choices with ChoiceID"
    NarrowedID _ _ -> followToIO i xs
    FreeID     _ _ -> lookupCs cs i (searchIO s cd cs) $
      lookupGlobalCs >>= \gs -> lookupCs gs i (searchIO s cd cs) (followToIO i xs)
  -- bindings of free variables are looked up first in the local
  -- constraint store and then in the global constraint store
  Guard_C_IO    _ cs' e -> solve initCover cs' e >>= \mbSol -> case mbSol of
    Nothing       -> failWith $ customFail "Unsatisfiable constraint"
    Just (_, val) -> do
      -- add the constraint to the global constraint store to make it
      -- available to subsequent operations.
      -- This is valid because no backtracking is done in IO.
      addToGlobalCs cs'
      searchIO s cd (cs' `addCs` cs) val
  where
  failWith = return . Left
  followToIO i xs = lookupDecision i >>= \c -> case c of
    ChooseN idx _ -> searchIO s cd cs (xs !! idx)
    NoDecision    -> failWith $ nondetIO (show i)
    LazyBind cs'  -> searchIO s cd cs
                   $ guardCons initCover (StructConstr cs')
                   $ choicesCons initCover i xs
    _             -> internalError $ "followToIO: " ++ show c

toIO :: IDSupply -> Cover -> ConstStore -> C_IO a -> IO a
toIO s cd cs act = searchIO s cd cs act >>= \res -> case res of
  Left info -> throwFail $ "IO action failed: " ++ failCause info
  Right val -> return val

-- ---------------------------------------------------------------------------
-- Simple evaluation without search or normal form computation
-- ---------------------------------------------------------------------------

getNormalForm :: NormalForm a => NonDetExpr a -> IO a
getNormalForm goal = do
  s <- initSupply
  return $ ((\x _ _ -> x) $!! goal s initCover emptyCs) initCover emptyCs

-- |Evaluate a deterministic expression without search
evalD :: Show a => DetExpr a -> IO ()
evalD goal = print (goal initCover emptyCs)

-- |Evaluate a non-deterministic expression without search
eval :: Show a => NonDetExpr a -> IO ()
eval goal = initSupply >>= \s -> print (goal s initCover emptyCs)

-- |Evaluate a deterministic IO action without search
evalDIO :: NormalForm a => DetExpr (C_IO a) -> IO ()
evalDIO goal = do
  _ <- toIO errSupply initCover emptyCs (goal initCover emptyCs)
  return ()
  where errSupply = internalError "Search.evalDIO: ID supply used"

-- |Evaluate a non-deterministic IO action with simple search
evalIO :: NormalForm a => NonDetExpr (C_IO a) -> IO ()
evalIO goal = do
  s <- initSupply
  _ <- toIO (leftSupply s) initCover emptyCs
       (goal (rightSupply s) initCover emptyCs)
  return ()

-- |Evaluate a deterministic expression without search, but trace failures
failtraceD :: NormalForm a => DetExpr a -> IO ()
failtraceD goal = case try (goal initCover emptyCs) of
  Fail _ info -> inspectTrace info
  Val v       -> print v
  x           -> internalError $ "Search.failtraceD: non-determinism: "
                 ++ show x

-- |Evaluate a deterministic expression without search, but trace failures
failtraceDIO :: NormalForm a => DetExpr (C_IO a) -> IO ()
failtraceDIO goal = do
  res <- searchIO errSupply initCover emptyCs (goal initCover emptyCs)
  case res of
    Left err -> inspectTrace err
    Right _  -> return ()
  where errSupply = internalError "Search.failtraceDIO: ID supply used"

-- ---------------------------------------------------------------------------
-- Evaluate a nondeterministic expression (thus, requiring some IDSupply)
-- and print the choice structure of all results.

prtChoices :: NormalForm a => NonDetExpr a -> IO ()
prtChoices goal = getNormalForm goal >>= print

-- Evaluate a nondeterministic expression (thus, requiring some IDSupply)
-- and print the choice structure of all results as a tree.
prtChoiceTree :: NormalForm a => Int -> NonDetExpr a -> IO ()
prtChoiceTree d goal = getNormalForm goal >>= putStrLn . showChoiceTree d

showChoiceTree' :: (Show a, NonDet a) => a -> String
showChoiceTree' = showChoiceTree 10

showChoiceTree :: (Show a, NonDet a) => Int -> a -> String
showChoiceTree n goal = showsTree n [] "" (try goal) []
  where
  -- showsTree d ctxt k <tryVal>, where
  --   * @d@    is the remaining depth to print the tree
  --   * @ctxt@ is a stack of flags whether we show the last alternative
  --            of the respective level (for drawing aesthetical corners)
  --   * @k@    is the key for the decision (L, R, constructor index)
  showsTree d l k ndVal
    | d <= 0    = indent l k . showChar elli . nl
    | otherwise = indent l k . case ndVal of
      Val v           -> showString "Val " . shows v . nl
      Fail _ _        -> showChar '!' . nl
      Choice  _ i x y -> shows i  . nl . showsChildren d l
                         [("L", try x), ("R", try y)]
      Narrowed _ i xs -> shows i  . nl . showsChildren d l
                         (zip (map show [(0 :: Int) ..]) (map try xs))
      Free     _ i xs -> shows i  . nl . showsChildren d l
                         (zip (map show [(0 :: Int) ..]) (map try xs))
      Guard    _ cs e -> shows cs . nl . showsChildren d l [("", try e)]

  indent []      _ = id
  indent (hd:tl) k = showString (concatMap showLines $ reverse tl)
                   . showChar (if hd then llc else lmc)
                   . showString (hbar:hbar:" ")
                   . showKey k
    where showLines inLast = (if inLast then ' ' else vbar) : "       "

  showKey "" = id
  showKey k  = showString k . showString ": "

  elli = '\x2026'     -- ellipsis …
  vbar = '\x2502'     -- vertical bar │
  hbar = '\x2500'     -- horizontal bar ─
  llc  = '\x2514'     -- left lower corner └
  lmc  = '\x251c'     -- left middle corner ├
  nl   = showChar '\n'-- newline :)

  showsChildren _ _ []          = id
  showsChildren d l [(k,v)]     = showsTree (d-1) (True :l) k v
  showsChildren d l ((k,v):kvs) = showsTree (d-1) (False:l) k v
                                . showsChildren d l kvs

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
prdfs prt goal = getNormalForm goal >>=
#ifdef Try
  printValsDFSTry False prt
#else
   printValsDFSMatch False prt
#endif

-- The first argument backTrack indicates whether backtracking is needed
printValsDFSMatch :: NormalForm a => Bool -> (a -> IO ()) -> a -> IO ()
printValsDFSMatch backTrack cont goal = do
  trace $ "prdfs: " ++ take 200 (show goal)
  match prChoice prNarrowed prFree prFail prGuard prVal goal
  where
  prFail _ _       = return ()
  prVal v          = searchNF (printValsDFSMatch backTrack) cont v
  prChoice _ i x y = lookupDecision i >>= follow
    where
    follow ChooseLeft  = printValsDFSMatch backTrack cont x
    follow ChooseRight = printValsDFSMatch backTrack cont y
    follow NoDecision  = if backTrack then do decide True ChooseLeft  x
                                              decide True ChooseRight y
                                              setDecision i NoDecision
                                      else do decide True  ChooseLeft x
                                              decide False ChooseRight y
      -- Assumption 1: Binary choices can only be set to one of
      -- [NoDecision, ChooseLeft, ChooseRight], therefore the reset action may
      -- be ignored in between
      where decide bt c a = setDecision i c >> printValsDFSMatch bt cont a
    follow c           = internalError $ "Search.prChoice: " ++ show c

  prFree cd i xs   = lookupDecisionID i >>= follow
    where
    follow (LazyBind cs, _) = processLB backTrack cs i xs
    follow (ChooseN c _, j) = printValsDFSMatch backTrack cont (ys !! c)
      where Free _ _ ys = try $ generate (supply j) cd
    follow (NoDecision , j) = cont $ choicesCons initCover j xs
    follow c                = internalError $ "Search.prFree: " ++ show c

  prNarrowed _ i@(NarrowedID pns _) xs = lookupDecision i >>= follow
    where
    follow (LazyBind cs) = processLB backTrack cs i xs
    follow (ChooseN c _) = printValsDFSMatch backTrack cont (xs !! c)
    follow NoDecision
      | backTrack        = do
        foldr1 (>>) $ zipWith3 (decide True) [0 ..] xs pns
        setDecision i NoDecision
      | otherwise        = foldr1 (>>) $
        zipWithButLast3 (decide True) (decide False) [0 ..] xs pns
      where decide bt n a pn = setDecision i (ChooseN n pn) >> printValsDFSMatch bt cont a
    follow c           = internalError $ "Search.prNarrowed: Bad choice " ++ show c
  prNarrowed _ i _ = internalError $ "Search.prNarrowed: Bad narrowed ID " ++ show i

  prGuard _ cs e = solve initCover cs e >>= \mbSltn -> case mbSltn of
    Nothing                      -> return ()
    Just (reset, e') | backTrack -> printValsDFSMatch True  cont e' >> reset
                     | otherwise -> printValsDFSMatch False cont e'

  processLB True cs i xs = do
    reset <- setUnsetDecision i NoDecision
    printValsDFSMatch backTrack cont
      (guardCons initCover (StructConstr cs) $ choicesCons initCover i xs)
    reset
  processLB False cs i xs = do
    setDecision i NoDecision
    printValsDFSMatch backTrack cont
      (guardCons initCover (StructConstr cs) $ choicesCons initCover i xs)



printValsDFSTry :: NormalForm a => Bool -> (a -> IO ()) -> a -> IO ()
printValsDFSTry backTrack cont goal = do
  trace $ "prdfs: " ++ take 200 (show goal)
  case try goal of
    Fail _ _ ->  return ()
    Val v    ->  searchNF (printValsDFSTry backTrack) cont v
    Choice _ i x y -> lookupDecision i >>= follow
     where
       follow ChooseLeft  = printValsDFSTry backTrack cont x
       follow ChooseRight = printValsDFSTry backTrack cont y
       follow NoDecision  = if backTrack then do decide True ChooseLeft  x
                                                 decide True ChooseRight y
                                                 setDecision i NoDecision
                                         else do decide True  ChooseLeft x
                                                 decide False ChooseRight y
         -- Assumption 1: Binary choices can only be set to one of
         -- [NoDecision, ChooseLeft, ChooseRight], therefore the reset action may
         -- be ignored in between
         where decide bt c a = setDecision i c >> printValsDFSTry bt cont a
       follow c           = error $ "Search.prChoice: " ++ show c

    Free cd i xs   -> lookupDecisionID i >>= follow
      where
      follow (LazyBind cs, _) = processLB backTrack cs i xs
      follow (ChooseN c _, j) = printValsDFSTry backTrack cont (ys !! c)
        where Free _ _ ys = try $ generate (supply j) cd
      follow (NoDecision , j) = cont $ choicesCons initCover j xs
      follow c                = error $ "Search.prFree: " ++ show c

    Narrowed _ i@(NarrowedID pns _) xs -> lookupDecision i >>= follow
      where
      follow (LazyBind cs) = processLB backTrack cs i xs
      follow (ChooseN c _) = printValsDFSTry backTrack cont (xs !! c)
      follow NoDecision
        | backTrack        = do
          foldr1 (>>) $ zipWith3 (decide True) [0 ..] xs pns
          setDecision i NoDecision
        | otherwise        = foldr1 (>>) $
          zipWithButLast3 (decide True) (decide False) [0 ..] xs pns
        where decide bt n a pn = setDecision i (ChooseN n pn) >> printValsDFSTry bt cont a
      follow c           = error $ "Search.prNarrowed: Bad choice " ++ show c
    Narrowed _ i _ -> error $ "Search.prNarrowed: Bad narrowed ID " ++ show i

    Guard _ cs e -> solve initCover cs e >>= \mbSltn -> case mbSltn of
      Nothing                      -> return ()
      Just (reset, e') | backTrack -> printValsDFSTry True  cont e' >> reset
                       | otherwise -> printValsDFSTry False cont e'
 where
  processLB True cs i xs = do
    reset <- setUnsetDecision i NoDecision
    printValsDFSTry backTrack cont
      (guardCons initCover (StructConstr cs) $ choicesCons initCover i xs)
    reset
  processLB False cs i xs = do
    setDecision i NoDecision
    printValsDFSTry backTrack cont
      (guardCons initCover (StructConstr cs) $ choicesCons initCover i xs)


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
computeWithDFS :: NormalForm a => Strategy a
computeWithDFS goal = getNormalForm goal >>= searchDFS msingleton

searchDFS :: NormalForm a => (a -> IO (IOList b)) -> a -> IO (IOList b)
searchDFS act goal = do
  trace $ "Corresponding search tree:\n" ++ showChoiceTree' goal
  dfs act goal
  where
  dfs cont x = do
    trace $ "dfs: " ++ take 200 (show x)
    match dfsChoice dfsNarrowed dfsFree dfsFail dfsGuard dfsVal x
    where
    dfsFail _ _       = mnil
    dfsVal v          = searchNF searchDFS cont v

    dfsChoice _ i x1 x2 = lookupDecision i >>= follow
      where
      follow ChooseLeft  = dfs cont x1
      follow ChooseRight = dfs cont x2
      follow NoDecision  = decide i ChooseLeft x1 +++ decide i ChooseRight x2
      follow c           = internalError $ "Search.dfsChoice: Bad choice " ++ show c

    dfsFree cd i xs = lookupDecisionID i >>= follow
      where
      follow (LazyBind cs, _) = processLB i cs xs
      follow (ChooseN c _, j) = dfs cont (ys !! c)
        where Free _ _ ys = try $ generate (supply j) cd
      follow (NoDecision , j) = cont $ choicesCons cd j xs
      follow c                = internalError $ "Search.dfsFree: Bad choice " ++ show c

    dfsNarrowed _ i@(NarrowedID pns _) xs = lookupDecision i >>= follow
      where
      follow (LazyBind cs) = processLB i cs xs
      follow (ChooseN c _) = dfs cont (xs !! c)
      follow NoDecision    = foldr1 (+++) $
        zipWith3 (\m pm y -> decide i (ChooseN m pm) y) [0 ..] pns xs
      follow c             = internalError $ "Search.dfsNarrowed: Bad choice " ++ show c
    dfsNarrowed _ i _ = internalError $ "Search.dfsNarrowed: Bad narrowed ID " ++ show i

    dfsGuard _ cs e = solve initCover cs e >>= \mbSltn -> case mbSltn of
      Nothing          -> mnil
      Just (reset, e') -> dfs cont e' |< reset

    processLB i cs xs = decide i NoDecision
                      $ guardCons initCover (StructConstr cs) $ choicesCons initCover i xs

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
computeWithBFS :: NormalForm a => Strategy a
-- computeWithBFS goal = getNormalForm goal >>= searchBFS msingleton
computeWithBFS goal = getNormalForm goal >>= fromList . bfsSearch . searchMSearch initCover

searchBFS :: NormalForm a => (a -> IO (IOList b)) -> a -> IO (IOList b)
searchBFS act goal = do
  trace $ "Corresponding search tree:\n" ++ showChoiceTree' goal
  bfs act [] [] (return ()) (return ()) goal
  where
  -- xs is the list of values to be processed in this level
  -- ys is the list of values to be processed in the next level
  bfs cont xs ys set reset x = do
    trace $ "bfs: " ++ take 200 (show x)
    match bfsChoice bfsNarrowed bfsFree bfsFail bfsGuard bfsVal x
    where
    bfsFail _ _     = reset >> next cont xs ys
    bfsVal v        = (set >> searchNF searchBFS cont v)
                      +++ (reset >> next cont xs ys) -- TODO: Check this!
    bfsChoice _ i a b = set >> lookupDecision i >>= follow
      where
      follow ChooseLeft  = bfs cont xs ys set reset a
      follow ChooseRight = bfs cont xs ys set reset b
      follow NoDecision  = do
        reset
        next cont xs (decide i ChooseLeft a : decide i ChooseRight b : ys)
      follow c           = internalError $ "Search.bfsChoice: Bad choice " ++ show c

    bfsFree cd i zs = set >> lookupDecisionID i >>= follow
      where
      follow (LazyBind cs, _) = processLB i cs zs
      follow (ChooseN c _, j) = bfs cont xs ys set reset (ws !! c)
        where Free _ _ ws = try $ generate (supply j) cd
      follow (NoDecision , j) = reset >> (cont (choicesCons initCover j zs) +++ (next cont xs ys))
      follow c                = internalError $ "Search.bfsFree: Bad choice " ++ show c

    bfsNarrowed _ i@(NarrowedID pns _) zs = set >> lookupDecision i >>= follow
      where
      follow (LazyBind cs) = processLB i cs zs
      follow (ChooseN c _) = bfs cont xs ys set reset (zs !! c)
      follow NoDecision    = reset >> next cont xs
        (zipWith3 (\n pn y -> decide i (ChooseN n pn) y) [0..] pns zs ++ ys)
      follow c             = internalError $ "Search.bfsNarrowed: Bad choice " ++ show c
    bfsNarrowed _ i _ = internalError $ "Search.bfsNarrowed: Bad narrowed ID " ++ show i

    bfsGuard _ cs e = set >> solve initCover cs e >>= \mbSltn -> case mbSltn of
      Nothing            -> reset >> next cont xs ys
      Just (newReset, a) -> bfs cont xs ys set (newReset >> reset) a

    next _     []                   [] = mnil
    next cont' []                   bs = next cont' (reverse bs) []
    next cont' ((setA,resetA,a):as) bs = bfs cont' as bs setA resetA a

    processLB i cs zs = do
      newReset <- setUnsetDecision i NoDecision
      bfs cont xs ys set (reset >> newReset)
        (guardCons initCover (StructConstr cs) (choicesCons initCover i zs))

    decide i c y = ( setDecision i c          >> set
                   , setDecision i NoDecision >> reset
                   , y)

-- ---------------------------------------------------------------------------
-- Iterative depth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- A function to increase the depth for the iterative deepening strategy
-- (here: double the depth after reaching the depth bound)
incrDepth4IDFS :: Int -> Int
incrDepth4IDFS n = n * 2

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
computeWithIDS :: NormalForm a => Int -> Strategy a
computeWithIDS initDepth goal = getNormalForm goal >>= searchIDS initDepth incrDepth4IDFS msingleton

searchIDS :: NormalForm a => Int -> (Int -> Int) -> (a -> IO (IOList b)) -> a -> IO (IOList b)
searchIDS initDepth incr cont goal = iter (-1) initDepth
  where iter oldDepth newDepth = startIDS oldDepth newDepth cont goal
                            ++++ iter newDepth (incr newDepth)

searchIDS2 :: NormalForm a => Int -> (Int -> Int) -> (a -> IO (IOList b)) -> NonDetExpr a -> IO (IOList b)
searchIDS2 initDepth incr cont goal = iter (-1) initDepth
  where
  iter oldDepth newDepth = (getNormalForm goal >>= startIDS oldDepth newDepth cont)
                            ++++ iter newDepth (incr newDepth)

-- start iterative deepening for a given depth interval
startIDS :: NormalForm a => Int -> Int -> (a -> IO (IOList b)) -> a -> IO (IOList b)
startIDS olddepth newdepth act goal = do
  trace $ "Corresponding search tree:\n" ++ showChoiceTree' goal
  ids newdepth act goal
  where
  ids :: NormalForm a => Int -> (a -> IO (IOList b)) -> a -> IO (IOList b)
  ids n cont x = do
    trace $ "ids: " ++ take 200 (show x)
    match idsChoice idsNarrowed idsFree idsFail idsGuard idsVal x
    where
    idsFail _ _ = mnil
    idsVal v | n < newdepth - olddepth = searchNF (ids n) cont v
             | otherwise                = mnil

    idsChoice _ i x1 x2 = lookupDecision i >>= follow
      where
      follow ChooseLeft  = ids n cont x1
      follow ChooseRight = ids n cont x2
      follow NoDecision  = checkDepth
                         $ decide i ChooseLeft x1 +++ decide i ChooseRight x2
      follow c           = internalError $ "Search.idsChoice: Bad choice " ++ show c

    idsFree cd i xs = lookupDecisionID i >>= follow
      where
      follow (LazyBind cs, _) = processLB i cs xs
      follow (ChooseN c _, j) = ids n cont (ys !! c)
        where Free _ _ ys = try $ generate (supply j) cd
      follow (NoDecision , j) = cont $ choicesCons initCover j xs
      follow c                = internalError $ "Search.idsFree: Bad choice " ++ show c

    idsNarrowed _ i@(NarrowedID pns _) xs = lookupDecision i >>= follow
      where
      follow (LazyBind cs) = processLB i cs xs
      follow (ChooseN c _) = ids n cont (xs !! c)
      follow NoDecision    = checkDepth $ foldr1 (+++) $
        zipWith3 (\m pm y -> decide i (ChooseN m pm) y) [0 ..] pns xs
      follow c             = internalError $ "Search.idsNarrowed: Bad choice " ++ show c
    idsNarrowed _ i _ = internalError $ "Search.idsNarrowed: Bad narrowed ID " ++ show i

    idsGuard _ cs e = solve initCover cs e >>= \mbSltn -> case mbSltn of
      Nothing          -> mnil
      Just (reset, e') -> ids n cont e' |< reset

    checkDepth deeper = if (n > 0) then deeper else abort

    processLB i cs xs = do
      reset <- setUnsetDecision i NoDecision
      ids n cont (guardCons initCover (StructConstr cs) $ choicesCons initCover i xs) |< reset

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
computeWithPar :: NormalForm a => Strategy a
computeWithPar goal = getNormalForm goal >>= fromList . parSearch . searchMSearch initCover

-- ---------------------------------------------------------------------------
-- Encapsulated search
-- ---------------------------------------------------------------------------

 -- |Collect results of a non-deterministic computation in a monadic structure
encapsulatedSearch :: (MonadSearch m, NormalForm a) => a -> Cover -> ConstStore -> m a
encapsulatedSearch x cd store = searchMSearch cd $ ((\y _ _ -> y) $!! x) cd store

-- ---------------------------------------------------------------------------
-- Generic search using MonadPlus instances for the result
-- ---------------------------------------------------------------------------

newtype DecisionMap = DecisionMap { decisionMap :: Map.Map Integer Decision }
  deriving Show

emptyDecisionMap :: DecisionMap
emptyDecisionMap = DecisionMap Map.empty

onDecisionMap :: (Map.Map Integer Decision -> Map.Map Integer Decision)
              -> DecisionMap -> DecisionMap
onDecisionMap f (DecisionMap m) = DecisionMap (f m)

instance Monad m => Store (StateT DecisionMap m) where
  getDecisionRaw u        = gets
                          $ Map.findWithDefault defaultDecision (mkInteger u)
                          . decisionMap
  setDecisionRaw u c
    | isDefaultDecision c = unsetDecisionRaw u
    | otherwise           = modify $ onDecisionMap $ Map.insert (mkInteger u) c
  unsetDecisionRaw u      = modify $ onDecisionMap $ Map.delete (mkInteger u)

searchMSearch :: (MonadSearch m, NormalForm a) => Cover -> a -> m a
searchMSearch cd x = evalStateT (searchMSearch' cd return x) emptyDecisionMap

searchMSearch' :: (NormalForm a, MonadSearch m, Store m) => Cover -> (a -> m b) -> a -> m b
searchMSearch' cd cont x = match mChoice mNarrowed mFree mFail mGuard mVal x
  where
  mFail d info  = szero d info
  mVal v        = searchNF (searchMSearch' cd) cont v

  mChoice d i a b = lookupDecision i >>= follow
    where
    follow ChooseLeft  = searchMSearch' cd cont a
    follow ChooseRight = searchMSearch' cd cont b
    follow NoDecision  = decide i ChooseLeft a `plus` decide i ChooseRight b
    follow c           = internalError $ "Search.mChoice: Bad decision " ++ show c
    plus = if isCovered d then splus d i else mplus

  mFree d i xs = lookupDecisionID i >>= follow
    where
    follow (LazyBind cs,_)  = processLB d i cs xs
    follow (ChooseN c _,j)  = searchMSearch' cd cont (ys !! c)
      where Free _ _ ys = try $ generate (supply j) d
    follow (NoDecision ,j)  = sumF j $
      zipWith3 (\m pm y -> decide i (ChooseN m pm) y) [0..] pns xs
    follow c             = internalError $ "Search.mFree: Bad decision "
                            ++ show c ++ " for " ++ show i
    pns = case i of
      FreeID     pns' _ -> pns'
      NarrowedID pns' _ -> pns'
      ChoiceID        _ -> internalError "Search.mFree.pns: ChoiceID"
    sumF j | isCovered d  = svar d i
           | otherwise    = var (cont (choicesCons d j xs))

  mNarrowed d i xs = lookupDecision i >>= follow
    where
    follow (LazyBind cs)  = processLB d i cs xs
    follow (ChooseN c _)  = searchMSearch' cd cont (xs !! c)
    follow NoDecision     = sumF $
      zipWith3 (\m pm y -> decide i (ChooseN m pm) y) [0..] pns xs
    follow c              = error $ "Search.mNarrowed: Bad decision " ++ show c
    pns = case i of
      FreeID     pns' _ -> pns'
      NarrowedID pns' _ -> pns'
      ChoiceID        _ -> error "Search.mNarrowed.pns: ChoiceID"
    sumF | isCovered d = ssum d i
         | otherwise   = msum

  mGuard d cs e
   | isCovered d = constrainMSearch d cs (searchMSearch' cd cont e)
   | otherwise = solve cd cs e >>= maybe (szero d defFailInfo) (searchMSearch' cd cont . snd)

  processLB d i cs xs = decide i NoDecision
                        $ guardCons d (StructConstr cs) (choicesCons d i xs)

  decide i c y = setDecision i c >> searchMSearch' cd cont y
  isCovered d = d < cd
