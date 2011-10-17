-- ---------------------------------------------------------------------------
-- Data structures and operations to collect and show results
-- w.r.t. various search strategies
-- ---------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module MonadList where

import Data.Char (toLower)
import System.IO (hFlush, stdin, stdout, hSetBuffering, BufferMode (..))

-- Monadic lists as a general representation of values obtained
-- in a mondic manner. The additional constructor Abort
-- represents an incomplete list due to reaching the depth-bound in
-- iterative deepening. The constructor (WithReset lis act) represents
-- a list lis where the monadic action act has to be performed at the
-- end of the list.
data MList m a
  = MNil
  | MCons a (m (MList m a))
  | Abort
  | WithReset (m (MList m a)) (m ())

-- Construct an empty monadic list
mnil :: Monad m => m (MList m a)
mnil = return MNil

-- Construct a non-empty monadic list
mcons :: Monad m => a -> m (MList m a) -> m (MList m a)
mcons x xs = return (MCons x xs)

-- Aborts a monadic list due to reaching the search depth (in iter. deepening)
abort :: Monad m => m (MList m a)
abort = return Abort

-- Add a monadic action with result type () to the end of a monadic list.
-- Used to reset a choice made via a dfs strategy.
(|<) :: Monad m => m (MList m a) ->  m () -> m (MList m a)
l |< r = return (WithReset l r)
{-getXs |< reset = do
  xs <- getXs
  case xs of
    MCons x getTail -> mcons x (getTail |< reset)
    end -> reset >> return end
-}

-- Concatenate two monadic lists
(+++) :: Monad m => m (MList m a) -> m (MList m a) -> m (MList m a)
get +++ getYs = withReset get (return ())
  where
    withReset getList outerReset = do
      l <- getList
      case l of
        WithReset getList' innerReset -> withReset getList' (innerReset >> outerReset)
        MNil  -> outerReset >> getYs -- perform action before going to next list
        Abort -> outerReset >> abortEnd getYs
        MCons x getXs -> mcons x (withReset getXs outerReset) -- move action down to end

    abortEnd getList = do -- move Abort down to end of second list
      ys <- getList
      case ys of
        WithReset getYs' innerReset -> abortEnd getYs' |< innerReset
        MNil  -> return Abort -- replace end of second list by Abort
        Abort -> return Abort
        MCons z getZs -> mcons z (abortEnd getZs)

-- Concatenate two monadic lists if the first ends with an Abort
(++++) :: Monad m => m (MList m a) -> m (MList m a) -> m (MList m a)
get ++++ getYs = withReset get (return ())
  where
    withReset getList outerReset = do
      l <- getList
      case l of
        WithReset getList' innerReset -> withReset getList' (innerReset >> outerReset)
        MNil  -> outerReset >> mnil
        Abort -> outerReset >> getYs -- ignore Abort when concatenating further vals
        MCons x getXs -> mcons x (withReset getXs outerReset)


-- For convencience, we define a monadic list for the IO monad:
type IOList a = MList IO a

-- Count and print the number of elements of a IO monad list:
countVals :: IOList a -> IO ()
countVals x = putStr "Number of values: " >> count 0 x >>= print
  where
    count :: Integer -> IOList a -> IO Integer
    count _ Abort = error "MonadList.countVals.count: Abort" -- TODO
    count i MNil = return i
    count i (WithReset l _) = l >>= count i
    count i (MCons _ cont) = do
      let !i' = i+1
      cont >>= count i'

-- Print the first value of a IO monad list:
printOneValue :: Show a => (a -> IO ()) -> IOList a -> IO ()
printOneValue _   Abort           = error "MonadList.printOneValue: Abort" -- TODO
printOneValue _   MNil            = putStrLn "No value"
printOneValue prt (MCons x _)     = prt x
printOneValue prt (WithReset l _) = l >>= printOneValue prt

-- Print all values of a IO monad list:
printAllValues :: Show a => (a -> IO ()) -> IOList a -> IO ()
printAllValues _   Abort             = error "MonadList.printAllValues: Abort" -- TODO
printAllValues _   MNil              = putStrLn "No more values"
printAllValues prt (MCons x getRest) = prt x >> getRest >>= printAllValues prt
printAllValues prt (WithReset l _)   = l >>= printAllValues prt

askKey :: IO ()
askKey = do
  putStr "Hit any key to terminate..."
  hFlush stdout
  hSetBuffering stdin NoBuffering
  _ <- getChar
  return ()

-- Type of default actions for interactive value printing
data MoreDefault = MoreYes | MoreNo | MoreAll deriving Eq

-- Print all values of a IO monad list on request by the user:
printValsOnDemand :: Show a => MoreDefault -> (a -> IO ()) -> IOList a -> IO ()
printValsOnDemand = printValsInteractive True

printValsInteractive :: Show a => Bool -> MoreDefault
                               -> (a -> IO ()) -> IOList a -> IO ()
printValsInteractive _ _ _ Abort = error "MonadList.printValsInteractive: Abort" -- TODO
printValsInteractive _ _ _ MNil = putStrLn "No more values" >> askKey
printValsInteractive st md prt (MCons x getRest) =
  prt x >> askMore st md prt getRest
printValsInteractive st md prt (WithReset l _) =
  l >>= printValsInteractive st md prt

-- ask the user for more values
askMore :: Show a => Bool -> MoreDefault
                  -> (a -> IO ()) -> IO (IOList a) -> IO ()
askMore st md prt getrest =
    if not st then getrest >>= printValsInteractive st md prt else do
  putStr $ "More values? ["++
           (if md==MoreYes then 'Y' else 'y'):"(es)/"++
           (if md==MoreNo  then 'N' else 'n'):"(o)/"++
           (if md==MoreAll then 'A' else 'a'):"(ll)] "
  hFlush stdout
  hSetBuffering stdin NoBuffering
  c <- getChar
  if c == '\n' then return () else putChar '\n'
  case (toLower c) of
    'y'  -> getrest >>= printValsInteractive st md prt
    'n'  -> return ()
    'a'  -> getrest >>= printValsInteractive False md prt
    '\n' -> case md of
              MoreYes -> getrest >>= printValsInteractive st md prt
              MoreNo  -> return ()
              MoreAll -> getrest >>= printValsInteractive False md prt
    _    -> askMore st md prt getrest

list2iolist :: [a] -> IO (IOList a)
list2iolist [] = mnil
list2iolist (x:xs) = mcons x (list2iolist xs)