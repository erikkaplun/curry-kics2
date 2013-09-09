-- ---------------------------------------------------------------------------
-- Data structures and operations to collect and show results
-- w.r.t. various search strategies
-- ---------------------------------------------------------------------------
module MonadList
  ( -- * Monadic list
    MList, mnil, msingleton, mcons, abort, (|<), (+++), (++++), fromList
    -- * list evaluation
  , IOList, MoreDefault (..), countValues, printOneValue, printAllValues
  , printValsOnDemand
  ) where

import Data.Char (toLower)
import System.IO (hFlush, stdin, stdout, hGetEcho, hSetEcho,
                  hGetBuffering, hSetBuffering, BufferMode (..))

-- |Monadic lists as a general representation of values obtained in a
-- monadic manner.
--
-- The additional constructor @Abort@ represents an incomplete
-- list due to reaching the depth-bound in iterative deepening.
-- The constructor @(WithReset list act)@ represents a list where the monadic
-- monadic reset action @act@ has to be performed at the end of the list.
data List m a
  -- |Empty list
  = Nil
  -- |List constructor
  | Cons a (MList m a)
  -- |Abortion at iterative deepening
  | Abort
  -- |Reset action to be performed afterwards
  | Reset (MList m a) (m ())

type MList m a = m (List m a)

-- |Construct an empty monadic list
mnil :: Monad m => MList m a
mnil = return Nil

-- |Construct a singleton monadic list
msingleton :: Monad m => a -> MList m a
msingleton x = mcons x mnil

-- |Construct a non-empty monadic list
mcons :: Monad m => a -> MList m a -> MList m a
mcons x xs = return (Cons x xs)

-- |Aborts a monadic list due to reaching the maximum search depth
-- (e.g., in iterative deepening)
abort :: Monad m => MList m a
abort = return Abort

-- |Add a monadic reset action to the end of a monadic list.
-- Used to reset a choice made via a dfs strategy.
(|<) :: Monad m => MList m a ->  m () -> MList m a
l |< r = return (Reset l r)

-- |Append two monadic lists
(+++) :: Monad m => MList m a -> MList m a -> MList m a
get +++ getYs = traverse get (return ())
  where
  traverse getList reset = do
    l <- getList
    case l of
      Nil                -> reset >> getYs -- perform reset before the next list
      Abort              -> reset >> deferAbort getYs
      Cons x getXs       -> x `mcons` traverse getXs reset -- move reset to end
      Reset getXs reset' -> traverse getXs (reset' >> reset)

  deferAbort getList = do -- move Abort down to end of second list
    ys <- getList
    case ys of
      Nil                -> return Abort -- replace end of second list by Abort
      Abort              -> return Abort
      Cons z getZs       -> z `mcons` deferAbort getZs
      Reset getZs reset  -> deferAbort getZs |< reset

-- Concatenate two monadic lists if the first ends with an Abort
(++++) :: Monad m => MList m a -> MList m a -> MList m a
get ++++ getYs = traverse get (return ())
  where
  traverse getList reset = do
    l <- getList
    case l of
      Nil                -> reset >> mnil
      Abort              -> reset >> getYs -- ignore Abort when concatenating further vals
      Cons x getXs       -> x `mcons` traverse getXs reset
      Reset getXs reset' -> traverse getXs (reset' >> reset)

-- |Construct a monadic list from a pure list
fromList :: Monad m => [a] -> MList m a
fromList = foldr mcons mnil

-- ---------------------------------------------------------------------------
-- IOList
-- ---------------------------------------------------------------------------

-- For convencience, we define a monadic list for the IO monad:
type IOList a = List IO a

-- Type of default actions for interactive value printing
data MoreDefault = MoreYes | MoreNo | MoreAll deriving Eq

-- |Count and print the number of elements of a IO monad list:
countValues :: IOList a -> IO ()
countValues x = putStr "Number of values: " >> count 0 x
  where
  count :: Integer -> IOList a -> IO ()
  count i Nil            = print i
  count i Abort          = print i >> warnAbort
  count i (Cons _ getXs) = let i' = i + 1 in i' `seq` getXs >>= count i'
  count i (Reset    l _) = l >>= count i

-- Print the first value of a IO monad list:
printOneValue :: Show a => (a -> IO ()) -> IOList a -> IO ()
printOneValue _   Nil         = putStrLn "No value"
printOneValue _   Abort       = warnAbort
printOneValue prt (Cons  x _) = prt x
printOneValue prt (Reset l _) = l >>= printOneValue prt

-- Print all values of a IO monad list:
printAllValues :: Show a => (a -> IO ()) -> IOList a -> IO ()
printAllValues _   Nil            = return ()
printAllValues _   Abort          = warnAbort
printAllValues prt (Cons x getXs) = prt x >> getXs >>= printAllValues prt
printAllValues prt (Reset    l _) = l >>= printAllValues prt

-- Print all values of a IO monad list on request by the user:
printValsOnDemand :: Show a => MoreDefault -> (a -> IO ()) -> IOList a -> IO ()
printValsOnDemand md prt rs = do
  te <- hGetEcho stdin
  tb <- hGetBuffering stdin
  printInteractive True md prt rs
  hSetEcho stdin te
  hSetBuffering stdin tb

warnAbort :: IO ()
warnAbort = putStrLn "Warning: Search aborted (maximum depth reached)"

-- Print all values of a IO monad list on request by the user:
printInteractive :: Show a => Bool -> MoreDefault -> (a -> IO ()) -> IOList a -> IO ()
printInteractive _        _  _   Abort          = warnAbort
printInteractive _        _  _   Nil            = putStrLn "No more values"
printInteractive stepWise md prt (Cons x getXs) = prt x >> askMore stepWise md prt getXs
printInteractive stepWise md prt (Reset    l _) = l >>= printInteractive stepWise md prt

-- ask the user for more values
askMore :: Show a => Bool -> MoreDefault -> (a -> IO ()) -> IO (IOList a) -> IO ()
askMore stepWise md prt getrest
  | not stepWise = getrest >>= printInteractive stepWise md prt
  | otherwise    = do
      c <- askUserKey prompt
      case toLower c of
        'y'  -> getrest >>= printInteractive stepWise md prt
        'n'  -> return ()
        'a'  -> getrest >>= printInteractive False md prt
        '\n' -> case md of
                  MoreYes -> getrest >>= printInteractive stepWise md prt
                  MoreNo  -> return ()
                  MoreAll -> getrest >>= printInteractive False md prt
        _    -> askMore stepWise md prt getrest
  where
  prompt = concat
    [ "More values? ["
    , (if md == MoreYes then 'Y' else 'y') : "(es)/"
    , (if md == MoreNo  then 'N' else 'n') : "(o)/"
    , (if md == MoreAll then 'A' else 'a') : "(ll)] "
    ]

askUserKey :: String -> IO Char
askUserKey prompt = do
  putStr prompt >> hFlush stdout
  hSetEcho stdin False  -- to avoid interference with rlwrap
  hSetBuffering stdin NoBuffering
  c <- getChar
  putChar c
  if c == '\n' then return () else putChar '\n'
  return c
