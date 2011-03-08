{-# LANGUAGE TypeOperators #-}

module Basics where

import ID

data Constraint = ID :=: Choice
 deriving Show

data Try a
  = Val a
  | Fail
  | Choice ID a a
  | Free ID a a
  | Guard Constraint a
  deriving Show

tryChoice :: ID -> a -> a -> Try a
tryChoice i@(ID _)     = Choice i
tryChoice i@(FreeID _) = Free i

-- ---------------------------------------------------------------------------
-- Non-determinism
-- ---------------------------------------------------------------------------

-- Class for data that support nondeterministic values
class NonDet a where
  choiceCons :: ID -> a -> a -> a
  failCons   :: a
  guardCons  :: Constraint -> a -> a
  try        :: a -> Try a

narrow :: NonDet a => ID -> a -> a -> a
narrow (FreeID i) = choiceCons (ID i)
narrow i = choiceCons i

-- Class for data that support generators
class NonDet a => Generable a where
  generate :: IDSupply -> a

-- ---------------------------------------------------------------------------
-- Computations to normal form
-- ---------------------------------------------------------------------------

-- Class for data that supports normal form computations.
-- The normal form computation is combined with a continuation to be
-- applied to the normal form.
class NonDet a => NormalForm a where
  ($!!) :: NonDet b => (a -> b) -> a -> b

-- Auxiliary operation to apply a continuation to the normal form.
($$!!) :: (NormalForm a, NonDet b) => (a -> b) -> a -> b
cont $$!! x = nf (try x)
  where
    nf (Val v)        = cont $!! v
    nf Fail           = failCons
    nf (Choice i x y) = choiceCons i (nf (try x)) (nf (try y))
    nf (Free i x y)   = error "($$!!) with free variable" -- was: cont (choiceCons i x y)
    nf (Guard c e)    = guardCons c (nf (try e))

nfChoice :: (NormalForm a, NonDet b) => (a -> b) -> ID -> a -> a -> b
nfChoice cont i x1 x2 = choiceCons i (cont $!! x1) (cont $!! x2)

-- ---------------------------------------------------------------------------
-- Unification
-- ---------------------------------------------------------------------------

-- Class for data that support unification
class (NonDet a, NormalForm a) => Unifiable a where
  (=.=) :: a -> a -> C_Success
  bind :: ID -> a -> [Constraint]

(=:=) :: Unifiable a => a -> a -> C_Success
_ =:= _ = error "(=:=) undefined"

(&) :: C_Success -> C_Success -> C_Success
_ & _ = error "(&) undefined"

-- ---------------------------------------------------------------------------
-- Built-in types
-- ---------------------------------------------------------------------------

-- The implementation of the Success type must be added here since it is used
-- in the class Unifiable.
data C_Success
  = C_Success
  | Choice_C_Success ID C_Success C_Success
  | Fail_C_Success
  | Guard_C_Success Constraint C_Success

instance Eq C_Success where
  C_Success == C_Success = True
  _         == _         = False

instance Ord C_Success where
  C_Success <= C_Success = True
  _         <= _         = False

instance Show C_Success where
  showsPrec d C_Success = showString "success"
  showsPrec d (Choice_C_Success i x y) = showsChoice d i x y
  showsPrec d Fail_C_Success = showChar '!'

instance NonDet C_Success where
  choiceCons = Choice_C_Success
  failCons   = Fail_C_Success
  guardCons  = Guard_C_Success
  try (Choice_C_Success i x y) = tryChoice i x y
  try Fail_C_Success           = Fail
  try (Guard_C_Success c e)    = Guard c e
  try x = Val x

instance Generable C_Success where
  generate _ = C_Success

instance NormalForm C_Success where
  cont $!! s@C_Success = cont s
  cont $!! x = cont $$!! x

instance Unifiable C_Success where
  C_Success =.= C_Success = C_Success
  _         =.= _         = Fail_C_Success
  bind i (Choice_C_Success j@(FreeID _) _ _) = [(i :=: (BindTo j))]

-- Higher Order Funcs

data Func a b = Func (a -> IDSupply -> b)
              | Func_Choice ID (Func a b) (Func a b)
              | Func_Fail
              | Func_Guard Constraint (Func a b)

instance Eq (Func a b) where
  f == g = False

instance Ord (Func a b) where
  f <= g = False

instance Show (Func a b) where
  show = error "show for Func is undefined"

instance NonDet (Func a b) where
  choiceCons = Func_Choice
  failCons = Func_Fail
  guardCons = Func_Guard
  try (Func_Choice i x1 x2) = Choice i x1 x2
  try (Func_Fail) = Fail
  try v = Val v

instance Generable (Func a b) where
  generate = error "generate for Func is undefined"

instance NormalForm (Func a b) where
  cont $!! f@(Func _) = cont f
  cont $!! f          = cont $$!! f

instance Unifiable (Func a b) where
  (=.=) = error "(=.=) for Func is undefined"
  bind = error "bind for Func is undefined"

-- Higher Order functions

instance Eq (a -> b) where
  f == g = False

instance Ord (a -> b) where
  f <= g = False

instance Show (a -> b) where
  show = error "show for function is undefined"

instance NonDet (a -> b) where
  choiceCons = undefined
  failCons = undefined
  guardCons = undefined
  try = undefined

instance Generable (a -> b) where
  generate = undefined

instance NormalForm (a -> b) where
  cont $!! f = cont f

instance Unifiable (a -> b) where
  (=.=) = error "(=.=) for function is undefined"
  bind = error "bind for function is undefined"

-- ---------------------------------------------------------------------------
-- IO
-- ---------------------------------------------------------------------------

-- TODO: reason about IO and non-determinism

data C_IO a
     = Choice_C_IO ID (C_IO a) (C_IO a)
     | Fail_C_IO
     | Guard_C_IO Constraint (C_IO a)
     | C_IO (IO a)

instance Eq (C_IO a) where
  (==) = error "(==) for C_IO"

instance Ord (C_IO a) where
  (<=) = error "(<=) for C_IO"

instance Show (C_IO a) where
  show = error "show for C_IO"

instance NonDet (C_IO a) where
  choiceCons = Choice_C_IO
  failCons = Fail_C_IO
  guardCons = Guard_C_IO
  try (Choice_C_IO i x y) = tryChoice i x y
  try Fail_C_IO = Fail
  try (Guard_C_IO c e) = Guard c e
  try x = Val x

instance Generable (C_IO a) where
  generate _ = error "C_IO: generate"

instance NormalForm (C_IO a) where
  cont $!! io@(C_IO _) = cont io
  cont $!! x           = cont $$!! x

instance Unifiable (C_IO a) where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_IO j@(FreeID _) _ _) = [(i :=: (BindTo j))]

toIO :: C_IO a -> IO a
toIO (C_IO io) = io

fromIO :: IO a -> C_IO a
fromIO io = C_IO io

-- ---------------------------------------------------------------------------
-- Auxiliaries for Show
-- ---------------------------------------------------------------------------

showsChoice :: Show a => Int -> ID -> a -> a -> ShowS
-- showsChoice d i@(FreeID _) _ _ = shows i
showsChoice d r x1 x2 =
  showChar '(' .
  showsPrec d x1 .
  showString " ?" . shows r . showChar ' ' .
  showsPrec d x2 .
  showChar ')'

showsGuard :: (Show a, Show b) => Int -> a -> b -> ShowS
showsGuard d c e = showsPrec d c . showString " &> " . showsPrec d e

-- ---------------------------------------------------------------------------
-- Auxiliaries for non-determinism
-- ---------------------------------------------------------------------------

-- make a deterministic function non-deterministic
nd :: (a -> b) -> a -> IDSupply -> b
nd f a _ = f a

wrapDX :: (c -> b) -> (a -> c) -> Func a b
wrapDX wrap f = wrapNX wrap (nd f)

wrapNX :: (c -> b) -> (a -> IDSupply -> c) -> Func a b
wrapNX wrap f = Func (\a s -> wrap $ f a s)

eval :: (IDSupply -> a) -> IO a
eval goal = initSupply >>= return . goal

evalIO :: (IDSupply -> C_IO a) -> IO a
evalIO goal = initSupply >>= (\s -> let (C_IO act) = goal s in act)

d_dollar_bang :: (NonDet a, NonDet b) => (a -> b) -> a -> b
d_dollar_bang f x = hnf (try x)
  where
   hnf (Val v) = f v
   hnf Fail    = failCons
   hnf (Choice id a b) = choiceCons id (hnf (try a)) (hnf (try b))
   -- TODO give reasonable implementation (see $$!!)
   hnf (Free id a b) = error "d_dollar_bang with free variable"
   hnf (Guard c e) = guardCons c (hnf (try e))

nd_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> b
nd_dollar_bang f x s = hnf (try x)
  where
   hnf (Val v) = nd_apply f v s
   hnf Fail    = failCons
   -- TODO Do we have to use leftSupply and rightSupply?
   hnf (Choice id a b) = choiceCons id (hnf (try a)) (hnf (try b))
   -- TODO give reasonable implementation (see $$!!)
   hnf (Free id a b) = error "nd_dollar_bang with free variable"
   hnf (Guard c e) = guardCons c (hnf (try e))

d_apply :: (a -> b) -> a -> b
d_apply f a = f a

-- TODO: Support non-deterministic Funcs
nd_apply :: NonDet b => Func a b -> a -> IDSupply -> b
nd_apply fun a s = (\(Func f) -> f a s) `d_dollar_bang` fun

{-
-- wrap a higher-order function with one argument
wrapD :: (a -> b) -> a :-> b
wrapD f = wrapDX id f
-- Func (\a _ -> f a)

wrapD2 :: (a -> b -> c) -> a :-> b :-> c
wrapD2 f = wrapDX (wrapDX id) f

wrapD3 :: (a -> b -> c -> d) -> a :-> b :-> c :-> d
wrapD3 f = wrapDX (wrapDX (wrapDX id)) f

wrapN :: (a -> IDSupply -> b) -> Func a b
wrapN f = wrapNX id f

wrapN2 :: (a -> b -> IDSupply -> c) -> a :-> b :-> c
wrapN2 f = wrapDX (wrapNX id) f

wrapN3 :: (a -> b -> c -> IDSupply -> d) -> a :-> b :-> c :-> d
wrapN3 f = wrapDX (wrapDX (wrapNX id)) f

unwrap :: Func a b -> IDSupply -> a -> b
unwrap (Func f) s x = f x s
-}


----------------------------------------------------------------------
-- Printing all results of a computation in a depth-first manner
----------------------------------------------------------------------

-- Evaluate a nondeterministic expression and show all results
-- in depth-first order
prdfs :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
prdfs mainexp = eval mainexp >>= \x -> printValsDFS (try (id $!! x))

printValsDFS :: (Show a,NonDet a) => Try a -> IO ()
printValsDFS x@Fail         = return () --print "Failure: " >> print x
printValsDFS (Val v)        = print v
{-
printValsDFS (Free i x y)   = print "case: Free" >> lookupChoice i >>= choose
 where
   choose ChooseLeft  = (printValsDFS . try) $!< x
   choose ChooseRight = (printValsDFS . try) $!< y
   -- we need some more deref if we really want to rely on this output
   choose NoChoice    = print i
-}
printValsDFS (Choice i x y) = lookupChoice i >>= choose
 where
   choose ChooseLeft  = printValsDFS (try x)
   choose ChooseRight = printValsDFS (try y)
   choose NoChoice    = do newChoice ChooseLeft  x
                           newChoice ChooseRight y

   newChoice j a = do setChoice i j
                      printValsDFS (try a)
                      setChoice i NoChoice

----------------------------------------------------------------------
-- Data structures and operations for various search strategies
----------------------------------------------------------------------

-- Monadic lists as a general representation of values obtained
-- in a mondic manner
data MList m a = MCons a (m (MList m a))
               | MNil
               | Abort
               | WithReset (m (MList m a)) (m ())

-- Construct an empty monadic list
mnil :: Monad m => m (MList m a)
mnil = return MNil

-- Construct a non-empty monadic list
mcons :: Monad m => a -> m (MList m a) -> m (MList m a)
mcons x xs = return (MCons x xs)

--- ???
abort :: Monad m => m (MList m a)
abort = return Abort

--- Concatenate two monadic lists
(+++) :: Monad m => m (MList m a) -> m (MList m a) -> m (MList m a)
get +++ getYs = withReset get (return ())
  where
    withReset getList reset = do
     l <- getList
     case l of
       WithReset getList' reset' -> withReset getList' (reset >> reset')
       MNil -> reset >> getYs
       Abort -> reset >> mayAbort getYs
       MCons x getXs -> mcons x (withReset getXs reset)
    mayAbort getYs = do
     ys <- getYs
     case ys of
       WithReset getYs' reset' -> mayAbort getYs' |< reset'
       MNil -> return Abort
       ys'  -> return ys'

-- Add a monadic action of with result type () to the end of a monadic list
(|<) :: Monad m => m (MList m a) ->  m () -> m (MList m a)
l |< r = return (WithReset l r)
{-getXs |< reset = do
  xs <- getXs
  case xs of
    MCons x getTail -> mcons x (getTail |< reset)
    end -> reset >> return end
-}

-- For convencience, we define a monadic list for the IO monad:
type IOList a = MList IO a

-- Print all values of a IO monad list:
printVals :: Show a => IOList a -> IO ()
printVals MNil              = return ()
printVals (MCons x getRest) = print x >> getRest >>= printVals
printVals (WithReset l _) = l >>= printVals

-- Count and print the number of elements of a IO monad list:
countVals :: IOList a -> IO ()
countVals x = putStr "Number of Solutions: " >> count 0 x >>= print
  where
    count i MNil = return i
    count i (WithReset l _) = l >>= count i
    count i (MCons _ cont) = do
          let !i' = i+1
          cont >>= count i'

----------------------------------------------------------------------
-- Depth-first search into a monadic list
----------------------------------------------------------------------

-- Print all values of a non-deterministic goal in a depth-first manner:
printDFS :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printDFS mainexp =
  eval mainexp >>= \x -> searchDFS (try (id $!! x)) >>= printVals -- countVals

--searchDFS :: (NormalFormIO a,NonDet a) => Try a -> IO (IOList a)
searchDFS :: NonDet a => Try a -> IO (IOList a)
searchDFS Fail             = mnil
{-
searchDFS (Free i x1 x2)   = lookupChoice i >>= choose
  where
    choose ChooseLeft  = (searchDFS . try) $!< x1
    choose ChooseRight = (searchDFS . try) $!< x2
    choose NoChoice    = mcons (choiceCons i x1 x2) mnil
-}
searchDFS (Val v)          = mcons v mnil
searchDFS (Choice i x1 x2) = lookupChoice i >>= choose
  where
    choose ChooseLeft  = searchDFS (try x1)
    choose ChooseRight = searchDFS (try x2)
    choose NoChoice    = newChoice ChooseLeft x1 +++ newChoice ChooseRight x2

    newChoice c x = do setChoice i c
                       searchDFS (try x) |< setChoice i NoChoice
{-
searchDFS (Guard cs e) = do
  mreset <- solves cs
  case mreset of
    Nothing    -> mnil
    Just reset -> ((searchDFS . try) $!< e) |< reset
-}

----------------------------------------------------------------------
-- Breadth-first search into a monadic list
----------------------------------------------------------------------

-- Print all values of a non-deterministic goal in a breadth-first manner:
printBFS :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printBFS mainexp =
  eval mainexp >>= \x -> searchBFS (try (id $!! x)) >>= printVals -- countVals

searchBFS :: NonDet a => Try a -> IO (IOList a)
searchBFS x = bfs [] [] (return ()) (return ()) x
  where
    bfs xs ys _   reset Fail           = reset >> next xs ys
    bfs xs ys _   reset (Val v)        = reset >> mcons v (next xs ys)
    bfs xs ys set reset (Choice i x y) = set   >> lookupChoice i >>= choose

     where
        choose ChooseLeft  = bfs xs ys (return ()) reset (try x)
        choose ChooseRight = bfs xs ys (return ()) reset (try y)
        choose NoChoice    = do
          reset
          next xs ((newSet ChooseLeft , newReset, x) :
                   (newSet ChooseRight, newReset, y) : ys)

        newSet c = set   >> setChoice i c
        newReset = reset >> setChoice i NoChoice

    --TODO: cases for Free, Guard

    next []  []                 = mnil
    next []  ((set,reset,y):ys) = bfs ys [] set reset (try y)
    next ((set,reset,x):xs) ys  = bfs xs ys set reset (try x)

----------------------------------------------------------------------
-- Iterative depth-first search into a monadic list
----------------------------------------------------------------------

-- The increase step size for the iterative deepening strategy:
stepIDFS = 100

-- Print all values of a non-deterministic goal with a iterative
-- deepening strategy:
printIDS :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printIDS goal = initSupply >>= \s -> iter s 0 >>= printVals
  where iter s n = startIDS s goal n ++++ iter s (n+stepIDFS)

(++++) :: Monad m => m (MList m a) -> m (MList m a) -> m (MList m a)
get ++++ getYs = withReset get (return ())
  where
    withReset getList reset = do
     l <- getList
     case l of
       WithReset getList' reset' -> withReset getList' (reset >> reset')
       MNil -> reset >> mnil
       Abort -> reset >> getYs
       MCons x getXs -> mcons x (withReset getXs reset)


startIDS s goal n = idsHNF n (id $!! goal s)

idsHNF :: (Show a,NonDet a) => Int -> a -> IO (IOList a)
idsHNF n x = case try x of
  Val v -> if n<stepIDFS then mcons x mnil else mnil
  Fail  -> mnil
  Choice i x1 x2 -> do
    c <- lookupChoice i
    case c of
      ChooseLeft   -> idsHNF n x1
      ChooseRight  -> idsHNF n x2
      NoChoice -> if n > 0
                  then choose ChooseLeft x1 +++ choose ChooseRight x2
                  else abort
     where
      choose c x = do
       setChoice i c
       idsHNF (n - 1) x |< setChoice i NoChoice


