{-# LANGUAGE MagicHash #-}
module Curry_Prelude where

import GHC.Prim
import Data.IORef
import qualified Data.Map
import System.IO.Unsafe
import Control.Monad
import Control.Parallel.TreeSearch
import qualified Data.Foldable as Fold
import qualified Data.FMList as FM

data Choice
  = NoChoice
  | ChooseLeft
  | ChooseRight
  | BoundTo ID
  | BindTo ID
  deriving (Show,Eq)

----------------
-- IDSupply
----------------

newtype ID = ID (IORef Choice) deriving Eq

instance Show ID where show _ = "r"

mkInt :: ID -> Integer
mkInt = error "IDSupplyIORef.mkInt"

data IDSupply = IDSupply ID IDSupply IDSupply

thisID :: IDSupply -> ID
thisID (IDSupply i _ _) = i

thisIDForFree :: IDSupply -> ID
thisIDForFree = error "free variable must be used with other supply library"

bind :: ID -> ID -> Solved
bind i j = undefined

(=:=) :: Try a -> Try a -> C_Success
(=:=) = undefined

leftID, rightID :: ID -> ID
leftID  _ = undefined
rightID _ = undefined

nfChoiceIO :: (NormalFormIO a,NonDet a) => (a -> IO b) -> ID -> a -> a -> IO b
nfChoiceIO = undefined

narrow :: NonDet a => ID -> a -> a -> a
narrow = choiceCons

nfChoice :: (NormalForm a,NonDet b) => (a -> b) -> ID -> a -> a -> b
nfChoice cont i x1 x2 = choiceCons i (cont $!! x1) (cont $!! x2)

tryChoice :: ID -> a -> a -> Try a
tryChoice = Choice

leftSupply, rightSupply :: IDSupply -> IDSupply
leftSupply  (IDSupply _ s _) = s
rightSupply (IDSupply _ _ s) = s

initIDSupply :: IO IDSupply
initIDSupply = getPureSupply NoChoice

{-# NOINLINE getPureSupply #-}
getPureSupply :: Choice -> IO IDSupply
getPureSupply def = do
    s1 <- unsafeInterleaveIO (getPureSupply def)
    s2 <- unsafeInterleaveIO (getPureSupply def)
    n  <- unsafeInterleaveIO  (newIORef def)
    return (IDSupply (ID n) s1 s2)


lookupChoice :: ID -> IO Choice
lookupChoice (ID r) = readIORef r

setChoice :: ID -> Choice -> IO ()
setChoice (ID r) c = writeIORef r c

showsChoice :: Show a => Int -> ID -> a -> a -> ShowS
showsChoice d r x1 x2 =
  showChar '(' .
  showsPrec d x1 .
  showString " ?" . shows r .
  showsPrec d x2 .
  showChar ')'


data C_Bool = Bool_Choice ID C_Bool C_Bool
            | Bool_Fail
            | Bool_Guard Constraint C_Bool
            | C_True
            | C_False

data C_Int = Int_Choice ID C_Int C_Int
           | Int_Fail
           | Int_Guard Constraint C_Int
           | C_Int Int#
           | C_Integer Integer deriving (Show,Eq)

data C_List a
 = List_Choice ID (C_List a) (C_List a)
 | List_Fail
 | List_Guard Constraint (C_List a)
 | C_Nil
 | C_Cons a (C_List a)

instance NonDet (C_List a) where
  choiceCons = List_Choice
  failCons   = List_Fail
  guardCons  = List_Guard
  try (List_Choice i x1 x2) = tryChoice i x1 x2
  try List_Fail = Fail
  try (List_Guard c e) = Guard c e
  try v = Val v

setUnsetChoice :: a
setUnsetChoice = undefined

mkInteger :: C_Int -> C_Int
mkInteger x@(C_Int _) = C_Integer (read $ drop 6 $ show x)
mkInteger x = x

c_op_lt :: C_Int -> C_Int -> C_Bool
c_op_lt x1 x2
 = case x1 of
    C_Int x3 -> c_op_lt_2 x3 x2
    C_Integer i -> c_op_lt_2_Integer i (mkInteger x2)
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_lt x1001 x2) (c_op_lt x1002 x2)
    Int_Fail -> Bool_Fail

c_op_lt_2 :: Int# -> C_Int -> C_Bool
c_op_lt_2 x1 x2
 = case x2 of
    C_Int x3 -> if x1 <# x3 then C_True else C_False
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_lt_2 x1 x1001) (c_op_lt_2 x1 x1002)

c_op_lt_2_Integer :: Integer -> C_Int -> C_Bool
c_op_lt_2_Integer x1 x2
 = case x2 of
    C_Integer x3 -> if x1 < x3 then C_True else C_False
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_lt_2_Integer x1 x1001) (c_op_lt_2_Integer x1 x1002)


c_op_gt :: C_Int -> C_Int -> C_Bool
c_op_gt x1 x2
 = case x1 of
    C_Int x3 -> c_op_gt_2 x3 x2
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_gt x1001 x2) (c_op_gt x1002 x2)

c_op_gt_2 :: Int# -> C_Int -> C_Bool
c_op_gt_2 x1 x2
 = case x2 of
    C_Int x3 -> if x1 ># x3 then C_True else C_False
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_gt_2 x1 x1001) (c_op_gt_2 x1 x1002)

c_op_eq_eq :: C_Int -> C_Int -> C_Bool
c_op_eq_eq x1 x2
 = case x1 of
    C_Int x3 -> c_op_eq_eq_2 x3 x2
    C_Integer x3 -> c_op_eq_eq_2_Integer x3 (mkInteger x2)
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_eq_eq x1001 x2) (c_op_eq_eq x1002 x2)
    Int_Fail -> Bool_Fail

c_op_eq_eq_2 :: Int# -> C_Int -> C_Bool
c_op_eq_eq_2 x1 x2
 = case x2 of
    C_Int x3 -> if x1 ==# x3 then C_True else C_False
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_eq_eq_2 x1 x1001) (c_op_eq_eq_2 x1 x1002)
    Int_Fail -> Bool_Fail

c_op_eq_eq_2_Integer :: Integer -> C_Int -> C_Bool
c_op_eq_eq_2_Integer x1 x2
 = case x2 of
    C_Integer x3 -> if x1 == x3 then C_True else C_False
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_eq_eq_2_Integer x1 x1001)
                       (c_op_eq_eq_2_Integer x1 x1002)

c_op_lt_eq :: C_Int -> C_Int -> C_Bool
c_op_lt_eq x1 x2
 = case x1 of
    C_Int x3 -> c_op_lt_eq_2 x3 x2
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_lt_eq x1001 x2) (c_op_lt_eq x1002 x2)

c_op_lt_eq_2 :: Int# -> C_Int -> C_Bool
c_op_lt_eq_2 x1 x2
 = case x2 of
    C_Int x3 -> if x1 <=# x3 then C_True else C_False
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_lt_eq_2 x1 x1001) (c_op_lt_eq_2 x1 x1002)

c_op_slash_eq :: C_Int -> C_Int -> C_Bool
c_op_slash_eq x1 x2
 = case x1 of
    C_Int x3 -> c_op_slash_eq_2 x3 x2
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_slash_eq x1001 x2) (c_op_slash_eq x1002 x2)

c_op_slash_eq_2 :: Int# -> C_Int -> C_Bool
c_op_slash_eq_2 x1 x2
 = case x2 of
    C_Int x3 -> if x1 /=# x3 then C_True else C_False
    Int_Choice x1000 x1001 x1002 ->
     Bool_Choice x1000 (c_op_slash_eq_2 x1 x1001) (c_op_slash_eq_2 x1 x1002)

c_op_minus :: C_Int -> C_Int -> C_Int
c_op_minus x1 x2
 = case x1 of
    C_Int x3 -> c_op_minus_2 x3 x2
    Int_Choice x1000 x1001 x1002 ->
     Int_Choice x1000 (c_op_minus x1001 x2) (c_op_minus x1002 x2)
    Int_Fail -> Int_Fail

c_op_minus_2 :: Int# -> C_Int -> C_Int
c_op_minus_2 x1 x2
 = case x2 of
    C_Int x3 -> C_Int (x1 -# x3)
    Int_Choice x1000 x1001 x1002 ->
     Int_Choice x1000 (c_op_minus_2 x1 x1001) (c_op_minus_2 x1 x1002)
    Int_Fail -> Int_Fail

c_op_plus :: C_Int -> C_Int -> C_Int
c_op_plus x1 x2
 = case x1 of
    C_Int x3 -> c_op_plus_2 x3 x2
    Int_Choice x1000 x1001 x1002 ->
     Int_Choice x1000 (c_op_plus x1001 x2) (c_op_plus x1002 x2)

c_op_plus_2 :: Int# -> C_Int -> C_Int
c_op_plus_2 x1 x2
 = case x2 of
    C_Int x3 -> C_Int (x1 +# x3)
    Int_Choice x1000 x1001 x1002 ->
     Int_Choice x1000 (c_op_plus_2 x1 x1001) (c_op_plus_2 x1 x1002)

c_op_mult :: C_Int -> C_Int -> C_Int
c_op_mult x1 x2
 = case x1 of
    C_Integer x3 -> c_op_mult_2 x3 x2
    Int_Choice x1000 x1001 x1002 ->
     Int_Choice x1000 (c_op_mult x1001 x2) (c_op_mult x1002 x2)
    _ -> c_op_mult (mkInteger x1) x2

c_op_mult_2 :: Integer -> C_Int -> C_Int
c_op_mult_2 x1 x2
 = case x2 of
    C_Integer x3 -> C_Integer (x1 * x3)
    Int_Choice x1000 x1001 x1002 ->
     Int_Choice x1000 (c_op_mult_2 x1 x1001) (c_op_mult_2 x1 x1002)
    _ -> c_op_mult_2 x1 (mkInteger x2)

c_mod :: C_Int -> C_Int -> C_Int
c_mod x1 x2
 = case x1 of
    C_Int x3 -> c_mod_2 x3 x2
    Int_Choice x1000 x1001 x1002 ->
     Int_Choice x1000 (c_mod x1001 x2) (c_mod x1002 x2)

c_mod_2 :: Int# -> C_Int -> C_Int
c_mod_2 x1 x2
 = case x2 of
    C_Int x3 -> C_Int (remInt# x1 x3)
    Int_Choice x1000 x1001 x1002 ->
     Int_Choice x1000 (c_mod_2 x1 x1001) (c_mod_2 x1 x1002)

data Try a
  = Fail
  | Val a
  | Free ID a a
  | Choice ID a a
  | Guard Constraint a
  deriving Show

class NonDet a where
  choiceCons :: ID -> a -> a -> a
  failCons :: a
  guardCons :: Constraint -> a -> a
  try :: a -> Try a

instance NonDet C_Bool where
  choiceCons = Bool_Choice
  failCons = Bool_Fail
  guardCons = Bool_Guard

  try Bool_Fail = Fail
  try (Bool_Choice i x1 x2) = tryChoice i x1 x2
  try (Bool_Guard c e)      = Guard c e
  try v = Val v

instance NonDet C_Int  where
  choiceCons = Int_Choice
  failCons = Int_Fail
  guardCons = Int_Guard

  try Int_Fail             = Fail
  try (Int_Choice i x1 x2) = tryChoice i x1 x2
  try (Int_Guard c e)      = Guard c e
  try v                    = Val v

class NonDet a => Generatable a where
  generate :: IDSupply -> a

{- moved to IDSupplyUnification

instance Generatable C_Bool where
  generate s = Bool_Choice (thisIDForFree s) C_False C_True

instance Generatable a => Generatable (C_List a) where
  generate s = let! i = thisIDForFree (leftSupply s)
                in let! s' = rightSupply s
                    in let! l = leftSupply s'
                        in let! r = rightSupply s'
                            in List_Choice i C_Nil (C_Cons (generate l) (generate r))

-}

c_op_qmark :: NonDet a => a -> a -> IDSupply -> a
c_op_qmark x y ids = let i = thisID ids in i `seq` choiceCons i x y



------------------
-- higher order
------------------

data Func a b = Func (a -> IDSupply -> b)
              | Func_Choice ID (Func a b) (Func a b)
              | Func_Fail
              | Func_Guard Constraint (Func a b)

instance NonDet (Func a b) where
  choiceCons = Func_Choice
  failCons = Func_Fail
  guardCons = Func_Guard
  try (Func_Choice i x1 x2) = Choice i x1 x2
  try (Func_Fail) = Fail
  try v = Val v

--c_apply :: NonDet b => Func a  b -> a -> IDSupply -> b
c_apply (Func f) s x = f s x
--c_apply (Func_Choice r f1 f2) s x = choiceCons r (c_apply f1 s x) (c_apply f2 s x)

d_apply :: (a -> b) -> a -> b
d_apply f x = f x

wrapD :: (a -> b) -> Func a b
wrapD f = Func (\ x s -> f x)

wrapN :: (a -> IDSupply -> b) -> Func a b
wrapN = Func

-----------------------
-- print vals dfs
-----------------------

prdfs x = printValsDFS (try (id $!! x))

printValsDFS :: (NormalFormIO a,Show a,NonDet a) => Try a -> IO ()
printValsDFS x@Fail         = print "case: Fail" >> print x
printValsDFS (Val v)        = print "case: Val" >> print v
printValsDFS (Free i x y)   = print "case: Free" >> lookupChoice i >>= choose
 where
   choose ChooseLeft  = (printValsDFS . try) $!< x
   choose ChooseRight = (printValsDFS . try) $!< y
   -- we need some more deref if we really want to rely on this output
   choose NoChoice    = print i
printValsDFS (Choice i x y) = print "case: Choice" >> lookupChoice i >>= choose
 where
   choose ChooseLeft  = printValsDFS (try x)
   choose ChooseRight = printValsDFS (try y)
   choose NoChoice    = do newChoice ChooseLeft  x
                           newChoice ChooseRight y

   newChoice j a = do setChoice i j
                      printValsDFS (try a)
                      setChoice i NoChoice

-----------------------
-- search
-----------------------

data MList m a = MCons a (m (MList m a))
               | MNil
               | Abort
               | WithReset (m (MList m a)) (m ())
{-
mapMList :: Monad m => (a -> b) -> MList m a -> MList m b
mapMList f MNil = MNil
mapMList f (MCons x ys) = MCons (f x) (ys >>= return . mapMList f)
-}
foldrMList :: Monad m => (a -> m b -> m b) -> m b -> MList m a -> m b
foldrMList f e MNil = e
foldrMList f e (MCons x ys) = f x (ys >>= foldrMList f e)

type IOList a = MList IO a

printVals :: Show a => IOList a -> IO ()
printVals MNil              = return ()
printVals (MCons x getRest) = print x >> getRest >>= printVals
printVals (WithReset l _) = l >>= printVals

countVals :: IOList a -> IO ()
countVals x = putStr "Number of Solutions: " >> count 0 x >>= print
  where
    count i MNil = return i
    count i (WithReset l _) = l >>= count i
    count i (MCons _ cont) = do
          let! i' = i+1
          cont >>= count i'

mnil, abort :: Monad m => m (MList m a)
mnil = return MNil
abort = return Abort

mcons :: Monad m => a -> m (MList m a) -> m (MList m a)
mcons x xs = return (MCons x xs)

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

(|<) :: Monad m => m (MList m a) ->  m () -> m (MList m a)
l |< r = return (WithReset l r)
{-getXs |< reset = do
  xs <- getXs
  case xs of
    MCons x getTail -> mcons x (getTail |< reset)
    end -> reset >> return end
-}
-----------------------
-- depth first
-----------------------

debug = False --True

-- toplevel

dfs g = searchDFS (try (id $!! g)) >>= if debug then printVals else countVals

searchDFS :: (NormalFormIO a,NonDet a) => Try a -> IO (IOList a)
searchDFS Fail             = mnil
searchDFS (Free i x1 x2)   = lookupChoice i >>= choose
  where
    choose ChooseLeft  = (searchDFS . try) $!< x1
    choose ChooseRight = (searchDFS . try) $!< x2
    choose NoChoice    = mcons (choiceCons i x1 x2) mnil
searchDFS (Val v)          = mcons v mnil
searchDFS (Choice i x1 x2) = lookupChoice i >>= choose
  where
    choose ChooseLeft  = searchDFS (try x1)
    choose ChooseRight = searchDFS (try x2)
    choose NoChoice    = newChoice ChooseLeft x1 +++ newChoice ChooseRight x2

    newChoice c x = do setChoice i c
                       searchDFS (try x) |< setChoice i NoChoice

searchDFS (Guard cs e) = do
  mreset <- solves cs
  case mreset of
    Nothing    -> mnil
    Just reset -> ((searchDFS . try) $!< e) |< reset

solves :: Constraint -> Solved
solves [] = return (Just (return ()))
solves (c:cs) = do
  mreset <- solve c
  case mreset of
    Nothing    -> return Nothing
    Just reset -> do
      mreset' <- solves cs
      case mreset' of
        Nothing -> return Nothing
        Just reset' -> return (Just (reset >> reset'))



type Solved = IO (Maybe (IO ()))

solved :: Solved
solved = return (Just (return ()))

unsolvable :: Solved
unsolvable = return Nothing

solve :: UniConstraint -> Solved
solve (i :=: cc) = lookupChoice i >>= choose cc
  where
    choose (BindTo j) ci       = lookupChoice j >>= check j ci
    choose c          NoChoice = setUnsetChoice i c
    choose c          x | c==x = solved
    choose c          ci       = unsolvable

    check j NoChoice NoChoice = setUnsetChoice i (BindTo j)

    check _ NoChoice y        = setUnsetChoice i y
    check j x        NoChoice = setUnsetChoice j x

    check _ x        y | x==y = solved

    check _ _ _               = unsolvable

-- encapsulated

instance Monad C_List where
  return x = C_Cons x C_Nil

  C_Nil       >>= _ = C_Nil
  C_Cons x xs >>= f = append (f x) (xs >>= f)

append :: C_List a -> C_List a -> C_List a
append C_Nil         ys = ys
append (C_Cons x xs) ys = C_Cons x (append xs ys)

instance MonadPlus C_List where
  mzero = C_Nil
  mplus = append

c_dfs :: (NormalForm a,NonDet a) => a -> C_List a
c_dfs x = searchMPlus Data.Map.empty (try (id $!! x))

{-
dfsInt :: C_Int -> IO (MList C_Int)
dfsInt (Int_Choice r x1 x2) = dfsOr dfsInt r x1 x2
dfsInt i = return (MCons i mnil)

dfsListInt :: C_List C_Int -> IO (MList (C_List C_Int))
dfsListInt (List_Choice r x1 x2) = dfsOr dfsListInt r x1 x2
dfsListInt (C_Cons x xs) = do
  MCons i mcont  <- dfsInt x
  dfsTailInt i mcont xs
dfsListInt i = return (MCons i mnil)

dfsTailInt i mcont xs = do
  MCons is mconts <- dfsListInt xs
  dfsTailCont i mcont xs is mconts

dfsTailCont i Nothing xs is Nothing =
  return (MCons (C_Cons i is) Nothing)
dfsTailCont i (Just cont) xs is Nothing =
  return $ MCons (C_Cons i is) $ Just $ do
    MCons i' mcont' <- cont
    dfsTailInt i' mcont' xs
dfsTailCont i mcont xs is (Just conts) = do
  return $ MCons (C_Cons i is) $ Just $ do
    MCons is' mconts' <- conts
    dfsTailCont i mcont xs is' mconts'
-}
-----------------------
-- breadth first
-----------------------

-- toplevel

bfs g = searchBFS (try (id $!! g)) >>= if debug then printVals else countVals

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

    next []  []                 = mnil
    next []  ((set,reset,y):ys) = bfs ys [] set reset (try y)
    next ((set,reset,x):xs) ys  = bfs xs ys set reset (try x)

-- encapsulated

c_bfs :: (NormalForm a,NonDet a) => a -> C_List a
c_bfs x = Fold.foldr (C_Cons) C_Nil (fmbfs x)

fmbfs :: (NormalForm a,NonDet a) => a -> FM.FMList a
fmbfs x = searchMPlus Data.Map.empty (try (id $!! x))

-------------------------
-- iterative depth first
-------------------------

stepIDFS = 10

idfs goal = initIDSupply >>= \s -> iter s 0 >>= countVals
  where iter s n = startIDFS s goal n ++++ iter s (n+stepIDFS)
    --iter 0 >>= countVals
  --where iter n = startIDFS goal n ++++ iter (n+1)

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
{-
getList ++++ getYs = do
     l <- getList
     case l of
       MNil  -> mnil
       Abort -> getYs
       WithReset getList' r ->
       MCons x getXs -> mcons x (getXs ++++ getYs)
-}

startIDFS s goal n = idfsHNF n (id $!! goal s)

idfsHNF :: (Show a,NonDet a) => Int -> a -> IO (IOList a)
idfsHNF n x = case try x of
  Val v -> if n<stepIDFS then mcons x mnil else mnil
  Fail  -> mnil
  Choice i x1 x2 -> do
    c <- lookupChoice i
    case c of
      ChooseLeft   -> idfsHNF n x1
      ChooseRight  -> idfsHNF n x2
      NoChoice -> if n > 0
                  then choose ChooseLeft x1 +++ choose ChooseRight x2
                  else abort
     where
      choose c x = do
       setChoice i c
       idfsHNF (n - 1) x |< setChoice i NoChoice


-----------------------
-- parallel
-----------------------

par :: (NonDet a, NormalForm a) => a -> IO ()
par g = print (length $ parSearch (searchMPlus Data.Map.empty (try (id $!! g))))

type SetOfChoices = Data.Map.Map Integer Choice

lookupChoice' :: SetOfChoices -> ID -> Choice
lookupChoice' set r =
  maybe NoChoice id (Data.Map.lookup (mkInt r) set)

setChoice' :: SetOfChoices -> ID -> Choice -> SetOfChoices
setChoice' set r c = Data.Map.insert (mkInt r) c set

searchMPlus :: (NonDet a, MonadPlus m) => SetOfChoices -> Try a -> m a
searchMPlus _   Fail           = mzero
searchMPlus _   (Val v)        = return v
searchMPlus set (Choice i x y) = choose (lookupChoice' set i)
  where
    choose ChooseLeft  = searchMPlus set (try x)
    choose ChooseRight = searchMPlus set (try y)
    choose NoChoice    = searchMPlus (pick ChooseLeft)  (try x)
                 `mplus` searchMPlus (pick ChooseRight) (try y)

    pick = setChoice' set i

---------------------------------------------------------
-- encapsulated search tree in Module IDSupplyInteger
---------------------------------------------------------



-----------------------
-- normal form
-----------------------

class NonDet a => NormalForm a where
  ($!!) :: NonDet b => (a -> b) -> a -> b


instance NormalForm C_Int where
  cont $!! (Int_Choice r x1 x2) = nfChoice cont r x1 x2
  cont $!! Int_Fail             = failCons
  cont $!! v                    = cont v

instance NormalForm a => NormalForm (C_List a) where
  cont $!! C_Nil = cont C_Nil
  cont $!! C_Cons x xs = (\ x' -> (\ xs' -> cont (C_Cons x' xs')) $!! xs) $!! x
  cont $!! l = cont $$!! l

instance NormalForm C_Bool where
  cont $!! v@C_True  = cont v
  cont $!! v@C_False = cont v
  cont $!! x = cont $$!! x

class NonDet a => NormalFormIO a where
  ($!<) :: (a -> IO b) -> a -> IO b

-----------------------
-- unification
-----------------------

-- (=:=) defined in supply file
--c_op_eq_colon_eq :: Unifiable a => a -> a -> Success
c_op_eq_colon_eq x y = x =:= y


c_seq = error "implement seq"

instance Show a => Show (C_List a) where
  showsPrec d (List_Choice r x1 x2) = showsChoice d r x1 x2
  showsPrec d (List_Guard c e)      = showsPrec d c . showString " &> " . showsPrec d e
  showsPrec d List_Fail = showChar '!'
  showsPrec d (C_Cons x xs) = showsPrec d x .
                              showString ":" .
                              showsPrec d xs
  showsPrec d C_Nil = showString "[]"

instance Show C_Bool where
  showsPrec d (Bool_Choice r x1 x2) = showsChoice d r x1 x2
  showsPrec d Bool_Fail = showChar '!'
  showsPrec d C_True  = showString "True"
  showsPrec d C_False = showString "False"

data C_Success = C_Success
               | Success_Choice ID C_Success C_Success
               | Success_Fail
               | Success_Guard Constraint C_Success deriving (Show,Eq)

success   = C_Success
c_success = success
type Success = C_Success


instance NonDet C_Success where
  choiceCons = Success_Choice
  failCons   = Success_Fail
  guardCons  = Success_Guard

  try (Success_Choice i x y) = tryChoice i x y
  try Success_Fail           = Fail
  try (Success_Guard c e)    = Guard c e
  try v = Val v

($$!!) :: (NormalForm a, NonDet b) => (a -> b) -> a -> b
cont $$!! x = nf (try x)
  where
    nf (Val v)        = cont $!! v
    nf Fail           = failCons
    nf (Choice i x y) = choiceCons i (nf (try x)) (nf (try y))
    nf (Free i x y)   = cont (choiceCons i x y)
    nf (Guard c e)    = guardCons c (nf (try e))


($$!<) :: (NormalFormIO a) => (a -> IO b) -> a -> IO b
cont $$!< x = nf (try x)
  where
    nf (Val v)        = cont $!< v
    nf Fail           = cont failCons
    nf (Choice i x y) = nfChoiceIO cont i x y
    nf (Free i x y)   = nfChoiceIO cont i x y
    nf (Guard c e)    = cont (guardCons c e)

instance NormalForm C_Success where
  cont $!! s@C_Success = cont s
  cont $!! x = cont $$!! x

instance NormalFormIO C_Success where
  cont $!< C_Success = cont C_Success
  cont $!< s         = cont $$!< s

instance NormalFormIO C_Int where
  cont $!< i@(C_Int _) = cont i
  cont $!< s           = cont $$!< s

instance NormalFormIO C_Bool where
  cont $!< C_True  = cont C_True
  cont $!< C_False = cont C_False
  cont $!< s       = cont $$!< s

instance NormalFormIO a => NormalFormIO (C_List a) where
  cont $!< C_Nil       = cont C_Nil
  cont $!< C_Cons x xs = (\ x' -> (\ xs' -> cont (C_Cons x' xs')) $!< xs) $!< x
  cont $!< s           = cont $$!< s


data UniConstraint = ID :=: Choice deriving (Show,Eq)

type Constraint = [UniConstraint]

c_op_and_gt, (&>) :: NonDet a => C_Success -> a -> a
c_op_and_gt c e = const e $!! c
(&>) = c_op_and_gt

-------------------------
-- set-valued functions
-------------------------

data Values a = Values [a]
           | Values_Choice ID (Values a) (Values a)
           | Values_Guard Constraint (Values a)
           | Values_Fail deriving (Show)

type C_Values a = Values a

instance NonDet (Values a) where
  failCons   = Values_Fail
  choiceCons = Values_Choice
  guardCons  = Values_Guard

  try Values_Fail           = Fail
  try (Values_Choice i x y) = Choice i x y
  try x                  = Val x

instance NormalForm a => NormalForm (Values a) where
  cont $!! (Values_Choice r x1 x2) = nfChoice cont r x1 x2
  cont $!! Values_Fail             = failCons
  cont $!! vs                   = cont vs

c_isEmpty (Values xs)           = if Prelude.null xs then C_True else C_False
c_isEmpty (Values_Choice i x y) = choiceCons i (c_isEmpty x) (c_isEmpty y)
c_isEmpty Values_Fail           = failCons

empty :: Values a
empty = Values []

insert :: a -> Values a -> Values a
insert x (Values xs) = Values (x:xs)

union :: Values a -> Values a -> Values a
union (Values xs) (Values ys) = Values (merge xs ys)
     where merge []     ys = ys
           merge (x:xs) ys = x : merge ys xs
union (Values_Choice r x y) ys = Values_Choice r (union x ys) (union y ys)
union xs (Values_Choice r x y) = Values_Choice r (union xs x) (union xs y)
union Values_Fail _ = Values_Fail
union _ Values_Fail = Values_Fail


instance Monad Values where
  return x = insert x empty
  Values xs >>= f = foldr union empty (map f xs)

instance MonadPlus Values where
  mzero = empty
  mplus = union
