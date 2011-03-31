{-# LANGUAGE MagicHash, MultiParamTypeClasses, FlexibleInstances #-}

module Basics where

import ID
import qualified Data.Map
import Data.List (foldl')
import System.IO
import Control.Monad
import Control.Parallel.TreeSearch
import GHC.Exts (Int#, Char#, chr#)

nonAsciiChr :: Int# -> Char#
nonAsciiChr i = chr# i

data Try a
  = Val a
  | Fail
  | Choice ID a a
  | Free ID a a
  | Choices ID [a]
  | Frees ID [a]
  | Guard [Constraint] a
    deriving Show

tryChoice :: ID -> a -> a -> Try a
tryChoice i@(ID _)       = Choice i
tryChoice i@(FreeID _)   = Free i
tryChoice i@(Narrowed _) = Choice i

tryChoices :: ID -> [a] -> Try a
tryChoices i@(ID _)       = Choices i
tryChoices i@(FreeID _)   = Frees i
tryChoices i@(Narrowed _) = Choices i

-- ---------------------------------------------------------------------------
-- Non-determinism
-- ---------------------------------------------------------------------------

-- Class for data that support nondeterministic values
class NonDet a where
  choiceCons :: ID -> a -> a -> a
  choicesCons:: ID -> [a] -> a
  failCons   :: a
  guardCons  :: [Constraint] -> a -> a
  try        :: a -> Try a
                                         -- matching for:
  match      :: (a -> b)                 -- Head Normal Forms
             -> b                        -- Failures
             -> (ID -> a -> a -> b)      -- Choices
             -> (ID -> a -> a -> b)      -- Free Variables
             -> ([Constraint] -> a -> b) -- Constraints
             -> a
             -> b

  match = error "match: not implemented yet"


narrow :: NonDet a => ID -> a -> a -> a
narrow id = choiceCons $! narrowID id


narrows :: NonDet a => ID -> [a] -> a
narrows id = choicesCons $! narrowID id


-- Apply a function to the head normal form
d_dollar_bang :: (NonDet a, NonDet b) => (a -> b) -> a -> b
d_dollar_bang f x = hnf (try x)
  where
   hnf (Val v)        = f v -- d_apply f v
   hnf Fail           = failCons
   hnf (Choice i a b) = choiceCons i (hnf (try a)) (hnf (try b))
   hnf (Free i a b)   = f (choiceCons i a b)
   hnf (Choices i xs) = choicesCons i (map (hnf . try) xs)
   hnf (Frees i xs)   = f (choicesCons i xs)
   hnf (Guard c e)    = guardCons c (hnf (try e))


-- Apply a non-deterministic function to the head normal form
nd_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> b
nd_dollar_bang f x s = hnf (try x)
  where
   hnf (Val v)        = nd_apply f v s
   hnf Fail           = failCons
   -- TODO Do we have to use leftSupply and rightSupply?
   hnf (Choice i a b) = choiceCons i (hnf (try a)) (hnf (try b))
   hnf (Free i a b)   = nd_apply f (choiceCons i a b) s
   hnf (Choices i xs) = choicesCons i (map (hnf . try) xs)
   hnf (Frees i xs)   = nd_apply f (choicesCons i xs) s
   hnf (Guard c e)    = guardCons c (hnf (try e))


-- TODO: test implementation for $! replace if more efficient
d_dollar_bang_test :: (NonDet a, NonDet b) => (a -> b) -> a -> b
d_dollar_bang_test f x = match f failCons choiceF freeF guardF x
  where
    choiceF i a b = choiceCons i (f `d_dollar_bang_test` a)
                                 (f `d_dollar_bang_test` b)
    freeF i a b   = f (choiceCons i a b)
    guardF c e    = guardCons c (f  `d_dollar_bang_test` e)

-- ---------------------------------------------------------------------------
-- Computations to normal form
-- ---------------------------------------------------------------------------

-- Class for data that supports normal form computations.
-- The normal form computation is combined with a continuation to be
-- applied to the normal form.
class NonDet a => NormalForm a where
  ($!!) :: NonDet b => (a -> b) -> a -> b
  ($!<) :: (a -> IO b) -> a -> IO b


-- Auxilary function to extend $!< for non-determinism
($$!<) :: (NormalForm a) => (a -> IO b) -> a -> IO b
cont $$!< x = nf (try x)
  where
    nf (Val v)        = cont $!< v
    nf Fail           = cont failCons
    nf (Choice i x y) = nfChoiceIO cont i x y
    nf (Free i x y)   = nfChoiceIO cont i x y
    nf (Choices i xs) = nfChoicesIO cont i xs
    nf (Frees i xs)   = nfChoicesIO cont i xs
    nf (Guard c e)    = cont (guardCons c e)


-- Auxilary Function to create a Choice and apply a continuation to
-- the normal forms of its alternatives
nfChoice :: (NormalForm a, NonDet b) => (a -> b) -> ID -> a -> a -> b
nfChoice cont i@(FreeID _) x1 x2 = cont (choiceCons i x1 x2)
nfChoice cont i x1 x2 = choiceCons i (cont $!! x1) (cont $!! x2)


nfChoices :: (NormalForm a, NonDet b) => (a -> b) -> ID -> [a] -> b
nfChoices cont i@(FreeID _) xs = cont (choicesCons i xs)
nfChoices cont i xs = choicesCons i (map (cont $!!) xs)


nfChoiceIO :: (NormalForm a, NonDet a) => (a -> IO b) -> ID -> a -> a -> IO b
{-nfChoiceIO cont i@(FreeID _) x1 x2 = lookupChoice i >>= choose where
  choose ChooseLeft  = cont x1 -- TODO: $!< x1
  choose ChooseRight = cont x2 -- $!< x2
  choose NoChoice    = cont (choiceCons i x1 x2)-}
nfChoiceIO cont i x1 x2 = cont $ choiceCons i x1 x2
-- nfChoiceIO cont i x1 x2 = do
-- --   x1' <- return $!< x1
-- --   x2' <- return $!< x2
--   cont (choiceCons i x1 x2)




nfChoicesIO :: (NormalForm a, NonDet a) => (a -> IO b) -> ID -> [a] -> IO b
nfChoicesIO cont i@(FreeID _) xs = lookupChoice i >>= choose where
  choose (LazyBind cs) = cont (guardCons cs (choicesCons i xs))
  choose (ChooseN c _) = cont $!< (xs !! c)
  choose NoChoice      = cont (choicesCons i xs)
nfChoicesIO cont i xs = cont (choicesCons i xs)
-- nfChoicesIO cont i xs = do
-- --   ys <- mapM (return $!<) xs
--   cont (choicesCons i xs)


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- Class for data that support generators
class NonDet a => Generable a where
  generate :: IDSupply -> a

-- ---------------------------------------------------------------------------
-- Unification
-- ---------------------------------------------------------------------------

-- Class for data that support unification
class (NonDet a, NormalForm a) => Unifiable a where
  -- unification on constructor terms, used for unification on general terms
  (=.=)    :: a -> a -> C_Success
  -- function pattern unification on constructor terms,
  -- used for function pattern unification on general terms
  (=.<=)   :: a -> a -> C_Success
  bind     :: ID -> a -> [Constraint]
  lazyBind :: ID -> a -> [Constraint]

-- unification
(=:=) :: Unifiable a => a -> a -> C_Success
x =:= y = unify (try x) (try y)
  where
    unify Fail _    = failCons
    unify _    Fail = failCons

    unify (Choice i x1 x2) y =
       choiceCons i (unify (try x1) y) (unify (try x2) y)

    unify x (Choice i x1 x2) =
       choiceCons i (unify x (try x1)) (unify x (try x2))

    unify (Choices i xs) y =
       choicesCons i $ map (\x -> unify (try x) y) xs

    unify x (Choices i ys) =
       choicesCons i $ map (\y -> unify x (try y)) ys

    unify (Guard c e) y = guardCons c (unify (try e) y)
    unify x (Guard c e) = guardCons c (unify x (try e))

    unify (Val vx) (Val vy) = vx =.= vy

    -- TODO: unify is too strict in this part, consider:
    -- x =:= [] &> x =:= repeat 1 where x free
    -- This example does not terminate because $!! requires the call
    -- repeat 1 to be evaluated to normal form.
    unify (Val v)      (Free j _ _) =
      (\ v' -> guardCons (bind j v') C_Success) $!! v

    unify (Val v)      (Frees j _) =
      (\ v' -> guardCons (bind j v') C_Success) $!! v

    unify (Free i _ _) (Val v)      =
      (\ v' -> guardCons (bind i v') C_Success) $!! v

    unify (Frees i _) (Val v)      =
      (\ v' -> guardCons (bind i v') C_Success) $!! v

    unify (Free i _ _) (Free j _ _)      =
      guardCons [i :=: BindTo j] C_Success

    unify (Frees i _) (Frees j _)      =
      guardCons [i :=: BindTo j] C_Success

-- function pattern unification
(=:<=) :: Unifiable a => a -> a -> C_Success
x =:<= y = unifyLazy (try x) y -- 1. Evaluate x to head normal form hx
  where
    -- special cases
    unifyLazy Fail             _ = failCons
    unifyLazy (Choice i x1 x2) y = choiceCons i (x1 =:<= y) (x2 =:<= y)
    unifyLazy (Choices i xs)   y = choicesCons i (map (=:<= y) xs)
    unifyLazy (Guard c e)      y = guardCons  c (e =:<= y)
    unifyLazy (Free _ _ _)     _ = error "Free occurred"
    -- 2. if hx is a free variable: bind it to y
    unifyLazy (Frees i _)      y = guardCons [i :=: LazyBind (lazyBind i y)] C_Success
    -- 3. if h1 is a constructor term, evaluate y to head normal form hy
    unifyLazy (Val x)          y = unify x (try y)

    unify _ Fail = failCons
    unify x (Choice j y1 y2) = choiceCons j (unify x (try y1)) (unify x (try y2))
    unify x (Choices j ys)   = choicesCons j (map (unify x . try) ys)
    unify x (Guard c e)      = guardCons c (unify x (try e))
    unify _ (Free _ _ _)     = error "Free occurred"
    -- 3b. if hy is a variable: instantiate hy to hx
    unify x (Frees j _)      = guardCons [j :=: LazyBind (lazyBind j x)] C_Success
    unify x (Val y)          = x =.<= y

-- ---------------------------------------------------------------------------
-- Conversion between Curry and Haskell data types
-- ---------------------------------------------------------------------------

class ConvertCurryHaskell ctype htype where -- needs MultiParamTypeClasses
  fromCurry :: ctype -> htype
  toCurry   :: htype -> ctype

-- ---------------------------------------------------------------------------
-- Built-in types
-- ---------------------------------------------------------------------------

-- The implementation of the Success type must be added here since it is used
-- in the class Unifiable.

-- BEGIN GENERATED FROM PrimTypes.curry
data C_Success
     = C_Success
     | Choice_C_Success ID C_Success C_Success
     | Choices_C_Success ID ([C_Success])
     | Fail_C_Success
     | Guard_C_Success ([Constraint]) C_Success

instance Show C_Success where
  showsPrec d (Choice_C_Success i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Success i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Success c e) = showsGuard d c e
  showsPrec d Fail_C_Success = showChar '!'
  showsPrec d C_Success = showString "Success"

instance Read C_Success where
  readsPrec d s = readParen False (\r -> [ (C_Success,r0) | (_,r0) <- readQualified "Prelude" "Success" r]) s

instance NonDet C_Success where
  choiceCons = Choice_C_Success
  choicesCons = Choices_C_Success
  failCons = Fail_C_Success
  guardCons = Guard_C_Success
  try (Choice_C_Success i x y) = tryChoice i x y
  try (Choices_C_Success i xs) = tryChoices i xs
  try Fail_C_Success = Fail
  try (Guard_C_Success c e) = Guard c e
  try x = Val x

instance Generable C_Success where
  generate s = Choices_C_Success (freeID s) [C_Success]

instance NormalForm C_Success where
  ($!!) cont C_Success = cont C_Success
  ($!!) cont (Choice_C_Success i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_Success i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_Success c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_Success = failCons
  ($!<) cont C_Success = cont C_Success
  ($!<) cont (Choice_C_Success i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Success i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x

instance Unifiable C_Success where
  (=.=) C_Success C_Success = C_Success
  (=.=) _ _ = Fail_C_Success
  (=.<=) C_Success C_Success = C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i C_Success = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_C_Success j _ _) = [(i :=: (BindTo j))]
  bind i (Choices_C_Success j _) = [(i :=: (BindTo j))]
  lazyBind i C_Success = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_C_Success j _ _) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Success j _) = [(i :=: (BindTo j))]
  lazyBind _ Fail_C_Success = [Failed]
  lazyBind i (Guard_C_Success cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]
-- END GENERATED FROM PrimTypes.curry

(&) :: C_Success -> C_Success -> C_Success
x & y = const y $!! x

-- Higher Order Funcs

-- BEGIN GENERATED FROM PrimTypes.curry
data Func t0 t1
     = Func (t0 -> IDSupply -> t1)
     | Choice_Func ID (Func t0 t1) (Func t0 t1)
     | Choices_Func ID ([Func t0 t1])
     | Fail_Func
     | Guard_Func ([Constraint]) (Func t0 t1)

instance Show (Func a b) where show = error "show for Func"

instance Read (Func a b) where readsPrec = error "readsPrec for Func"

instance NonDet (Func t0 t1) where
  choiceCons = Choice_Func
  choicesCons = Choices_Func
  failCons = Fail_Func
  guardCons = Guard_Func
  try (Choice_Func i x y) = tryChoice i x y
  try (Choices_Func i xs) = tryChoices i xs
  try Fail_Func = Fail
  try (Guard_Func c e) = Guard c e
  try x = Val x

instance Generable (Func a b) where generate _ = error "generate for Func"

instance (NormalForm t0,NormalForm t1) => NormalForm (Func t0 t1) where
  ($!!) cont f@(Func _) = cont f
  ($!!) cont (Choice_Func i x y) = nfChoice cont i x y
  ($!!) cont (Choices_Func i xs) = nfChoices cont i xs
  ($!!) cont (Guard_Func c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_Func = failCons
  ($!<) cont (Choice_Func i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_Func i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x

instance (Unifiable t0,Unifiable t1) => Unifiable (Func t0 t1) where
  (=.=) _ _ = Fail_C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i (Choice_Func j _ _) = [(i :=: (BindTo j))]
  bind i (Choices_Func j _) = [(i :=: (BindTo j))]
  lazyBind i (Choice_Func j _ _) = [(i :=: (BindTo j))]
  lazyBind i (Choices_Func j _) = [(i :=: (BindTo j))]
  lazyBind _ Fail_Func = [Failed]
  lazyBind i (Guard_Func cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]
-- END GENERATED FROM PrimTypes.curry


-- Higher Order functions
instance Show (a -> b) where
  show = error "show for function is undefined"

instance Read (a -> b) where
  readsPrec = error "read for function is undefined"

instance NonDet (a -> b) where
  choiceCons = undefined
  choicesCons = undefined
  failCons = undefined
  guardCons = undefined
  try = undefined

instance Generable (a -> b) where
  generate = error "generate for function is undefined"

instance NormalForm (a -> b) where
  cont $!! f = cont f
  cont $!< f = cont f

instance Unifiable (a -> b) where
  (=.=) = error "(=.=) for function is undefined"
  (=.<=) = error "(=.<=) for function is undefined"
  bind = error "bind for function is undefined"
  lazyBind = error "lazyBind for function is undefined"

-- ---------------------------------------------------------------------------
-- IO
-- ---------------------------------------------------------------------------

-- TODO: reason about IO and non-determinism

-- BEGIN GENERATED FROM PrimTypes.curry
data C_IO t0
     = C_IO (IO t0)
     | Choice_C_IO ID (C_IO t0) (C_IO t0)
     | Choices_C_IO ID ([C_IO t0])
     | Fail_C_IO
     | Guard_C_IO ([Constraint]) (C_IO t0)

instance Show (C_IO a) where show = error "show for C_IO"

instance Read (C_IO a) where readsPrec = error "readsPrec for C_IO"

instance NonDet (C_IO t0) where
  choiceCons = Choice_C_IO
  choicesCons = Choices_C_IO
  failCons = Fail_C_IO
  guardCons = Guard_C_IO
  try (Choice_C_IO i x y) = tryChoice i x y
  try (Choices_C_IO i xs) = tryChoices i xs
  try Fail_C_IO = Fail
  try (Guard_C_IO c e) = Guard c e
  try x = Val x

instance Generable (C_IO a) where generate _ = error "generate for C_IO"

instance (NormalForm t0,Show t0) => NormalForm (C_IO t0) where
  ($!!) cont io@(C_IO _) = cont io
  ($!!) cont (Choice_C_IO i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_IO i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_IO c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_IO = failCons
  ($!<) cont (Choice_C_IO i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_IO i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x

instance (Unifiable t0,Show t0) => Unifiable (C_IO t0) where
  (=.=) _ _ = Fail_C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i (Choice_C_IO j _ _) = [(i :=: (BindTo j))]
  bind i (Choices_C_IO j _) = [(i :=: (BindTo j))]
  lazyBind i (Choice_C_IO j _ _) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_IO j _) = [(i :=: (BindTo j))]
  lazyBind _ Fail_C_IO = [Failed]
  lazyBind i (Guard_C_IO cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]
-- END GENERATED FROM PrimTypes.curry


toIO :: C_IO a -> IO a
toIO (C_IO io) = io
toIO (Choice_C_IO _ _ _) = error "toIO: Choice_C_IO"
toIO (Guard_C_IO _ _) = error "toIO: Guard_C_IO"
toIO Fail_C_IO = error "toIO: Fail_C_IO"

fromIO :: IO a -> C_IO a
fromIO io = C_IO io

-- Use a Haskell IO action to implement a Curry IO action:
fromHaskellIO0 :: (ConvertCurryHaskell ca ha) => IO ha -> C_IO ca
fromHaskellIO0 hact = fromIO (hact >>= return . toCurry)

fromHaskellIO1 :: (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb) =>
                  (ha -> IO hb) -> ca -> C_IO cb
fromHaskellIO1 hact ca = fromIO (hact (fromCurry ca) >>= return . toCurry)

fromHaskellIO2 :: (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb,
                   ConvertCurryHaskell cc hc) =>
                  (ha -> hb -> IO hc) -> ca -> cb -> C_IO cc
fromHaskellIO2 hact ca cb =
  fromIO (hact (fromCurry ca) (fromCurry cb) >>= return . toCurry)

fromHaskellIO3 :: (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb,
                   ConvertCurryHaskell cc hc, ConvertCurryHaskell cd hd) =>
                  (ha -> hb -> hc -> IO hd) -> ca -> cb -> cc -> C_IO cd
fromHaskellIO3 hact ca cb cc =
 fromIO (hact (fromCurry ca) (fromCurry cb) (fromCurry cc) >>= return . toCurry)

-----------------------------------------------------------------------------
-- Our own implemenation of file handles (put here since used in various
-- libraries)
-----------------------------------------------------------------------------

-- since the operation IOExts.connectToCmd uses one handle for reading and
-- writing, we implement handles either as a single handle or two handles:
data CurryHandle = OneHandle Handle | InOutHandle Handle Handle

inputHandle :: CurryHandle -> Handle
inputHandle (OneHandle h)     = h
inputHandle (InOutHandle h _) = h

outputHandle :: CurryHandle -> Handle
outputHandle (OneHandle h)     = h
outputHandle (InOutHandle _ h) = h

-- ---------------------------------------------------------------------------
-- Primitive data that is built-in (e.g., Handle, IORefs,...)
-- ---------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data PrimData t0
     = PrimData t0
     | Choice_PrimData ID (PrimData t0) (PrimData t0)
     | Choices_PrimData ID ([PrimData t0])
     | Fail_PrimData
     | Guard_PrimData ([Constraint]) (PrimData t0)

instance Show (PrimData a) where show = error "show for PrimData"

instance Read (PrimData a) where readsPrec = error "readsPrec for PrimData"

instance NonDet (PrimData t0) where
  choiceCons = Choice_PrimData
  choicesCons = Choices_PrimData
  failCons = Fail_PrimData
  guardCons = Guard_PrimData
  try (Choice_PrimData i x y) = tryChoice i x y
  try (Choices_PrimData i xs) = tryChoices i xs
  try Fail_PrimData = Fail
  try (Guard_PrimData c e) = Guard c e
  try x = Val x

instance Generable (PrimData a) where generate _ = error "generate for PrimData"

instance NormalForm (PrimData a) where
  ($!!) cont p@(PrimData _) = cont p
  ($!!) cont (Choice_PrimData i x y) = nfChoice cont i x y
  ($!!) cont (Choices_PrimData i xs) = nfChoices cont i xs
  ($!!) cont (Guard_PrimData c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_PrimData = failCons
  ($!<) cont (Choice_PrimData i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_PrimData i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x

instance Unifiable (PrimData a) where
  (=.=) _ _ = Fail_C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i (Choice_PrimData j _ _) = [(i :=: (BindTo j))]
  bind i (Choices_PrimData j _) = [(i :=: (BindTo j))]
  lazyBind i (Choice_PrimData j _ _) = [(i :=: (BindTo j))]
  lazyBind i (Choices_PrimData j _) = [(i :=: (BindTo j))]
  lazyBind _ Fail_PrimData = [Failed]
  lazyBind i (Guard_PrimData cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]
-- END GENERATED FROM PrimTypes.curry

instance ConvertCurryHaskell (PrimData a) a where -- needs FlexibleInstances
  fromCurry (PrimData a) = a
  fromCurry _            = error "PrimData with no ground term occurred"

  toCurry a = PrimData a


-- ---------------------------------------------------------------------------
-- Auxiliaries for Show and Read
-- ---------------------------------------------------------------------------

showsChoice :: Show a => Int -> ID -> a -> a -> ShowS
showsChoice d i@(FreeID _) _ _ = shows i
showsChoice d r x1 x2 =
  showChar '(' .
  showsPrec d x1 .
  showString " ?" . shows r . showChar ' ' .
  showsPrec d x2 .
  showChar ')'

showsChoices :: Show a => Int -> ID -> [a] -> ShowS
showsChoices d i@(FreeID _) _ = shows i
showsChoices d r xs =
  showString "[?" . shows r .
  foldr (.) id (zipWith (\n x -> showString ", " . shows n . showString "->" . showsPrec d x) [0 ..] xs) .
  showChar ']'

showsGuard :: (Show a, Show b) => Int -> a -> b -> ShowS
showsGuard d c e = showsPrec d c . showString " &> " . showsPrec d e

-- Reads a possibly qualified name
readQualified :: String -> String -> ReadS ()
readQualified mod name r =
 let lexname = lex r in
     [((),s)  | (name',s)  <- lexname, name' == name]
  ++ [((),s3) | (mod',s1)  <- lexname
              , mod' == mod
              , (".",s2)   <- lex s1
              , (name',s3) <- lex s2
              , name' == name]

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

eval :: Show a => (IDSupply -> a) -> IO ()
eval goal = initSupply >>= print . goal

evalD :: Show a => a -> IO ()
evalD goal = print goal

evalIO :: Show a => (IDSupply -> C_IO a) -> IO ()
evalIO goal = initSupply >>= \s -> toIO (goal s) >>= print

evalDIO :: Show a => C_IO a -> IO ()
evalDIO goal = toIO goal >>= print

d_apply :: (a -> b) -> a -> b
d_apply f a = f a

-- TODO: Support non-deterministic Funcs
nd_apply :: NonDet b => Func a b -> a -> IDSupply -> b
nd_apply fun a s = (\(Func f) -> f a s) `d_dollar_bang` fun

----------------------------------------------------------------------
-- Printing all results of a computation in a depth-first manner
----------------------------------------------------------------------

-- Evaluate a nondeterministic expression and show all results
-- in depth-first order
prdfs :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
prdfs mainexp = initSupply >>= \s -> printValsDFS False (try (id $!! (mainexp s)))

printValsDFS :: (Show a,NonDet a, NormalForm a) => Bool -> Try a -> IO ()
printValsDFS _  Fail           = return ()
printValsDFS _  (Val v)        = print v
printValsDFS _ (Free _ _ _)    = error "free occurred"
-- printValsDFS fb (Free i x y)   = lookupChoice i >>= choose
--   where
--    choose ChooseLeft  = (printValsDFS fb . try) $!< x
--    choose ChooseRight = (printValsDFS fb . try) $!< y
--    choose NoChoice    = print i
printValsDFS fb (Frees i xs)   = lookupChoice i >>= choose
  where
   choose (LazyBind cs) = printValsDFS fb (Guard cs (choicesCons i xs))
   choose (ChooseN c _) = (printValsDFS fb . try) $!< (xs !! c)
   choose NoChoice      = print i

printValsDFS fb (Choice i x y) = lookupChoice i >>= choose
  where
    choose ChooseLeft  = (printValsDFS fb . try) $!< x
    choose ChooseRight = (printValsDFS fb . try) $!< y
    choose NoChoice    = if fb
                         then do
                           newChoice True ChooseLeft  x
                           newChoice True ChooseRight y
                           setChoice i NoChoice
                         else do
                           newChoice True ChooseLeft  x
                           newChoice False ChooseRight y

--     choose ChooseLeft  = (printValsDFS fb . try) $!< x
--     choose ChooseRight = (printValsDFS fb . try) $!< y
--     choose NoChoice    = do
--       -- Resetting the Choice is only necessary if we perform further
--       -- backtracking (indicated by flag fb). The reset action is computed
--       -- early to prevent space leaks for ID i.
--       let !maybeReset = if fb then setChoice i NoChoice else return ()
--       newChoice True ChooseLeft  x
--       newChoice fb   ChooseRight y
--       maybeReset

    newChoice fbt j a = do
      setChoice i j
      (printValsDFS fbt . try) $!< a

printValsDFS fb (Choices i xs) = lookupChoice i >>= choose
  where
    choose (LazyBind cs) = printValsDFS fb (Guard cs (choicesCons i xs))
    choose (ChooseN c _) = (printValsDFS fb . try) $!< (xs !! c)
    choose NoChoice      =
      if fb
        then do
          foldr1 (>>) $ zipWith (newChoice True) [0 ..] xs
          setChoice i NoChoice
        else foldr1 (>>) $ zipWithButLast (newChoice True) (newChoice False) [0 ..] xs

    choose c             = error $ "choose with " ++ show c ++ " for ID " ++ show i

    newChoice fbt n a = do
      setChoice i (ChooseN n errChoice)
      (printValsDFS fbt . try) $!< a

    errChoice = error "propagation number used within non-free Choice"

    zipWithButLast :: (a -> b -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
    zipWithButLast _ _     []     _      = []
    zipWithButLast _ _      _     []     = []
    zipWithButLast _ lastf (a:[]) (b:_ ) = lastf a b : []
    zipWithButLast _ lastf (a:_ ) (b:[]) = lastf a b : []
    zipWithButLast f lastf (a:as) (b:bs) = f a b : zipWithButLast f lastf as bs

printValsDFS fb (Guard cs e) = do
  mreset <- solves cs
  case mreset of
    Nothing    -> return ()
    Just reset -> if fb then (printValsDFS fb . try) $!< e >> reset
                        else (printValsDFS fb . try) $!< e

 -- Nothing -> Constraint is unsolvable
 -- Just reset -> Constraint has been solved
type Solved = IO (Maybe (IO ()))

solved :: Solved
solved = return (Just (return ()))

unsolvable :: Solved
unsolvable = return Nothing

mkSolved :: IO (IO ()) -> Solved
mkSolved mkReset = mkReset >>= return . Just 

(>>>) :: Solved -> Solved -> Solved
a >>> b = do
  mra <- a
  case mra of
    Nothing -> return Nothing
    Just ra -> do
      mrb <- b
      case mrb of
        Nothing -> ra >> return Nothing
        Just rb -> return (Just (ra >> rb))

solves :: [Constraint] -> Solved
solves [] = solved
solves (c:cs) = do
--   putStrLn $ "solving " ++ show c
  solve c >>> solves cs
--   mreset <- solve c
--   case mreset of
--     Nothing    -> return Nothing
--     Just reset -> do
--       mreset' <- solves cs
--       case mreset' of
--         Nothing -> reset >> return Nothing
--         Just reset' -> return (Just (reset >> reset'))

solve :: Constraint -> Solved
solve Failed = unsolvable
solve (i :=: cc) = lookupChoice i >>= choose cc
  where
    -- 1.: the Choice which should be stored for i
    -- 2.: the Choice for i in the store
    choose (LazyBind cs) NoChoice      = mkSolved (setUnsetChoice i cc)
    choose _             (LazyBind cs) = mkSolved (setUnsetChoice i NoChoice) >>> solves cs >>> solve (i :=: cc)
    choose (LazyBind cs) _             = solves cs
{-    choose (LazyBind cs) (LazyBind cs2) = solves cs >>> solves cs2
    choose (LazyBind cs) (ChooseN _ _)  = solves cs
    choose (ChooseN _ _) (LazyBind cs) = (setUnsetChoice i NoChoice >>> solves cs) >>> solve (i :=: cc)-}
    choose (BindTo j) ci       = lookupChoice j >>= check j ci
    choose c          NoChoice = mkSolved (setUnsetChoice i c)
    choose c          x | c==x = solved
    choose c          ci       = unsolvable

    -- 1.: the ID j to which i should be bound
    -- 2.: the Choice for i in the store
    -- 3.: the Choice for j in the store
--     check j (LazyBind cs) (LazyBind cs2) = solves cs >>> solves cs2 >>> solve (i :=: cc) -- cc = BindTo j
--     check j (LazyBind cs) (ChooseN _ _)  = solves cs >>> solve (i :=: cc) -- cc = BindTo j
--     check j c@(ChooseN _ _) (LazyBind cs) = (setUnsetChoice j NoChoice >>> solves cs) >>> solve (j :=: c)

    check j NoChoice NoChoice = mkSolved (setUnsetChoice i (BindTo j))

    check _ NoChoice y        = mkSolved (setUnsetChoice i y)
    check j x        NoChoice = mkSolved (setUnsetChoice j x)

    check _ x        y | x==y = solved

    check _ _ _               = unsolvable


----------------------------------------------------------------------
-- Data structures and operations to collect and show results
-- w.r.t. various search strategies
----------------------------------------------------------------------

-- Monadic lists as a general representation of values obtained
-- in a mondic manner. The additional constructor Abort
-- represents an incomplete list due to reaching the depth-bound in
-- iterative deepening. The constructor (WithReset lis act) represents
-- a list lis where the monadic action act has to be performed at the
-- end of the list.
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

-- Aborts a monadic list due to reaching the search depth (in iter. deepening)
abort :: Monad m => m (MList m a)
abort = return Abort

-- Concatenate two monadic lists
(+++) :: Monad m => m (MList m a) -> m (MList m a) -> m (MList m a)
get +++ getYs = withReset get (return ())
 where
  withReset getList reset = do
   l <- getList
   case l of
     WithReset getList' reset' -> withReset getList' (reset >> reset')
     MNil -> reset >> getYs -- perform action before going to next list
     Abort -> reset >> abortEnd getYs
     MCons x getXs -> mcons x (withReset getXs reset) -- move action down to end

  abortEnd getYs = do -- move Abort down to end of second list
   ys <- getYs
   case ys of
     WithReset getYs' reset' -> abortEnd getYs' |< reset'
     MNil -> return Abort -- replace end of second list by Abort
     Abort  -> return Abort
     MCons z getZs -> mcons z (abortEnd getZs)

-- Add a monadic action of with result type () to the end of a monadic list.
-- Used to reset a choice made via a dfs strategy.
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

-- Count and print the number of elements of a IO monad list:
countVals :: IOList a -> IO ()
countVals x = putStr "Number of Solutions: " >> count 0 x >>= print
  where
    count i MNil = return i
    count i (WithReset l _) = l >>= count i
    count i (MCons _ cont) = do
          let !i' = i+1
          cont >>= count i'

-- Print the first value of a IO monad list:
printOneValue :: Show a => IOList a -> IO ()
printOneValue MNil              = putStrLn "No solution"
printOneValue (MCons x getRest) = print x
printOneValue (WithReset l _) = l >>= printOneValue

-- Print all values of a IO monad list:
printAllValues :: Show a => IOList a -> IO ()
printAllValues MNil              = putStrLn "No more solutions"
printAllValues (MCons x getRest) = print x >> getRest >>= printAllValues
printAllValues (WithReset l _) = l >>= printAllValues

askKey = do
  putStr "Hit any key to terminate..."
  hFlush stdout
  hSetBuffering stdin NoBuffering
  getChar
  return ()

-- Print all values of a IO monad list on request by the user:
printValsOnDemand :: Show a => IOList a -> IO ()
printValsOnDemand = printValsInteractive True

printValsInteractive st MNil = putStrLn "No more solutions" >> askKey
printValsInteractive st (MCons x getRest) = print x >> askUser st getRest
printValsInteractive st (WithReset l _) = l >>= printValsInteractive st

-- ask the user for more values
askUser :: Show a => Bool -> IO (IOList a) -> IO ()
askUser st getrest = if not st then getrest >>= printValsInteractive st else do
  putStr "More solutions? [y(es)/n(o)/A(ll)] "
  hFlush stdout
  hSetBuffering stdin NoBuffering
  c <- getChar
  if c== '\n' then return () else putChar '\n'
  case c of
    'y'  -> getrest >>= printValsInteractive st
    'n'  -> return ()
    'a'  -> getrest >>= printValsInteractive False
    '\n' -> getrest >>= printValsInteractive False
    _    -> askUser st getrest

----------------------------------------------------------------------
-- Depth-first search into a monadic list
----------------------------------------------------------------------

-- Print all values of an expression in a depth-first manner:
printDFS :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printDFS mainexp = computeWithDFS mainexp >>= printAllValues

-- Print one value of an expression in a depth-first manner:
printDFS1 :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printDFS1 mainexp = computeWithDFS mainexp >>= printOneValue

-- Print all values on demand of an expression in a depth-first manner:
printDFSi :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printDFSi mainexp = computeWithDFS mainexp >>= printValsOnDemand

-- Compute all values of a non-deterministic goal in a depth-first manner:
computeWithDFS :: (NormalForm a, Show a) => (IDSupply -> a) -> IO (IOList a)
computeWithDFS mainexp =
  initSupply >>= \s -> searchDFS (try (id $!! (mainexp s)))

searchDFS :: (Show a, NormalForm a) => Try a -> IO (IOList a)
searchDFS Fail             = mnil
searchDFS (Val v)          = mcons v mnil
searchDFS (Free _ _ _)     = error "free occurred"
-- searchDFS (Free i x1 x2)   = lookupChoice i >>= choose
--   where
--     choose ChooseLeft  = (searchDFS . try) $!< x1
--     choose ChooseRight = (searchDFS . try) $!< x2
--     choose NoChoice    = mcons (choiceCons i x1 x2) mnil
searchDFS (Frees i xs)     = lookupChoice i >>= choose
  where
    choose (ChooseN c _) = (searchDFS . try) $!< (xs !! c)
    choose NoChoice      = mcons (choicesCons i xs) mnil


searchDFS (Choice i x1 x2) = lookupChoice i >>= choose
  where
    choose ChooseLeft  = (searchDFS . try) $!< x1
    choose ChooseRight = (searchDFS . try) $!< x2
    choose NoChoice    = newChoice ChooseLeft x1 +++ newChoice ChooseRight x2

    newChoice c x = do setChoice i c
                       (searchDFS .try) $!< x |< setChoice i NoChoice

searchDFS (Choices i xs) = lookupChoice i >>= choose
  where
    choose (ChooseN c _) = (searchDFS . try) $!< (xs !! c)
    choose NoChoice      = foldr1 (+++) $ zipWith newChoice [0 ..] xs
    choose x             = error ("choose: " ++ show x)

    newChoice n x = do setChoice i (ChooseN n errChoice)
                       (searchDFS .try) $!< x |< setChoice i NoChoice

    errChoice = error "propagation number used within non-free Choice"

searchDFS (Guard cs e) = do
  mreset <- solves cs
  case mreset of
    Nothing    -> mnil
    Just reset -> (searchDFS . try) $!< e |< reset

----------------------------------------------------------------------
-- Breadth-first search into a monadic list
----------------------------------------------------------------------

-- Print all values of a non-deterministic goal in a breadth-first manner:
printBFS :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printBFS mainexp = computeWithBFS mainexp >>= printAllValues

-- Print first value of a non-deterministic goal in a breadth-first manner:
printBFS1 :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printBFS1 mainexp = computeWithBFS mainexp >>= printOneValue

-- Print all values of a non-deterministic goal in a breadth-first manner:
printBFSi :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printBFSi mainexp = computeWithBFS mainexp >>= printValsOnDemand

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

    bfs xs ys set reset (Choices i cs) = set   >> lookupChoice i >>= choose

     where
        choose (ChooseN c _) = (bfs xs ys (return ()) reset . try) $!< (cs !! c)
        choose NoChoice    = do
          reset
          next xs ((zipWith newChoice [0..] cs) ++ ys)
        newChoice n x = (newSet n, newReset, x)
        newSet n = set   >> setChoice i (ChooseN n errChoice)
        newReset = reset >> setChoice i NoChoice
        errChoice = error "propagation number used within non-free Choice"

    bfs _ _ _ _(Free _ _ _) = error "bfs: asumption violated"
    bfs xs ys set reset (Frees i cs) = lookupChoice i >>= choose
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

----------------------------------------------------------------------
-- Iterative depth-first search into a monadic list
----------------------------------------------------------------------

-- A function to increase the depth for the iterative deepening strategy
-- (here: double the depth after reaching the depth bound)
incrDepth4IDFS n = n*2

-- Print all values of an expression with iterative deepening where
-- the first argument is the initial depth size which will be increased
-- by function incrDepth4IDFS in each iteration:
printIDS :: (NormalForm a, Show a) => Int -> (IDSupply -> a) -> IO ()
printIDS initdepth mainexp =
  computeWithIDS initdepth mainexp >>= printAllValues

-- Print one value of an expression with iterative deepening:
printIDS1 :: (NormalForm a, Show a) => Int -> (IDSupply -> a) -> IO ()
printIDS1 initdepth mainexp =
  computeWithIDS initdepth mainexp >>= printOneValue

-- Print all values on demand of an expression with iterative deepening:
printIDSi :: (NormalForm a, Show a) => Int -> (IDSupply -> a) -> IO ()
printIDSi initdepth mainexp =
  computeWithIDS initdepth mainexp >>= printValsOnDemand

-- Compute all values of a non-deterministic goal with a iterative
-- deepening strategy:
computeWithIDS :: (NormalForm a, Show a) => Int -> (IDSupply -> a)
                                         -> IO (IOList a)
computeWithIDS initdepth goal = initSupply >>= \s -> iter s 0 initdepth
 where
   iter s olddepth newdepth = startIDS (id $!! goal s) olddepth newdepth
                              ++++ iter s newdepth (incrDepth4IDFS newdepth)

-- Concatenate two monadic lists if the first ends with an Abort
(++++) :: Monad m => m (MList m a) -> m (MList m a) -> m (MList m a)
get ++++ getYs = withReset get (return ())
  where
    withReset getList reset = do
     l <- getList
     case l of
       WithReset getList' reset' -> withReset getList' (reset >> reset')
       MNil -> reset >> mnil
       Abort -> reset >> getYs -- ignore Abort when concatenating further vals
       MCons x getXs -> mcons x (withReset getXs reset)

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


----------------------------------------------------------------------
-- Parallel search by mapping search results into monadic structure
----------------------------------------------------------------------

-- Print all values of an expression in a parallel manner:
printPar :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printPar mainexp = computeWithPar mainexp >>= printAllValues

-- Print one value of an expression in a parallel manner:
printPar1 :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printPar1 mainexp = computeWithPar mainexp >>= printOneValue

-- Print all values on demand of an expression in a parallel manner:
printPari :: (NormalForm a, Show a) => (IDSupply -> a) -> IO ()
printPari mainexp = computeWithPar mainexp >>= printValsOnDemand

list2iolist :: [a] -> IO (IOList a)
list2iolist [] = mnil
list2iolist (x:xs) = mcons x (list2iolist xs)

-- Compute all values of a non-deterministic goal in a parallel manner:
computeWithPar :: NormalForm a => (IDSupply -> a) -> IO (IOList a)
computeWithPar mainexp = do
  s <- initSupply
  list2iolist
    (parSearch (searchMPlus Data.Map.empty (try (id $!! (mainexp s)))))

type SetOfChoices = Data.Map.Map Integer Choice

lookupChoice' :: SetOfChoices -> ID -> Choice
lookupChoice' set r =
  maybe NoChoice id (Data.Map.lookup (mkInt r) set)

setChoice' :: SetOfChoices -> ID -> Choice -> SetOfChoices
setChoice' set r c = Data.Map.insert (mkInt r) c set

-- Collect results of a non-deterministic computation in a monadic structure.
searchMPlus :: (NonDet a, MonadPlus m) => SetOfChoices -> Try a -> m a
searchMPlus _   Fail           = mzero
searchMPlus _   (Val v)        = return v
searchMPlus set (Choice i x y) = choose (lookupChoice' set i)
  where
    choose ChooseLeft  = searchMPlus set (try x)
    choose ChooseRight = searchMPlus set (try y)
    choose NoChoice    = searchMPlus (pick ChooseLeft)  (try x)
                 `mplus` searchMPlus (pick ChooseRight) (try y)

    pick c = setChoice' set i c

----------------------------------------------------------------------
