{-# LANGUAGE MagicHash, MultiParamTypeClasses, FlexibleInstances, Rank2Types #-}

module Basics
  ( module Basics
  , module ID
  ) where

import qualified Data.Map
import Control.Monad
import Control.Parallel.TreeSearch
import GHC.Exts (Int#, Char#, chr#)
import System.IO (Handle)

import ID
import MonadList
import Solver (Solution, SolutionTree (..), solves)
import Debug


nonAsciiChr :: Int# -> Char#
nonAsciiChr i = chr# i

-- TODO: Rename Frees to Free and Choices to Narrowed
-- METATODO: Reason about pros and cons of reusing Choices for free, narrowed, (?)

-- |Data type to wrap values in a generic structure
data Try a
  = Val a                 -- ^Value in head normal form (HNF)
  | Fail                  -- ^Fail
  | Choice ID a a         -- ^Binary choice, used for (?)
  | Choices ID [a]        -- ^N-ary choice for a narrowed variable
  | Frees ID [a]          -- ^N-ary choice for a free variable
  | Guard [Constraint] a  -- ^Constrained value
    deriving Show

tryChoice :: ID -> a -> a -> Try a
tryChoice i@(ID _) = Choice i
tryChoice _        = error "Basics.tryChoice: no ID"

tryChoices :: ID -> [a] -> Try a
tryChoices (ID _)         = error "Basics.tryChoices: ID"
tryChoices i@(FreeID _)   = Frees   i
tryChoices i@(Narrowed _) = Choices i

-- ---------------------------------------------------------------------------
-- Non-determinism
-- ---------------------------------------------------------------------------

-- |Class for data that supports nondeterministic values
class NonDet a where
  -- |Constructor for a binary choice, used for (?)
  choiceCons :: ID -> a -> a -> a
  -- |Constructor for a n-ary choice, used for free variables
  choicesCons:: ID -> [a] -> a
  -- |Constructor for a fail
  failCons   :: a
  -- |Constructor for a constrained value
  guardCons  :: [Constraint] -> a -> a
  -- |conversion of a value into its generic 'Try' structure
  try        :: a -> Try a

  -- Alternative approach: Replace generic Try structure with a matching
  -- function to gain more efficiency.

  -- |Matching with different function for the respective constructors
  match      :: (a -> b)                 -- ^Head Normal Form
             -> b                        -- ^Failure
             -> (ID -> a -> a -> b)      -- ^binary Choice
             -> (ID -> [a] -> b)         -- ^n-ary Choice for narrowed variable
             -> (ID -> [a] -> b)         -- ^n-ary Choice for free variable
             -> ([Constraint] -> a -> b) -- ^Constrained value
             -> a                        -- ^value to apply the function to
             -> b
  match = error "match not implemented"


narrow :: NonDet a => ID -> a -> a -> a
narrow i@(ID _) = choiceCons i
narrow _        = error "Basics.narrow: no ID"


-- |Convert a n-ary choice of a free variable into one with a narrowed variable
narrows :: NonDet a => ID -> [a] -> a
narrows i = choicesCons $! narrowID i


-- Apply a function to the head normal form
d_dollar_bang :: (NonDet a, NonDet b) => (a -> b) -> a -> b
d_dollar_bang f x = hnf (try x)
  where
   hnf (Val v)        = f v -- inlined d_apply f v
   hnf Fail           = failCons
   hnf (Choice i a b) = choiceCons i (hnf (try a)) (hnf (try b))
   hnf (Choices i xs) = choicesCons i (map (hnf . try) xs)
   hnf (Frees i xs)   = f (choicesCons i xs)
   hnf (Guard c e)    = guardCons c (hnf (try e))


-- Apply a non-deterministic function to the head normal form
nd_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> b
nd_dollar_bang f x s = hnf (try x)
  where
   hnf (Val v)        = nd_apply f v s
   hnf Fail           = failCons
   -- TODO Do we better use leftSupply and rightSupply?
   hnf (Choice i a b) = choiceCons i (hnf (try a)) (hnf (try b))
   hnf (Choices i xs) = choicesCons i (map (hnf . try) xs)
   hnf (Frees i xs)   = nd_apply f (choicesCons i xs) s
   hnf (Guard c e)    = guardCons c (hnf (try e))


-- TODO: test implementation for $! replace if more efficient
-- d_dollar_bang_test :: (NonDet a, NonDet b) => (a -> b) -> a -> b
-- d_dollar_bang_test f x = match f failCons choiceF freeF guardF x
--   where
--     choiceF i a b = choiceCons i (f `d_dollar_bang_test` a)
--                                  (f `d_dollar_bang_test` b)
--     freeF i a b   = f (choiceCons i a b)
--     guardF c e    = guardCons c (f  `d_dollar_bang_test` e)

-- ---------------------------------------------------------------------------
-- Computations to normal form
-- ---------------------------------------------------------------------------

-- Class for data that supports the computaton of its normal form.
-- The normal form computation is combined with a continuation to be
-- applied to the normal form.
class (Show a, NonDet a) => NormalForm a where
  -- |Apply a continuation to the normal form
  ($!!) :: NonDet b => (a -> b) -> a -> b
  -- |Apply a continuation to the ground normal form
  ($##) :: NonDet b => (a -> b) -> a -> b
  -- |TODO: We are not perfectly sure what this does (or at least should do)
  ($!<) :: (a -> IO b) -> a -> IO b
  -- new approach
  searchNF :: (forall b . NormalForm b => (b -> IO c) -> b -> IO c) -> (a -> IO c) -> a -> IO c
  searchNF = error "searchNF not implemented"


-- Auxiliaries for $!!

-- Auxiliary function to create a binary Choice and apply a continuation to
-- the normal forms of its two alternatives
nfChoice :: (NormalForm a, NonDet b) => (a -> b) -> ID -> a -> a -> b
nfChoice cont i@(ID _) x1 x2 = choiceCons i (cont $!! x1) (cont $!! x2)
nfChoice _ _ _ _ = error "Basics.nfChoice: no ID"
-- nfChoice cont i@(FreeID _) x1 x2 = cont (choiceCons i x1 x2)

-- Auxiliary function to create a n-ary Choice and apply a continuation to
-- the normal forms of its alternatives
nfChoices :: (NormalForm a, NonDet b) => (a -> b) -> ID -> [a] -> b
nfChoices _      (ID _)       _  = error "Basics.nfChoices: ID"
nfChoices cont i@(FreeID _)   xs = cont (choicesCons i xs)
nfChoices cont i@(Narrowed _) xs = choicesCons i (map (cont $!!) xs)


-- Auxiliaries for $##

gnfChoice :: (NormalForm a, NonDet b) => (a -> b) -> ID -> a -> a -> b
gnfChoice cont i@(ID _) x1 x2 = choiceCons i (cont $## x1) (cont $## x2)
gnfChoice _ _ _ _ = error "Basics.gnfChoice: no ID"

gnfChoices :: (NormalForm a, NonDet b) => (a -> b) -> ID -> [a] -> b
gnfChoices cont i xs = narrows i (map (cont $##) xs)


-- Auxiliaries for $!<

nfChoiceIO :: (NormalForm a, NonDet a) => (a -> IO b) -> ID -> a -> a -> IO b
nfChoiceIO cont i@(ID _) x1 x2 = cont $ choiceCons i x1 x2
nfChoiceIO _ _ _ _ = error "Basics.nfChoiceIO: no ID"
-- nfChoiceIO cont i@(ID _) x1 x2 = do
--   x1' <- return $!< x1
--   x2' <- return $!< x2
--   cont (choiceCons i x1' x2')


nfChoicesIO :: (NormalForm a, NonDet a) => (a -> IO b) -> ID -> [a] -> IO b
nfChoicesIO _      (ID _)     _  = error "Basics.nfChoicesIO: ID"
nfChoicesIO cont i@(FreeID _) xs = lookupChoiceID i >>= choose
  where
  choose (ChooseN c _, _) = cont $!< (xs !! c)
  choose (LazyBind cs, _) = do
    setChoice i NoChoice
    cont (guardCons cs (choicesCons i xs))
  choose (NoChoice   , _) = cont (choicesCons i xs) -- TODO replace i with j?
  choose c                = error $ "Basics.nfChoicesIO.choose: " ++ show c
nfChoicesIO cont i@(Narrowed _) xs = cont (choicesCons i xs)
-- nfChoicesIO cont i xs = do
-- --   ys <- mapM (return $!<) xs
--   cont (choicesCons i xs)


-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- Class for data that supports generators
class NonDet a => Generable a where
  generate :: IDSupply -> a

-- ---------------------------------------------------------------------------
-- Unification
-- ---------------------------------------------------------------------------

-- Class for data that supports unification
class NormalForm a => Unifiable a where
  -- |Unification on constructor terms, used for unification on general terms
  (=.=)    :: a -> a -> C_Success
  -- |Lazy unification on constructor terms, used for function pattern
  --  unification on general terms
  (=.<=)   :: a -> a -> C_Success
  -- |Binding of a free variable to a value
  bind     :: ID -> a -> [Constraint]
  -- |Lazy binding of a free variable to a value
  lazyBind :: ID -> a -> [Constraint]

-- Unification on terms
(=:=) :: Unifiable a => a -> a -> C_Success
x =:= y = unify (try x) (try y) -- 1. Compute the head normal forms hx, hy
  where
  -- failure
  unify Fail _    = failCons
  unify _    Fail = failCons

  -- binary choice
  unify (Choice i x1 x2) hy = choiceCons i (unify (try x1) hy) (unify (try x2) hy)
  unify hx (Choice i y1 y2) = choiceCons i (unify hx (try y1)) (unify hx (try y2))

  -- n-ary choice
  unify (Choices i xs) hy = choicesCons i (map (\x' -> unify (try x') hy) xs)
  unify hx (Choices i ys) = choicesCons i (map (\y' -> unify hx (try y')) ys)

  -- constrained value
  unify (Guard c e) hy = guardCons c (unify (try e) hy)
  unify hx (Guard c e) = guardCons c (unify hx (try e))

  -- constructor-rooted terms
  unify (Val vx) (Val vy) = vx =.= vy

  -- free variables
  unify (Frees i _) (Frees j _) = guardCons [i :=: BindTo j] C_Success

  -- bind a variable to a constructor-rooted term
  unify (Frees i _) (Val v) = guardCons (bind i v) C_Success
  unify (Val v) (Frees j _) = guardCons (bind j v) C_Success
  -- TODO2: Occurs check?

  -- unify is too strict in this part, consider:
  -- x =:= [] &> x =:= repeat 1 where x free
  -- This example does not terminate because $!! requires the call
  -- repeat 1 to be evaluated to normal form first.
  -- unify (Frees i _) (Val v) = (\v' -> guardCons (bind i v') C_Success) $!! v
  -- unify (Val v) (Frees j _) = (\v' -> guardCons (bind j v') C_Success) $!! v

-- Lazy unification for function patterns
(=:<=) :: Unifiable a => a -> a -> C_Success
x =:<= y = unifyLazy (try x) -- 1. Compute the head normal form hx
  where
  -- failure
  unifyLazy Fail             = failCons
  -- binary choice
  unifyLazy (Choice i x1 x2) = choiceCons i (x1 =:<= y) (x2 =:<= y)
  -- n-ary choice
  unifyLazy (Choices i xs)   = choicesCons i (map (=:<= y) xs)
  -- constrained value
  unifyLazy (Guard c e)      = guardCons c (e =:<= y)
  -- constructor-rooted term
  unifyLazy (Val vx)         = unify vx (try y)
  -- free variable
  unifyLazy (Frees i _)      = guardCons [i :=: LazyBind (lazyBind i y)] C_Success

  unify _  Fail             = failCons
  unify vx (Choice j y1 y2) = choiceCons j (unify vx (try y1)) (unify vx (try y2))
  unify vx (Choices j ys)   = choicesCons j (map (unify vx . try) ys)
  unify vx (Guard c e)      = guardCons c (unify vx (try e))
  unify vx (Val vy)          = vx =.<= vy
  unify vx (Frees j _)      = guardCons [j :=: LazyBind (lazyBind j vx)] C_Success

-- ---------------------------------------------------------------------------
-- Conversion between Curry and Haskell data types
-- ---------------------------------------------------------------------------

class ConvertCurryHaskell ctype htype where -- needs MultiParamTypeClasses
  fromCurry :: ctype -> htype
  toCurry   :: htype -> ctype

------------------------------------------------------------------------------
-- Matching for Integers
------------------------------------------------------------------------------

-- TODO: use unboxed int

matchInteger :: NonDet a => [(Int,a)] -> C_Integer -> a
matchInteger rules (C_Neg nat)              = 
  matchNat (map (mapFst abs) $ filter ((<0).fst) rules) nat 
matchInteger rules C_Zero                   = maybe failCons id $ lookup 0 rules
matchInteger rules (C_Pos nat)              = matchNat (filter ((>0).fst) rules) nat
matchInteger rules (Choice_C_Integer i l r) = 
  narrow i (matchInteger rules l) (matchInteger rules r)
matchInteger rules (Choices_C_Integer i cs) = 
  narrows i $ map (matchInteger rules) cs
matchInteger rules Fail_C_Integer           = failCons
matchInteger rules (Guard_C_Integer cs int) = guardCons cs (matchInteger rules int)

matchNat []    _                    = failCons
matchNat rules C_IHi                = maybe failCons id $ lookup 1 rules
matchNat rules (C_O nat)            = matchNat (map halfKey $ filter (evenPos.fst) rules) nat
  where
   evenPos n = even n && (0 < n)
matchNat rules (C_I nat)            = matchNat (map halfKey $ filter (odd.fst) rules) nat
matchNat rules (Choice_C_Nat i l r) = narrow i (matchNat rules l) (matchNat rules r)
matchNat rules (Choices_C_Nat i cs) = narrows i $ map (matchNat rules) cs
matchNat rules (Guard_C_Nat cs nat) = guardCons cs $ matchNat rules nat


halfKey :: (Int,a) -> (Int,a)
halfKey =  mapFst (`div` 2)


mapFst :: (a -> b) -> (a,c) -> (b,c)
mapFst f (a,b) = (f a,b)    

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
  showsPrec _ Fail_C_Success = showChar '!'
  showsPrec _ C_Success = showString "Success"

instance Read C_Success where
  readsPrec _ s = readParen False (\r -> [ (C_Success,r0) | (_,r0) <- readQualified "Prelude" "Success" r]) s

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
  ($##) cont C_Success = cont C_Success
  ($##) cont (Choice_C_Success i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_Success i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_Success c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_Success = failCons
  ($!<) cont C_Success = cont C_Success
  ($!<) cont (Choice_C_Success i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Success i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont C_Success = cont C_Success

instance Unifiable C_Success where
  (=.=) C_Success C_Success = C_Success
  (=.=) _ _ = Fail_C_Success
  (=.<=) C_Success C_Success = C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i C_Success = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_C_Success j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Success j@(FreeID _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Success j@(Narrowed _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Success = [Unsolvable]
  bind i (Guard_C_Success cs e) = cs ++ (bind i e)
  lazyBind i C_Success = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_C_Success j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Success j@(FreeID _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Success j@(Narrowed _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Success = [Unsolvable]
  lazyBind i (Guard_C_Success cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]
-- END GENERATED FROM PrimTypes.curry

(&) :: C_Success -> C_Success -> C_Success
x & y = const y $!! x


-- Integer

data C_Integer
     = C_Neg C_Nat
     | C_Zero
     | C_Pos C_Nat
     | Choice_C_Integer ID C_Integer C_Integer
     | Choices_C_Integer ID ([C_Integer])
     | Fail_C_Integer
     | Guard_C_Integer ([Constraint]) C_Integer

instance Show C_Integer where
  showsPrec d (Choice_C_Integer i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Integer i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Integer c e) = showsGuard d c e
  showsPrec _ Fail_C_Integer = showChar '!'
  showsPrec d (C_Neg x1) = (showString "(Neg") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec d C_Zero = showString "Zero"
  showsPrec d (C_Pos x1) = (showString "(Pos") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Integer where
  readsPrec d s = (readParen (d > 10) (\r -> [ (C_Neg x1,r1) | (_,r0) <- readQualified "Prelude" "Neg" r, (x1,r1) <- readsPrec 11 r0]) s) ++ ((readParen False (\r -> [ (C_Zero,r0) | (_,r0) <- readQualified "Prelude" "Zero" r]) s) ++ (readParen (d > 10) (\r -> [ (C_Pos x1,r1) | (_,r0) <- readQualified "Prelude" "Pos" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_Integer where
  choiceCons = Choice_C_Integer
  choicesCons = Choices_C_Integer
  failCons = Fail_C_Integer
  guardCons = Guard_C_Integer
  try (Choice_C_Integer i x y) = tryChoice i x y
  try (Choices_C_Integer i xs) = tryChoices i xs
  try Fail_C_Integer = Fail
  try (Guard_C_Integer c e) = Guard c e
  try x = Val x


instance Generable C_Integer where
  generate s = Choices_C_Integer (freeID s) [(C_Neg (generate (leftSupply s))),C_Zero,(C_Pos (generate (leftSupply s)))]


instance NormalForm C_Integer where
  ($!!) cont (C_Neg x1) = (\y1 -> cont (C_Neg y1)) $!! x1
  ($!!) cont C_Zero = cont C_Zero
  ($!!) cont (C_Pos x1) = (\y1 -> cont (C_Pos y1)) $!! x1
  ($!!) cont (Choice_C_Integer i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_Integer i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_Integer c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_Integer = failCons
  ($##) cont (C_Neg x1) = (\y1 -> cont (C_Neg y1)) $## x1
  ($##) cont C_Zero = cont C_Zero
  ($##) cont (C_Pos x1) = (\y1 -> cont (C_Pos y1)) $## x1
  ($##) cont (Choice_C_Integer i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_Integer i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_Integer c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_Integer = failCons
  ($!<) cont (C_Neg x1) = (\y1 -> cont (C_Neg y1)) $!< x1
  ($!<) cont C_Zero = cont C_Zero
  ($!<) cont (C_Pos x1) = (\y1 -> cont (C_Pos y1)) $!< x1
  ($!<) cont (Choice_C_Integer i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Integer i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont (C_Neg x1) = search (\y1 -> cont (C_Neg y1)) x1
  searchNF search cont C_Zero = cont C_Zero
  searchNF search cont (C_Pos x1) = search (\y1 -> cont (C_Pos y1)) x1


instance Unifiable C_Integer where
  (=.=) (C_Neg x1) (C_Neg y1) = x1 =:= y1
  (=.=) C_Zero C_Zero = C_Success
  (=.=) (C_Pos x1) (C_Pos y1) = x1 =:= y1
  (=.=) _ _ = Fail_C_Success
  (=.<=) (C_Neg x1) (C_Neg y1) = x1 =:<= y1
  (=.<=) C_Zero C_Zero = C_Success
  (=.<=) (C_Pos x1) (C_Pos y1) = x1 =:<= y1
  (=.<=) _ _ = Fail_C_Success
  bind i (C_Neg x2) = ((i :=: (ChooseN 0 1)):(concat [(bind (leftID i) x2)]))
  bind i C_Zero = ((i :=: (ChooseN 1 0)):(concat []))
  bind i (C_Pos x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_Integer j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Integer j@(FreeID _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Integer j@(Narrowed _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Integer = [Unsolvable]
  bind i (Guard_C_Integer cs e) = cs ++ (bind i e)
  lazyBind i (C_Neg x2) = [(i :=: (ChooseN 0 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i C_Zero = [(i :=: (ChooseN 1 0))]
  lazyBind i (C_Pos x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_Integer j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Integer j@(FreeID _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Integer j@(Narrowed _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Integer = [Unsolvable]
  lazyBind i (Guard_C_Integer cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]




-- Nats

data C_Nat
     = C_IHi
     | C_O C_Nat
     | C_I C_Nat
     | Choice_C_Nat ID C_Nat C_Nat
     | Choices_C_Nat ID ([C_Nat])
     | Fail_C_Nat
     | Guard_C_Nat ([Constraint]) C_Nat

instance Show C_Nat where
  showsPrec d (Choice_C_Nat i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Nat i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Nat c e) = showsGuard d c e
  showsPrec _ Fail_C_Nat = showChar '!'
  showsPrec d C_IHi = showString "IHi"
  showsPrec d (C_O x1) = (showString "(O") . ((showChar ' ') . ((shows x1) . (showChar ')')))
  showsPrec d (C_I x1) = (showString "(I") . ((showChar ' ') . ((shows x1) . (showChar ')')))


instance Read C_Nat where
  readsPrec d s = (readParen False (\r -> [ (C_IHi,r0) | (_,r0) <- readQualified "Prelude" "IHi" r]) s) ++ ((readParen (d > 10) (\r -> [ (C_O x1,r1) | (_,r0) <- readQualified "Prelude" "O" r, (x1,r1) <- readsPrec 11 r0]) s) ++ (readParen (d > 10) (\r -> [ (C_I x1,r1) | (_,r0) <- readQualified "Prelude" "I" r, (x1,r1) <- readsPrec 11 r0]) s))


instance NonDet C_Nat where
  choiceCons = Choice_C_Nat
  choicesCons = Choices_C_Nat
  failCons = Fail_C_Nat
  guardCons = Guard_C_Nat
  try (Choice_C_Nat i x y) = tryChoice i x y
  try (Choices_C_Nat i xs) = tryChoices i xs
  try Fail_C_Nat = Fail
  try (Guard_C_Nat c e) = Guard c e
  try x = Val x


instance Generable C_Nat where
  generate s = Choices_C_Nat (freeID s) [C_IHi,(C_O (generate (leftSupply s))),(C_I (generate (leftSupply s)))]


instance NormalForm C_Nat where
  ($!!) cont C_IHi = cont C_IHi
  ($!!) cont (C_O x1) = (\y1 -> cont (C_O y1)) $!! x1
  ($!!) cont (C_I x1) = (\y1 -> cont (C_I y1)) $!! x1
  ($!!) cont (Choice_C_Nat i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_Nat i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_Nat c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_Nat = failCons
  ($##) cont C_IHi = cont C_IHi
  ($##) cont (C_O x1) = (\y1 -> cont (C_O y1)) $## x1
  ($##) cont (C_I x1) = (\y1 -> cont (C_I y1)) $## x1
  ($##) cont (Choice_C_Nat i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_Nat i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_Nat c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_Nat = failCons
  ($!<) cont C_IHi = cont C_IHi
  ($!<) cont (C_O x1) = (\y1 -> cont (C_O y1)) $!< x1
  ($!<) cont (C_I x1) = (\y1 -> cont (C_I y1)) $!< x1
  ($!<) cont (Choice_C_Nat i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Nat i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont C_IHi = cont C_IHi
  searchNF search cont (C_O x1) = search (\y1 -> cont (C_O y1)) x1
  searchNF search cont (C_I x1) = search (\y1 -> cont (C_I y1)) x1


instance Unifiable C_Nat where
  (=.=) C_IHi C_IHi = C_Success
  (=.=) (C_O x1) (C_O y1) = x1 =:= y1
  (=.=) (C_I x1) (C_I y1) = x1 =:= y1
  (=.=) _ _ = Fail_C_Success
  (=.<=) C_IHi C_IHi = C_Success
  (=.<=) (C_O x1) (C_O y1) = x1 =:<= y1
  (=.<=) (C_I x1) (C_I y1) = x1 =:<= y1
  (=.<=) _ _ = Fail_C_Success
  bind i C_IHi = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (C_O x2) = ((i :=: (ChooseN 1 1)):(concat [(bind (leftID i) x2)]))
  bind i (C_I x2) = ((i :=: (ChooseN 2 1)):(concat [(bind (leftID i) x2)]))
  bind i (Choice_C_Nat j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Nat j@(FreeID _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Nat j@(Narrowed _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Nat = [Unsolvable]
  bind i (Guard_C_Nat cs e) = cs ++ (bind i e)
  lazyBind i C_IHi = [(i :=: (ChooseN 0 0))]
  lazyBind i (C_O x2) = [(i :=: (ChooseN 1 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (C_I x2) = [(i :=: (ChooseN 2 1)),((leftID i) :=: (LazyBind (lazyBind (leftID i) x2)))]
  lazyBind i (Choice_C_Nat j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Nat j@(FreeID _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Nat j@(Narrowed _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Nat = [Unsolvable]
  lazyBind i (Guard_C_Nat cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]




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
  ($##) cont f@(Func _) = cont f
  ($##) cont (Choice_Func i x y) = gnfChoice cont i x y
  ($##) cont (Choices_Func i xs) = gnfChoices cont i xs
  ($##) cont (Guard_Func c x) = guardCons c (cont $## x)
  ($##) _ Fail_Func = failCons
  ($!<) cont (Choice_Func i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_Func i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont (Func x1) = search (\y1 -> cont (Func y1)) x1

instance (Unifiable t0,Unifiable t1) => Unifiable (Func t0 t1) where
  (=.=) _ _ = Fail_C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i (Choice_Func j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_Func j@(FreeID _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_Func j@(Narrowed _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_Func = [Unsolvable]
  bind i (Guard_Func cs e) = cs ++ (bind i e)
  lazyBind i (Choice_Func j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_Func j@(FreeID _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_Func j@(Narrowed _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_Func = [Unsolvable]
  lazyBind i (Guard_Func cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]
-- END GENERATED FROM PrimTypes.curry


-- Higher Order functions
instance Show (a -> b) where
  show = error "show for function is undefined"

instance Read (a -> b) where
  readsPrec = error "read for function is undefined"

instance NonDet (a -> b) where
  choiceCons  = undefined
  choicesCons = undefined
  failCons    = undefined
  guardCons   = undefined
  try         = undefined

instance Generable (a -> b) where
  generate = error "generate for function is undefined"

instance NormalForm (a -> b) where
  cont $!! f = cont f
  cont $## f = cont f
  cont $!< f = cont f
  searchNF _ cont f = cont f

instance Unifiable (a -> b) where
  (=.=)    = error "(=.=) for function is undefined"
  (=.<=)   = error "(=.<=) for function is undefined"
  bind     = error "bind for function is undefined"
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

instance (NormalForm t0) => NormalForm (C_IO t0) where
  ($!!) cont io@(C_IO _) = cont io
  ($!!) cont (Choice_C_IO i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_IO i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_IO c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_IO = failCons
  ($##) cont io@(C_IO _) = cont io
  ($##) cont (Choice_C_IO i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_IO i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_IO c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_IO = failCons
  ($!<) cont (Choice_C_IO i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_IO i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont io@(C_IO _) = cont io

instance Unifiable t0 => Unifiable (C_IO t0) where
  (=.=) _ _ = Fail_C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i (Choice_C_IO j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_IO j@(FreeID _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_IO j@(Narrowed _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_IO = [Unsolvable]
  bind i (Guard_C_IO cs e) = cs ++ (bind i e)
  lazyBind i (Choice_C_IO j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_IO j@(FreeID _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_IO j@(Narrowed _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_IO = [Unsolvable]
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
  ($##) cont p@(PrimData _) = cont p
  ($##) cont (Choice_PrimData i x y) = gnfChoice cont i x y
  ($##) cont (Choices_PrimData i xs) = gnfChoices cont i xs
  ($##) cont (Guard_PrimData c x) = guardCons c (cont $## x)
  ($##) _ Fail_PrimData = failCons
  ($!<) cont (Choice_PrimData i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_PrimData i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  -- no search inside argument of PrimData since it is primitive:
  searchNF search cont (PrimData x) = cont (PrimData x)

instance Unifiable (PrimData t0) where
  (=.=) _ _ = Fail_C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i (Choice_PrimData j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_PrimData j@(FreeID _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_PrimData j@(Narrowed _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_PrimData = [Unsolvable]
  bind i (Guard_PrimData cs e) = cs ++ (bind i e)
  lazyBind i (Choice_PrimData j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_PrimData j@(FreeID _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_PrimData j@(Narrowed _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_PrimData = [Unsolvable]
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
showsChoice d i@(ID _) x1 x2 =
  showChar '(' .
  showsPrec d x1 .
  showString " ?" . shows i . showChar ' ' .
  showsPrec d x2 .
  showChar ')'
showsChoice _ _ _ _ = error "showsChoice with no ID"

showsChoices :: Show a => Int -> ID -> [a] -> ShowS
showsChoices d i@(ID _) _        = error "showsChoices with ID"
showsChoices d i@(FreeID _) _    = shows i
showsChoices d i@(Narrowed _) xs =
  showString "[?" . shows i .
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
              , (".", s2)   <- lex s1
              , (name', s3) <- lex s2
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

d_apply :: (a -> b) -> a -> b
d_apply f a = f a

nd_apply :: NonDet b => Func a b -> a -> IDSupply -> b
nd_apply fun a s = (\(Func f) -> f a s) `d_dollar_bang` fun

-- ---------------------------------------------------------------------------
-- Simple evaluation
-- ---------------------------------------------------------------------------

eval :: Show a => (IDSupply -> a) -> IO ()
eval goal = initSupply >>= print . goal

evalD :: Show a => a -> IO ()
evalD goal = print goal

evalIO :: (NormalForm a, Show a) => (IDSupply -> C_IO a) -> IO ()
-- evalIO goal = initSupply >>= toIO . goal >>= print
evalIO goal = computeWithDFS goal >>= execIOList

evalDIO :: (NormalForm a, Show a) => C_IO a -> IO ()
evalDIO goal = toIO goal >> return ()

execIOList :: IOList (C_IO a) -> IO ()
execIOList MNil                 = return ()
execIOList (MCons xact getRest) = toIO xact >> getRest >>= execIOList
execIOList (WithReset l _)      = l >>= execIOList

-- ---------------------------------------------------------------------------
-- Printing all results of a computation in a depth-first manner
-- ---------------------------------------------------------------------------

-- Evaluate a nondeterministic expression (thus, requiring some IDSupply)
-- and print all results in depth-first order.
-- The first argument is the operation to print a result (e.g., Prelude.print).
prdfs :: (Show a, NormalForm a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
prdfs prt mainexp = do
  s <- initSupply
  printValsDFS False prt (id $!! (mainexp s))

printValsDFS :: (Show a, NormalForm a) => Bool -> (a -> IO ()) -> a -> IO ()
printValsDFS fb cont a = do
  trace $ "\n\n-- \n\nprintValsDFS " ++ take 200 (show a)
  printValsDFS' fb cont (try a)

printValsDFS' :: (Show a, NormalForm a) => Bool -> (a -> IO ()) -> Try a -> IO ()
printValsDFS' _  _    Fail           = return ()
printValsDFS' fb cont (Val v)        = searchNF (printValsDFS fb) cont v
printValsDFS' fb cont (Choice i x y) = lookupChoice i >>= choose
  where
    choose ChooseLeft  = printValsDFS fb cont x
    choose ChooseRight = printValsDFS fb cont y
    choose NoChoice    = -- doWithChoices_ fb i
--                            [ (ChooseLeft , printValsDFS True cont x)
--                            , (ChooseRight, printValsDFS fb   cont y)
--                            ]
--  TODO: reactivate the implementation above, when performant
      if fb
        then do
          newChoiceOpt True ChooseLeft  x
          newChoiceOpt True ChooseRight y
          setChoice i NoChoice
        else do
          newChoiceOpt True ChooseLeft   x
          newChoiceOpt False ChooseRight y
     where
      newChoice fbt c a = do
        reset <- setUnsetChoice i c
        printValsDFS fbt cont a
        reset

      newChoiceOpt fbt c a = do
        -- Assumption 1: Binary choices can only be set to one of
        -- [NoChoice, ChooseLeft, ChooseRight], therefore the reset action may
        -- be ignored in between
        -- Assumption 2: Furthermore, binary Choices can not be chained, so
        -- setChoiceRaw may be used
        setChoice i c
        printValsDFS fbt cont a
    choose c           = error $ "Basics.printValsDFS'.choose: " ++ show c




printValsDFS' fb cont (Frees i xs)   = lookupChoiceID i >>= choose
  where
    choose (LazyBind cs, _) = processLazyBind fb cs i xs (printValsDFS fb cont)
    choose (ChooseN c _, _) = printValsDFS fb cont (xs !! c)
    choose (NoChoice   , j) = cont $ choicesCons j xs

printValsDFS' fb cont (Choices i xs) = lookupChoiceID i >>= choose
  where
    choose (LazyBind cs, _) = processLazyBind fb cs i xs (printValsDFS fb cont)
    choose (ChooseN c _, _) = printValsDFS fb cont (xs !! c)
    choose (NoChoice   , j) =
--       doWithChoices_ fb i $ zipWithButLast mkChoice mkLastChoice [0 ..] xs

--     mkChoice n x = (ChooseN n (-1), printValsDFS True cont x)
--     mkLastChoice n x = (ChooseN n (-1), printValsDFS fb   cont x)
--  TODO: reactivate the implementation above, when performant
     if fb
       then do
         foldr1 (>>) $ zipWith (newChoice True) [0 ..] xs
         setChoice i NoChoice
       else foldr1 (>>) $ zipWithButLast (newChoice True) (newChoice False) [0 ..] xs
      where

       newChoice fbt n a = do
         setChoice i (ChooseN n (-1)) -- was errChoice
         printValsDFS fbt cont a

       errChoice = error "propagation number used within narrowed Choice"
    choose c           = error $ "Basics.printValsDFS'.choose: " ++ show c

printValsDFS' fb cont (Guard cs e) = solves cs >>= traverse fb
  where
    traverse _     FailST               = return ()
    traverse True  (SuccessST reset)    = printValsDFS True  cont e >> reset
    traverse False (SuccessST _    )    = printValsDFS False cont e
    traverse True  (ChoiceST reset l r) = do
      l >>= traverse True
      r >>= traverse True
      reset
    traverse False (ChoiceST _     l r) = do
      l >>= traverse True
      r >>= traverse False
    traverse True  (ChoicesST reset cs) = do
      mapM_ (>>= traverse True) cs
      reset
    traverse False (ChoicesST _     cs) = do
      mapM_ (>>= traverse True) cs -- TODO: do not reset at last alternative

--     traverse Nothing = return ()
--     traverse (Just reset) =  if fb then (printValsDFS fb . try) $!< e >> reset
--                                    else (printValsDFS fb . try) $!< e

processLazyBind :: NonDet a => Bool -> [Constraint] -> ID -> [a] -> (a -> IO ()) -> IO ()
processLazyBind True cs i xs search = do
  reset <- setUnsetChoice i NoChoice
  search $ guardCons cs $ choicesCons i xs
  reset
processLazyBind False cs i xs search = do
  setChoice i NoChoice
  search $ guardCons cs $ choicesCons i xs

zipWithButLast :: (a -> b -> c) -> (a -> b -> c) -> [a] -> [b] -> [c]
zipWithButLast _ _     []     _      = []
zipWithButLast _ _      _     []     = []
zipWithButLast _ lastf (a:[]) (b:_ ) = lastf a b : []
zipWithButLast _ lastf (a:_ ) (b:[]) = lastf a b : []
zipWithButLast f lastf (a:as) (b:bs) = f a b : zipWithButLast f lastf as bs



-- Attempt to gain more abstraction during search

-- doWithChoice :: Bool -> ID -> Choice -> IO a -> IO a
-- doWithChoice True i c act = do
--   reset <- setUnsetChoice i c
--   retVal <- act
--   reset
--   return retVal
-- doWithChoice False i c act = do
--   setChoice i c
--   act

-- doWithChoices :: Bool -> ID -> [(Choice, IO a)] -> IO [a]
-- doWithChoices _ _ []     = return []
-- doWithChoices True i ((c, act): cacts) = do
--   reset   <- setUnsetChoice i c
--   retVal  <- act
--   retVals <- mapM (uncurry (doWithChoice False i)) cacts
--   reset
--   return (retVal:retVals)
-- doWithChoices False i cacts = mapM1 (uncurry (doWithChoice False i)) cacts

-- doWithChoices_ :: Bool -> ID -> [(Choice, IO a)] -> IO ()
-- doWithChoices_ _ _ []     = return ()
-- doWithChoices_ True i ((c, act): cacts) = do
--   reset <- setUnsetChoice i c
--   _     <- act
--   mapM_ (uncurry (doWithChoice False i)) cacts
--   reset
-- doWithChoices_ False i cacts = mapM1_ (uncurry (doWithChoice False i)) cacts

-- ---------------------------------------------------------------------------
-- Depth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- Print all values of an expression in a depth-first manner.
-- The first argument is the operation to print a result (e.g., Prelude.print).
printDFS :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printDFS prt mainexp = computeWithDFS mainexp >>= printAllValues prt

-- Print one value of an expression in a depth-first manner:
printDFS1 :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printDFS1 prt mainexp = computeWithDFS mainexp >>= printOneValue prt

-- Print all values on demand of an expression in a depth-first manner:
printDFSi :: (NormalForm a, Show a) =>
             MoreDefault -> (a -> IO ()) -> (IDSupply -> a) -> IO ()
printDFSi ud prt mainexp = computeWithDFS mainexp >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal in a depth-first manner:
computeWithDFS :: (NormalForm a, Show a) => (IDSupply -> a) -> IO (IOList a)
computeWithDFS mainexp = initSupply >>=
  \s -> searchDFS (`mcons` mnil) (id $!! (mainexp s))

searchDFS :: (Show a, NormalForm a) => (a -> IO (IOList b)) -> a -> IO (IOList b)
searchDFS cont a = searchDFS' cont (try a)

searchDFS' :: (Show a, NormalForm a) => (a -> IO (IOList b)) -> Try a -> IO (IOList b)
searchDFS' cont Fail             = mnil
searchDFS' cont (Val v)          = searchNF searchDFS cont v
searchDFS' cont (Choice i x1 x2) = lookupChoice i >>= choose
  where
    choose ChooseLeft  = searchDFS cont x1
    choose ChooseRight = searchDFS cont x2
    choose NoChoice    = newChoice ChooseLeft x1 +++ newChoice ChooseRight x2

    newChoice c x = do
      reset <- setUnsetChoice i c
      searchDFS cont x |< reset

searchDFS' cont (Frees i xs) = lookupChoiceID i >>= choose
  where
    choose (LazyBind cs, _) = processLB cs
    choose (ChooseN c _, _) = searchDFS cont (xs !! c)
    choose (NoChoice   , j) = cont $ choicesCons j xs
    choose c                = error $ "Basics.searchDFS'.choose: " ++ show c

    processLB cs = do
      reset <- setUnsetChoice i NoChoice
      searchDFS cont (guardCons cs $ choicesCons i xs) |< reset

searchDFS' cont (Choices i xs) = lookupChoice i >>= choose
  where
    choose (LazyBind cs) = processLB cs
    choose (ChooseN c _) = searchDFS cont (xs !! c)
    choose NoChoice      = foldr1 (+++) $ zipWith newChoice [0 ..] xs
    choose c             = error $ "Basics.searchDFS'.choose: " ++ show c

    processLB cs = do
      reset <- setUnsetChoice i NoChoice
      searchDFS cont (guardCons cs $ choicesCons i xs) |< reset

    newChoice n x = do
      reset <- setUnsetChoice i (ChooseN n errChoice)
      searchDFS cont x |< reset

    errChoice = error "propagation number used for narrowed Choice"

searchDFS' cont (Guard cs e) = solves cs >>= traverse
  where
    traverse FailST               = mnil
    traverse (SuccessST reset)    = searchDFS cont e |< reset
    traverse (ChoiceST reset l r) =
      ((l >>= traverse) +++ (r >>= traverse)) |< reset
    traverse (ChoicesST reset cs) =
      foldr1 (+++) (map (>>= traverse) cs) |< reset

-- searchDFS (Guard cs e) = do
--   mreset <- solves cs
--   case mreset of
--     Nothing    -> mnil
--     Just reset -> (searchDFS . try) $!< e |< reset

-- ---------------------------------------------------------------------------
-- Breadth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- Print all values of a non-deterministic goal in a breadth-first manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printBFS :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printBFS prt mainexp = computeWithBFS mainexp >>= printAllValues prt

-- Print first value of a non-deterministic goal in a breadth-first manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printBFS1 :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printBFS1 prt mainexp = computeWithBFS mainexp >>= printOneValue prt

-- Print all values of a non-deterministic goal in a breadth-first manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printBFSi :: (NormalForm a, Show a) =>
             MoreDefault -> (a -> IO ()) -> (IDSupply -> a) -> IO ()
printBFSi ud prt mainexp = computeWithBFS mainexp >>= printValsOnDemand ud prt

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

-- ---------------------------------------------------------------------------
-- Iterative depth-first search into a monadic list
-- ---------------------------------------------------------------------------

-- A function to increase the depth for the iterative deepening strategy
-- (here: double the depth after reaching the depth bound)
incrDepth4IDFS n = n*2

-- Print all values of an expression with iterative deepening where
-- the first argument is the initial depth size which will be increased
-- by function incrDepth4IDFS in each iteration:
-- The second argument is the operation to print a result (e.g., Prelude.print).
printIDS :: (NormalForm a, Show a) => Int -> (a -> IO ())
         -> (IDSupply -> a) -> IO ()
printIDS initdepth prt mainexp =
  computeWithIDS initdepth mainexp >>= printAllValues prt

-- Print one value of an expression with iterative deepening:
printIDS1 :: (NormalForm a, Show a) => Int -> (a -> IO ())
          -> (IDSupply -> a) -> IO ()
printIDS1 initdepth prt mainexp =
  computeWithIDS initdepth mainexp >>= printOneValue prt

-- Print all values on demand of an expression with iterative deepening:
printIDSi :: (NormalForm a, Show a) => MoreDefault -> Int -> (a -> IO ())
          -> (IDSupply -> a) -> IO ()
printIDSi ud initdepth prt mainexp =
  computeWithIDS initdepth mainexp >>= printValsOnDemand ud prt

-- Compute all values of a non-deterministic goal with a iterative
-- deepening strategy:
computeWithIDS :: (NormalForm a, Show a) => Int -> (IDSupply -> a)
                                         -> IO (IOList a)
computeWithIDS initdepth goal = initSupply >>= \s -> iter s 0 initdepth
 where
   iter s olddepth newdepth = startIDS (id $!! goal s) olddepth newdepth
                              ++++ iter s newdepth (incrDepth4IDFS newdepth)

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

-- ---------------------------------------------------------------------------
-- Parallel search by mapping search results into monadic structure
-- ---------------------------------------------------------------------------

-- Print all values of an expression in a parallel manner:
-- The first argument is the operation to print a result (e.g., Prelude.print).
printPar :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printPar prt mainexp = computeWithPar mainexp >>= printAllValues prt

-- Print one value of an expression in a parallel manner:
printPar1 :: (NormalForm a, Show a) => (a -> IO ()) -> (IDSupply -> a) -> IO ()
printPar1 prt mainexp = computeWithPar mainexp >>= printOneValue prt

-- Print all values on demand of an expression in a parallel manner:
printPari :: (NormalForm a, Show a) =>
             MoreDefault -> (a -> IO ()) -> (IDSupply -> a) -> IO ()
printPari ud prt mainexp = computeWithPar mainexp >>= printValsOnDemand ud prt

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
-- Auxillary Functions
----------------------------------------------------------------------

-- Operation to print the result of the main goal with bindings of free
-- variables in the goal. Since the formatting is defined in the Curry
-- module lib/ShowBindings, we strip here only the surrounding quotes.
printWithBindings :: Show a => a -> IO ()
printWithBindings x = printWithoutLastChar (tail (show x))
 where printWithoutLastChar [] = putChar '\n'
       printWithoutLastChar [_] = putChar '\n'
       printWithoutLastChar (c:cs) = putChar c >> printWithoutLastChar cs

-- mapM1 :: Monad m => (a -> m b) -> [a] -> m [b]
-- mapM1 f as = sequence1 (map f as)

-- mapM1_ :: Monad m => (a -> m b) -> [a] -> m ()
-- mapM1_ f as = sequence1_ (map f as)

-- sequence1 :: Monad m => [m a] -> m [a]
-- sequence1 [act] = act >>= return . (:[])
-- sequence1 (a:as) = a >>= \a' -> sequence1 as >>= return .(a':)


-- sequence1_ :: Monad m => [m a] -> m ()
-- sequence1_ [act] = act >> return ()
-- sequence1_ (a:as) = a >> sequence1_ as