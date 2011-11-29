{-# LANGUAGE MultiParamTypeClasses, Rank2Types, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Types where

import ID
import qualified Data.Map as Map
import Data.Map (Map)
import Data.IORef
import Unsafe.Coerce(unsafeCoerce)
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Constraint Store
-- ---------------------------------------------------------------------------

type ConstStore = Map Integer Value

data Value = forall a . V a

unV :: Value -> a
unV (V x) = unsafeCoerce x

emptyCs :: ConstStore
emptyCs = Map.empty

-- combines a constraint and a constraint sote
combConstr :: Constraints -> ConstStore -> ConstStore
#ifdef DISABLE_CS
combConstr = const
#else
combConstr (StructConstr _)  cs = cs
combConstr (ValConstr i v _) cs = Map.insert (getKey i) (V v) cs
#endif
-- combines two constraint stores
combConstStores :: ConstStore -> ConstStore -> ConstStore
#ifdef DISABLE_CS
combConstStores = const
#else
combConstStores = Map.union
#endif

-- lookupCs looks for a velue bound in a constraint store
-- if the value is found the given function is applied,
-- if not, the default value is returned
lookupCs :: ConstStore -> ID -> (a -> b) -> b -> b
#ifdef DISABLE_CS
lookupCs _ _ _ def = def
#else
lookupCs cs i f def = maybe def (f . unV) (Map.lookup (getKey i) cs)
#endif

-- ---------------------------------------------------------------------------
-- Global Constraint Store
------------------------------------------------------------------------------
#ifndef DISABLE_CS
globalCs :: IORef ConstStore
globalCs = unsafePerformIO $ newIORef emptyCs
#endif

-- adds a Constraint to the global constraint store
addToGlobalCs :: Constraints -> IO ()
#ifdef DISABLE_CS
addToGlobalCs = return ()
#else
addToGlobalCs (StructConstr _) = return ()
addToGlobalCs cs@(ValConstr i x _ ) = do
  gcs <- readIORef globalCs
  let gcs' = combConstr cs gcs
  writeIORef globalCs $! gcs'
#endif

lookupGlobalCs :: IO ConstStore
#ifdef DISABLE_CS
lookupGlobalCs = return emptyCs
#else
lookupGlobalCs = readIORef globalCs
#endif
-- ---------------------------------------------------------------------------
-- Try structure
-- ---------------------------------------------------------------------------

-- TODO: Reason about pros and cons of reusing Choices for free, narrowed, (?)

-- |Data type to represent non-deterministic values in a generic structure.
--
-- The 'Try' structure is used to provide a type-independent representation
-- of non-deterministic values in the runtime system, e.g. for implementing
-- the search strategies.
data Try a
  = Val a               -- ^ Value in head normal form (HNF)
  | Fail                -- ^ Failure
  | Choice ID a a       -- ^ Binary choice, introduced by the (?) operator
  | Narrowed ID [a]     -- ^ N-ary choice for narrowed variable
  | Free ID [a]         -- ^ N-ary choice for free variable, where
                        --   N corresponds to the number of constructors of
                        --   the underlying type
  | Guard Constraints a -- ^ Constrained value
    deriving Show

-- |Convert a binary choice of type a into one of type 'Try' a
tryChoice :: ID -> a -> a -> Try a
tryChoice i@(ChoiceID _) = Choice i
tryChoice _              = error "Basics.tryChoice: no ChoiceID"

-- |Convert a n-ary choice of type a into one of type 'Try' a
tryChoices :: ID -> [a] -> Try a
tryChoices (ChoiceID       _) = error "Basics.tryChoices: ChoiceID"
tryChoices i@(FreeID     _ _) = Free     i
tryChoices i@(NarrowedID _ _) = Narrowed i

-- ---------------------------------------------------------------------------
-- Non-determinism
-- ---------------------------------------------------------------------------

-- |Class for types that support nondeterministic values
class NonDet a where
  -- |Construct a binary choice, used by the (?) operator
  choiceCons :: ID -> a -> a -> a
  -- |Construct a n-ary choice, used for free variables and narrowing
  choicesCons:: ID -> [a] -> a
  -- |Construct a failed computation
  failCons   :: a
  -- |Construct a constrained value
  guardCons  :: Constraints -> a -> a
  -- |Convert a value into the generic 'Try' structure
  try        :: a -> Try a
  -- |Apply the adequate function to a non-deterministic value, where each of
  --  the supplied functions handles a different constructor.
  --
  -- /Note:/ This functionality was introduced to render the conversion from
  -- and to the 'Try' structure obsolete. Nonetheless, the performance impact
  -- still is to be analyzed.
  match      :: (ID -> a -> a -> b)     -- ^ Binary Choice
             -> (ID -> [a] -> b)        -- ^ n-ary Choice for narrowed variable
             -> (ID -> [a] -> b)        -- ^ n-ary Choice for free variable
             -> b                       -- ^ Failure
             -> (Constraints -> a -> b) -- ^ Constrained value
             -> (a -> b)                -- ^ Head Normal Form
             -> a                       -- ^ value to apply the functions to
             -> b


narrow :: NonDet a => ID -> a -> a -> a
narrow i@(ChoiceID _) = choiceCons i
narrow _              = error "Basics.narrow: no ChoiceID"

-- |Convert an n-ary choice of a free variable into one with a narrowed variable
-- |If the varible is bound in either the local or the global constraint store
-- |the value found in the store is used
narrows :: NonDet b => ConstStore -> ID -> (a -> b) -> [a] -> b
narrows cs i@(FreeID p s) f xs = 
  lookupCs cs i f (lookupCs gcs i f
                            (choicesCons (NarrowedID p s) (map f xs)))
 where
  gcs = unsafePerformIO lookupGlobalCs
narrows cs i@(NarrowedID _ _) f xs = choicesCons i (map f xs)
narrows _ (ChoiceID _) _ _ = error "Types.narrows: ChoiceID"
-- ---------------------------------------------------------------------------
-- Computation of normal forms
-- ---------------------------------------------------------------------------

-- |Class for types that support the computaton of its normal form (NF) and
--  ground normal form (GNF).
--
-- While NF allows free variables, GNF does not, therefore free variables will
-- be narrowed when computing the GNF.
--
-- The NF/GNF computation is combined with a continuation to be applied to
-- the NF/GNF.
class (NonDet a, Show a) => NormalForm a where
  -- |Apply a continuation to the normal form
  ($!!) :: NonDet b => (a -> ConstStore -> b) -> a -> ConstStore -> b
  -- |Apply a continuation to the ground normal form
  ($##) :: NonDet b => (a -> ConstStore -> b) -> a -> ConstStore -> b
  -- |TODO: We are not perfectly sure what this does (or at least should do)
  ($!<) :: (a -> IO b) -> a -> IO b
  -- new approach
  searchNF :: (forall b . NormalForm b => (b -> c) -> b -> c) -> (a -> c) -> a -> c

-- |Auxiliary function to apply the continuation to the normal forms of the
-- two alternatives of a binary choice.
nfChoice :: (NormalForm a, NonDet b) => (a -> ConstStore -> b) -> ID -> a -> a -> ConstStore -> b
nfChoice cont i@(ChoiceID _) x1 x2 cs = choiceCons i ((cont $!! x1) cs) ((cont $!! x2) cs)
nfChoice _ _ _ _ _ = error "Basics.nfChoice: no ChoiceID"
-- nfChoice cont i@(FreeID _) x1 x2 = cont (choiceCons i x1 x2)

-- |Auxiliary function to apply the continuation to the normal forms of the
-- n alternatives of a n-ary choice.
nfChoices :: (NormalForm a, NonDet b) => (a -> ConstStore -> b) -> ID -> [a] -> ConstStore -> b
nfChoices _      (ChoiceID _)     _  _ = error "Basics.nfChoices: ChoiceID"
nfChoices cont i@(FreeID _ _)     xs cs = cont (choicesCons i xs) cs
nfChoices cont i@(NarrowedID _ _) xs cs = choicesCons i (map (\x -> (cont $!! x) cs) xs)

-- |Auxiliary function to apply the continuation to the ground normal forms of
-- the two alternatives of a binary choice.
gnfChoice :: (NormalForm a, NonDet b) => (a -> ConstStore -> b) -> ID -> a -> a -> ConstStore -> b
gnfChoice cont i@(ChoiceID _) x1 x2 cs = choiceCons i ((cont $## x1) cs) ((cont $## x2) cs)
gnfChoice _ _ _ _ _ = error "Basics.gnfChoice: no ChoiceID"

-- |Auxiliary function to apply the continuation to the ground normal forms of
-- the n alternatives of a n-ary choice.
gnfChoices :: (NormalForm a, NonDet b) => (a -> ConstStore -> b) -> ID -> [a] -> ConstStore -> b
gnfChoices cont i xs cs = narrows cs  i (\x -> (cont $## x) cs) xs

nfChoiceIO :: (NormalForm a) => (a -> IO b) -> ID -> a -> a -> IO b
nfChoiceIO cont i@(ChoiceID _) x1 x2 = cont $ choiceCons i x1 x2
nfChoiceIO _ _ _ _ = error "Basics.nfChoiceIO: no ChoiceID"
-- nfChoiceIO cont i@(ID _) x1 x2 = do
--   x1' <- return $!< x1
--   x2' <- return $!< x2
--   cont (choiceCons i x1' x2')

nfChoicesIO :: (NormalForm a) => (a -> IO b) -> ID -> [a] -> IO b
nfChoicesIO _      (ChoiceID _)     _  = error "Basics.nfChoicesIO: ChoiceID"
nfChoicesIO cont i@(FreeID _ _) xs = lookupChoiceID i >>= choose
  where
  choose (ChooseN c _, _) = cont $!< (xs !! c)
  choose (LazyBind cs, _) = do
    setChoice i NoChoice
    cont (guardCons (StructConstr cs) (choicesCons i xs))
  choose (NoChoice   , _) = cont (choicesCons i xs) -- TODO replace i with j?
  choose c                = error $ "Basics.nfChoicesIO.choose: " ++ show c
nfChoicesIO cont i@(NarrowedID  _ _) xs = cont (choicesCons i xs)
-- nfChoicesIO cont i xs = do
-- --   ys <- mapM (return $!<) xs
--   cont (choicesCons i xs)

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

-- |Class for types that support generator functions to represent free
-- variables.
class NonDet a => Generable a where
  -- |Generate a free variable for the given 'IDSupply'
  generate :: IDSupply -> a

-- ---------------------------------------------------------------------------
-- Unification
-- ---------------------------------------------------------------------------

-- Class for data that supports unification
class NormalForm a => Unifiable a where
  -- |Unification on constructor-rooted terms
  (=.=)    :: a -> a -> ConstStore -> C_Success
  -- |Lazy unification on constructor-rooted terms,
  --  used for functional patterns
  (=.<=)   :: a -> a -> ConstStore -> C_Success
  -- |Bind a free variable to a value
  bind     :: ID -> a -> [Constraint]
  -- |Lazily bind a free variable to a value
  lazyBind :: ID -> a -> [Constraint]

-- |Unification on general terms
(=:=) :: Unifiable a => a -> a -> ConstStore -> C_Success
(=:=) x y cs = match uniChoice uniNarrowed uniFree failCons uniGuard uniVal x
  where
  uniChoice i x1 x2 = checkFail (choiceCons i ((x1 =:= y) cs) ((x2 =:= y) cs)) y
  uniNarrowed i xs  = checkFail (choicesCons i (map (\x' -> (x' =:= y) cs) xs)) y
  uniFree i _       = lookupCs cs i (\xval -> (xval =:= y) cs) (bindTo cs y) -- TODO: use global cs
    where
    bindTo cs' = match bindChoice bindNarrowed bindFree failCons bindGuard bindVal
      where
      bindChoice j y1 y2 = choiceCons j  (bindTo cs' y1) (bindTo cs' y2)
      bindNarrowed j ys  = choicesCons j (map (bindTo cs') ys)
      bindFree j _       = lookupCs cs j (bindTo cs') (guardCons (ValConstr i y [i :=: BindTo j]) C_Success)
      bindGuard c      = guardCons c . (bindTo $! combConstr c cs')
#ifdef STRICT_VAL_BIND
      bindVal v          = ((\v' _ -> guardCons (ValConstr i v' (bind i v')) C_Success) $!! v) cs'
#else
      bindVal v          = guardCons (ValConstr i v (bind i v)) C_Success
#endif
      
  uniGuard c e      = checkFail (guardCons c ((e =:= y) $! combConstr c cs)) y
  uniVal v          = uniWith cs y
    where
    uniWith cs' = match univChoice univNarrowed univFree failCons univGuard univVal
      where
      univChoice j y1 y2   = choiceCons j (uniWith cs' y1) (uniWith cs' y2)
      univNarrowed j ys    = choicesCons j (map (uniWith cs') ys)
#ifdef STRICT_VAL_BIND
      univFree j _         = lookupCs cs j (uniWith cs') (guardCons (ValConstr j v (bind j v)) C_Success) -- TODO: use global cs
#else
      univFree j _         = lookupCs cs j (uniWith cs') (((\v' _ -> (guardCons (ValConstr j v' (bind j v')) C_Success)) $!! v) cs)
#endif
      univGuard c        = guardCons c . (uniWith $! combConstr c cs')
      univVal w            = (v =.= w) cs'
  checkFail e = match (\_ _ _ -> e) const2 const2 failCons const2 (const e)
    where const2 _ _ = e

  -- TODO2: Occurs check?

-- Lazy unification on general terms, used for function patterns
(=:<=) :: Unifiable a => a -> a -> ConstStore -> C_Success
(=:<=) x y cs = match uniChoice uniNarrowed uniFree failCons uniGuard uniVal x
  where
  -- binary choice
  uniChoice i x1 x2 = choiceCons i ((x1 =:<= y) cs) ((x2 =:<= y) cs)
  -- n-ary choice
  uniNarrowed i xs  = choicesCons i (map (\z -> (z =:<= y) cs) xs)
  -- constrained value
  uniGuard c e      = guardCons c ((e =:<= y) $! combConstr c cs)
  -- free variable
  uniFree i _       = lookupCs cs i (\xVal -> (xVal =:<= y) cs) (guardCons (StructConstr [i :=: LazyBind (lazyBind i y)]) C_Success)
  -- constructor-rooted term
  uniVal vx = unifyWith cs y
    where
    unifyWith cs' = match uniyChoice uniyNarrowed uniyFree failCons uniyGuard uniyVal
      where
      uniyChoice j y1 y2  = choiceCons j (unifyWith cs' y1) (unifyWith cs' y2)
      uniyNarrowed j ys   = choicesCons j (map (unifyWith cs') ys)
      uniyGuard c         = guardCons c . (unifyWith $! combConstr c cs')
      uniyVal vy          = (vx =.<= vy) cs'
      uniyFree j _        = lookupCs cs' j (unifyWith cs') (guardCons (StructConstr [j :=: LazyBind (lazyBind j vx)]) C_Success)

-- ---------------------------------------------------------------------------
-- Conversion between Curry and Haskell data types
-- ---------------------------------------------------------------------------

class ConvertCurryHaskell ctype htype where -- needs MultiParamTypeClasses
  fromCurry :: ctype -> htype
  toCurry   :: htype -> ctype

-- ---------------------------------------------------------------------------
-- Auxiliaries for Show and Read
-- ---------------------------------------------------------------------------

showsChoice :: Show a => Int -> ID -> a -> a -> ShowS
showsChoice d i@(ChoiceID _) x1 x2 =
  showString "(?" . shows i . showChar ' ' .
  showsPrec d x1 . showChar ' ' .
  showsPrec d x2 .
  showChar ')'
showsChoice _ _ _ _ = error "showsChoice with no ChoiceID"
{-
showsChoice d i@(ID _) x1 x2 =
  showChar '(' .
  showsPrec d x1 .
  showString " ?" . shows i . showChar ' ' .
  showsPrec d x2 .
  showChar ')'
showsChoice _ _ _ _ = error "showsChoice with no ID"
-}

showsChoices :: Show a => Int -> ID -> [a] -> ShowS
showsChoices _ (ChoiceID _)   _  = error "showsChoices with ChoiceID"
showsChoices _ i@(FreeID _ _) _  = shows i
showsChoices d i@(NarrowedID _ _) xs
  = showString "[?" . shows i
  . foldr (.) id (zipWith showNarrowing [(0 :: Int) ..] xs)
  . showChar ']'
  where showNarrowing n x = showString ", " . shows n
                          . showString "->" . showsPrec d x

showsGuard :: (Show a, Show b) => Int -> a -> b -> ShowS
showsGuard d c e = showsPrec d c . showString " &> " . showsPrec d e

-- Reads a possibly qualified name
readQualified :: String -> String -> ReadS ()
readQualified mdl name r =
 let lexname = lex r in
     [((),s)  | (name',s)  <- lexname, name' == name]
  ++ [((),s3) | (mod',s1)  <- lexname
              , mod' == mdl
              , (".", s2)   <- lex s1
              , (name', s3) <- lex s2
              , name' == name]

-- ---------------------------------------------------------------------------
-- Success type
-- ---------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data C_Success
     = C_Success
     | Choice_C_Success ID C_Success C_Success
     | Choices_C_Success ID ([C_Success])
     | Fail_C_Success
     | Guard_C_Success Constraints C_Success

instance Show C_Success where
  showsPrec d (Choice_C_Success i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Success i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Success cs e) = showsGuard d cs e
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
  try (Guard_C_Success cs e) = Guard cs e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Success i x y) = f i x y
  match _ f _ _ _ _ (Choices_C_Success i@(NarrowedID _ _) xs) = f i xs
  match _ _ f _ _ _ (Choices_C_Success i@(FreeID _ _) xs) = f i xs
  match _ _ _ _ _ _ (Choices_C_Success i@(ChoiceID _) _) = error ("Prelude.Success.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ Fail_C_Success = f
  match _ _ _ _ f _ (Guard_C_Success cs e) = f cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Success where
  generate s = Choices_C_Success (freeID [0] s) [C_Success]

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
  searchNF _ cont C_Success = cont C_Success
  searchNF _ _ x = error ("Prelude.Success.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Success where
  (=.=) C_Success C_Success _ = C_Success
  (=.=) _ _ _ = Fail_C_Success
  (=.<=) C_Success C_Success _ = C_Success
  (=.<=) _ _ _ = Fail_C_Success
  bind i C_Success = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_C_Success j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Success j@(FreeID _ _) _) = [(i :=: (BindTo j))]
  bind i (Choices_C_Success j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ (Choices_C_Success i@(ChoiceID _) _) = error ("Prelude.Success.bind: Choices with ChoiceID: " ++ (show i))
  bind _ Fail_C_Success = [Unsolvable]
  bind i (Guard_C_Success cs e) = (getConstrList cs) ++ (bind i e)
  lazyBind i C_Success = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_C_Success j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Success j@(FreeID _ _) _) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Success j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Success i@(ChoiceID _) _) = error ("Prelude.Success.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ Fail_C_Success = [Unsolvable]
  lazyBind i (Guard_C_Success cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind i e)))]
-- END GENERATED FROM PrimTypes.curry

-- Higher Order functions
instance Show (a -> b) where
  show = error "show for function is undefined"

instance Read (a -> b) where
  readsPrec = error "read for function is undefined"

instance NonDet b => NonDet (a -> b) where
  choiceCons    i f g = \ x -> choiceCons i (f x) (g x)
  choicesCons    i fs = \ x -> choicesCons i (map ($x) fs)
  failCons            = \ _ -> failCons
  guardCons       c f = \ x -> guardCons c (f x)
  try                 = Val
  match _ _ _ _ _ f x = f x

instance NonDet b => Generable (a -> b) where
  generate = error "generate for function is undefined"

instance NonDet b => NormalForm (a -> b) where
  cont $!! f = cont f
  cont $## f = cont f
  cont $!< f = cont f
  searchNF _ cont f = cont f

instance NonDet b => Unifiable (a -> b) where
  (=.=)    = error "(=.=) for function is undefined"
  (=.<=)   = error "(=.<=) for function is undefined"
  bind     = error "bind for function is undefined"
  lazyBind = error "lazyBind for function is undefined"
