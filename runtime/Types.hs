-- ---------------------------------------------------------------------------
-- This module contains the basic type definitions to represent Curry types
-- in Haskell
-- ---------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Types
  ( module ConstStore
  , module ID
  , module Types
  ) where

import ConstStore
import Debug
import ID

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
  = Val a                     -- ^ Value in head normal form (HNF)
  | Fail Cover FailInfo       -- ^ Failure with covering depth and additional information
  | Choice Cover ID a a       -- ^ Binary choice, introduced by the (?) operator
  | Narrowed Cover ID [a]     -- ^ N-ary choice for narrowed variable
  | Free Cover ID [a]         -- ^ N-ary choice for free variable, where
                              --   N corresponds to the number of constructors of
                              --   the underlying type
  | Guard Cover Constraints a -- ^ Constrained value
    deriving Show

-- |Convert a binary choice of type a into one of type 'Try' a
tryChoice :: Cover -> ID -> a -> a -> Try a
tryChoice cd i@(ChoiceID    _  ) = Choice cd i
tryChoice _  _                   = internalError "Basics.tryChoice: no ChoiceID"

-- |Convert a n-ary choice of type a into one of type 'Try' a
tryChoices :: Cover -> ID -> [a] -> Try a
tryChoices cd i@(FreeID          _ _) = Free     cd i
tryChoices cd i@(NarrowedID      _ _) = Narrowed cd i
tryChoices _  i                       = internalError $ "Basics.tryChoices: wrong ID " ++ show i

-- unused because of triviality:

-- tryFail :: Try a
-- tryFail = Fail

-- tryGuard :: Constraints -> a -> Try a
-- tryGuard = Guard

-- tryVal :: a -> Try a
-- tryVal = Val

-- ---------------------------------------------------------------------------
-- Non-determinism
-- ---------------------------------------------------------------------------

-- |Class for types that support nondeterministic values
class NonDet a where
  -- |Construct a binary choice, used by the (?) operator
  choiceCons :: Cover -> ID -> a -> a -> a
  -- |Construct a n-ary choice, used for free variables and narrowing
  choicesCons:: Cover -> ID -> [a] -> a
  -- |Construct a failed computation
  failCons   :: Cover -> FailInfo -> a
  -- |Construct a constrained value
  guardCons  :: Cover -> Constraints -> a -> a
  -- |Convert a value into the generic 'Try' structure
  try        :: a -> Try a
  -- |Apply the adequate function to a non-deterministic value, where each of
  --  the supplied functions handles a different constructor.
  --
  -- /Note:/ This functionality was introduced to render the conversion from
  -- and to the 'Try' structure obsolete. Nonetheless, the performance impact
  -- still is to be analyzed.
  match      :: (Cover -> ID -> a -> a -> b)     -- ^ Binary Choice
             -> (Cover -> ID -> [a] -> b)        -- ^ n-ary Choice for narrowed variable
             -> (Cover -> ID -> [a] -> b)        -- ^ n-ary Choice for free variable
             -> (Cover -> FailInfo -> b)         -- ^ Failure
             -> (Cover -> Constraints -> a -> b) -- ^ Constrained value
             -> (a -> b)                         -- ^ Head Normal Form
             -> a                                -- ^ value to apply the functions to
             -> b

  try = match Choice Narrowed Free Fail Guard Val

  match chc nrwd fr fl grd vl x = case try x of
    Val v             -> vl v
    Fail cd info      -> fl cd info
    Choice cd i x1 x2 -> chc cd i x1 x2
    Narrowed cd i xs  -> nrwd cd i xs
    Free cd i xs      -> fr cd i xs
    Guard cd cs e     -> grd cd cs e

-- |Lift a choice encountered at pattern matching to the result value.
-- The name of this function is misleading because of historical reasons
-- and should be renamed to sth. like "choice"
narrow :: NonDet a => Cover -> ID -> a -> a -> a
narrow cd i@(ChoiceID      _) = choiceCons cd i
narrow _  _                   = internalError "Basics.narrow: no ChoiceID"

-- |Convert an n-ary choice of a free variable into one with a narrowed variable
-- |If the varible is bound in either the local or the global constraint store
-- |the value found in the store is used
narrows :: NonDet b => ConstStore -> Cover -> ID -> (a -> b) -> [a] -> b
narrows cs cd i@(FreeID        p s) f xs
  = lookupWithGlobalCs cs i f $ choicesCons cd (NarrowedID p s) (map f xs)
narrows _  cd i@(NarrowedID      _ _) f xs = choicesCons cd i (map f xs)
narrows _  _    (ChoiceID          _) _ _  = internalError "Types.narrows: ChoiceID"


bindOrNarrow :: Unifiable a => ID -> Cover -> ID -> [a] -> [Constraint]
bindOrNarrow i cd j@(FreeID p s) xs | isCovered cd = [ConstraintChoices cd (NarrowedID p s) (map (bind i) xs)]
                                    | otherwise    = [ i :=: BindTo j ]

lazyBindOrNarrow :: Unifiable a => ID -> Cover -> ID -> [a] -> [Constraint]
lazyBindOrNarrow i cd j@(FreeID p s) xs | isCovered cd = [ConstraintChoices cd (NarrowedID p s) (map (lazyBind i) xs)]
                                        | otherwise    = [ i :=: BindTo j ]

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
  -- new approach
  searchNF :: (forall b . NormalForm b => (b -> c) -> b -> c) -> (a -> c) -> a -> c

-- |Auxiliary function to apply the continuation to the normal forms of the
-- two alternatives of a binary choice.
nfChoice :: (NormalForm a, NonDet b) 
         => (a -> ConstStore -> b) -> Cover -> ID -> a -> a -> ConstStore -> b
nfChoice cont cd i x1 x2 cs = case i of
  ChoiceID      _ -> choiceCons cd i ((cont $!! x1) cs) ((cont $!! x2) cs)
  _               -> internalError "Basics.nfChoice: no ChoiceID" 

-- |Auxiliary function to apply the continuation to the normal forms of the
-- n alternatives of a n-ary choice.
nfChoices :: (NormalForm a, NonDet b) 
          => (a -> ConstStore -> b) -> Cover -> ID -> [a] -> ConstStore -> b
nfChoices cont cd i xs cs = case i of
   ChoiceID _     -> internalError "Basics.nfChoices: ChoiceID"
   FreeID _ _     -> cont (choicesCons cd i xs) cs
   NarrowedID _ _ -> choicesCons cd i (map (\x -> (cont $!! x) cs) xs)

-- |Auxiliary function to apply the continuation to the ground normal forms of
-- the two alternatives of a binary choice.
gnfChoice :: (NormalForm a, NonDet b) 
          => (a -> ConstStore -> b) -> Cover -> ID -> a -> a -> ConstStore -> b
gnfChoice cont cd i x1 x2 cs = case i of
  ChoiceID _ -> choiceCons cd i ((cont $## x1) cs) ((cont $## x2) cs)
  _          -> internalError "Basics.gnfChoice: no ChoiceID"

-- |Auxiliary function to apply the continuation to the ground normal forms of
-- the n alternatives of a n-ary choice.
gnfChoices :: (NormalForm a, NonDet b) 
           => (a -> ConstStore -> b) -> Cover -> ID -> [a] -> ConstStore -> b
gnfChoices cont cd i xs cs = narrows cs cd i (\x -> (cont $## x) cs) xs

-- ---------------------------------------------------------------------------
-- Generator function for free variables
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
class (NormalForm a) => Unifiable a where
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
(=:=) = unifyMatch

unifyMatch :: Unifiable a => a -> a -> ConstStore -> C_Success
unifyMatch x y cs = match uniChoice uniNarrowed uniFree failCons uniGuard uniVal x
  where
  uniChoice cd i x1 x2 = checkFail (choiceCons cd i ((x1 =:= y) cs) ((x2 =:= y) cs)) y
  uniNarrowed cd i xs  = checkFail (choicesCons cd i (map (\x' -> (x' =:= y) cs) xs)) y
  uniFree cdi i xs     = lookupCs cs i (\xval -> (xval =:= y) cs)
                                    (if isCovered cdi then (unifyMatch (narrows cs cdi i id xs) y cs) else (bindTo cs y))
                                      -- TODO: use global cs
    where
    bindTo cs' y' = match bindChoice bindNarrowed bindFree failCons bindGuard bindVal y'
      where
      bindChoice   cdj j y1 y2 = choiceCons  cdj j (bindTo cs' y1) (bindTo cs' y2)
      bindNarrowed cdj j ys    = choicesCons cdj j (map (bindTo cs') ys)
      bindFree     cdj j ys    = lookupCs cs j (bindTo cs') $
                              if isCovered cdj
                                then unifyMatch x (narrows cs cdj j id ys) cs
                                else guardCons defCover (ValConstr i y' [i :=: BindTo j]) C_Success
      bindGuard cdj c    = guardCons cdj c . (bindTo $! c `addCs` cs')
      bindVal v          = bindToVal i v cs'

  uniGuard cd c e    = checkFail (guardCons cd c ((e =:= y) $! c `addCs` cs)) y
  uniVal v          = uniWith cs y
    where
    uniWith cs' y' = match univChoice univNarrowed univFree failCons univGuard univVal y'
      where
      univChoice cd j y1 y2   = choiceCons cd  j (uniWith cs' y1) (uniWith cs' y2)
      univNarrowed cd j ys    = choicesCons cd j (map (uniWith cs') ys)
      univFree cd j ys        = lookupCs cs j (uniWith cs') 
                                 (if isCovered cd then uniWith cs' (narrows cs' cd j id ys) 
                                                  else (bindToVal j v cs'))
      univGuard cd c          = guardCons cd c . (uniWith $! c `addCs` cs')
      univVal w            = (v =.= w) cs'
  checkFail e = match (\_ _ _ _ -> e) const3 const3 failCons const3 (const e)
    where const3 _ _ _ = e

unifyTry :: Unifiable a => a -> a -> ConstStore -> C_Success
unifyTry xVal yVal csVal = unify (try xVal) (try yVal) csVal -- 1. compute HNF hx, hy
  where
  -- failure
  unify (Fail cd info) _              _ = failCons cd info
  unify _              (Fail cd info) _ = failCons cd info
  -- binary choice
  unify (Choice cd i x1 x2) hy cs = choiceCons cd i (unify (try x1) hy cs)
                                                    (unify (try x2) hy cs)
  unify hx (Choice cd j y1 y2) cs = choiceCons cd j (unify hx (try y1) cs)
                                                    (unify hx (try y2) cs)
  -- n-ary choice
  unify (Narrowed cd i xs) hy cs = choicesCons cd i (map (\x -> unify (try x) hy cs) xs)
  unify hx (Narrowed cd j ys) cs = choicesCons cd j (map (\y -> unify hx (try y) cs) ys)
  -- constrained value
  unify (Guard cd c x) hy cs = guardCons cd c (unify (try x) hy $! c `addCs` cs)
  unify hx (Guard cd c y) cs = guardCons cd c (unify hx (try y) $! c `addCs` cs)
  -- constructor-rooted terms
  unify (Val x) (Val y) cs = (x =.= y) cs
  -- two free variables
  unify hx@(Free cdi i xs) hy@(Free cdj j nfy) cs = lookupCs cs i
    (\x -> unify (try x) hy cs)
    (lookupCs cs j (\y -> unify hx (try y) cs)
                   (if isCovered cdi 
                    then unify (try (narrows cs cdi i id xs)) hy cs
                    else (if isCovered cdj
                           then unify hx (try (narrows cs cdj j id nfy)) cs
                           else guardCons cdi (ValConstr i nfy [i :=: BindTo j]) C_Success)))
  -- one free variable and one value
  unify (Free cdi i xs) hy@(Val y) cs = lookupCs cs i
    (\x -> unify (try x) hy cs) 
    (if isCovered cdi then unify (try (narrows cs cdi i id xs)) hy cs
                      else bindToVal i y cs)
  -- one free variable and one value
  unify hx@(Val x) (Free cdj j ys) cs = lookupCs cs j
    (\y -> unify hx (try y) cs) 
    (if isCovered cdj then unify hx (try (narrows cs cdj j id ys)) cs
                      else bindToVal j x cs)

bindToVal :: Unifiable a => ID -> a -> ConstStore -> C_Success
#ifdef STRICT_VAL_BIND
bindToVal i v cs = ((\w _ -> constrain defCover i w) $!! v) cs
#else
bindToVal i v _ =           constrain defCover i v
#endif

constrain :: Unifiable a => Cover -> ID -> a -> C_Success
constrain cd i v = guardCons cd (ValConstr i v (bind i v)) C_Success

  -- TODO2: Occurs check?

-- Lazy unification on general terms, used for function patterns
(=:<=) :: Unifiable a => a -> a -> ConstStore -> C_Success
(=:<=) x y cs = match uniChoice uniNarrowed uniFree failCons uniGuard uniVal x
  where
  -- binary choice
  uniChoice cd i x1 x2 = choiceCons cd i ((x1 =:<= y) cs) ((x2 =:<= y) cs)
  -- n-ary choice
  uniNarrowed cd i xs  = choicesCons cd i (map (\z -> (z =:<= y) cs) xs)
  -- constrained value
  uniGuard cd c e      = guardCons cd c ((e =:<= y) $! c `addCs` cs)
  -- free variable
  uniFree cd i xs   = lookupCs cs i (\xVal -> (xVal =:<= y) cs)
                       (if isCovered cd
                        then (narrows cs cd i id xs =:<= y) cs
                        else guardCons cd (StructConstr [i :=: LazyBind (lazyBind i y)]) C_Success)
  -- constructor-rooted term
  uniVal vx = unifyWith cs y
    where
    unifyWith cs' = match uniyChoice uniyNarrowed uniyFree failCons uniyGuard uniyVal
      where
      uniyChoice cd j y1 y2  = choiceCons cd j (unifyWith cs' y1) (unifyWith cs' y2)
      uniyNarrowed cd j ys   = choicesCons cd j (map (unifyWith cs') ys)
      uniyGuard cd c         = guardCons cd c . (unifyWith $! c `addCs` cs')
      uniyVal vy          = (vx =.<= vy) cs'
      uniyFree cd j ys    = 
         lookupCs cs' j (unifyWith cs') 
          (if isCovered cd
           then unifyWith cs' (narrows cs' cd j id ys)
           else guardCons cd (StructConstr [j :=: LazyBind (lazyBind j vx)]) C_Success)

-- ---------------------------------------------------------------------------
-- Conversion between Curry and Haskell data types
-- ---------------------------------------------------------------------------

class ConvertCurryHaskell ctype htype where -- needs MultiParamTypeClasses
  fromCurry :: ctype -> htype
  toCurry   :: htype -> ctype

instance (ConvertCurryHaskell ca ha, ConvertCurryHaskell cb hb)
  => ConvertCurryHaskell (ca -> cb) (ha -> hb) where
  fromCurry f = fromCurry . f . toCurry
  toCurry   f = toCurry   . f . fromCurry


-- ---------------------------------------------------------------------------
-- Auxiliaries for Show and Read
-- ---------------------------------------------------------------------------

showsChoice :: Show a => Int -> Cover -> ID -> a -> a -> ShowS
showsChoice d _ i@(ChoiceID _) x1 x2
  = showString "(?" . shows i . showChar ' '
  . showsPrec d x1 . showChar ' ' . showsPrec d x2
  . showChar ')'
showsChoice _ _ _ _ _ = internalError "showsChoice: No ChoiceID"

showsChoices :: Show a => Int -> Cover -> ID -> [a] -> ShowS
showsChoices _ _   (ChoiceID _)     _  = internalError "showsChoices: ChoiceID"
showsChoices _ _ i@(FreeID _ _)     _  = shows i
showsChoices d _ i@(NarrowedID _ _) xs
  = showString "[?" . shows i
  . foldr (.) id (zipWith showNarrowing [(0 :: Int) ..] xs)
  . showChar ']'
  where showNarrowing n x = showString ", " . shows n
                          . showString "->" . showsPrec d x

showsGuard :: (Show a, Show b) => Int -> Cover -> a -> b -> ShowS
showsGuard d _ c e = showsPrec d c . showString " &> " . showsPrec d e

-- |Read a possibly qualified name
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
     | Choice_C_Success Cover ID C_Success C_Success
     | Choices_C_Success Cover ID ([C_Success])
     | Fail_C_Success Cover FailInfo
     | Guard_C_Success Cover Constraints C_Success

instance Show C_Success where
  showsPrec d (Choice_C_Success cd i x y) = showsChoice d cd i x y
  showsPrec d (Choices_C_Success cd i xs) = showsChoices d cd i xs
  showsPrec d (Guard_C_Success cd cs e) = showsGuard d cd cs e
  showsPrec _ (Fail_C_Success _ _) = showChar '!'
  showsPrec _ C_Success = showString "Success"

instance Read C_Success where
  readsPrec _ s = readParen False (\r -> [ (C_Success,r0) | (_,r0) <- readQualified "Prelude" "Success" r]) s

instance NonDet C_Success where
  choiceCons = Choice_C_Success
  choicesCons = Choices_C_Success
  failCons = Fail_C_Success
  guardCons = Guard_C_Success
  try (Choice_C_Success cd i x y) = tryChoice cd i x y
  try (Choices_C_Success cd i xs) = tryChoices cd i xs
  try (Fail_C_Success cd info) = Fail cd info
  try (Guard_C_Success cd cs e) = Guard cd cs e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_Success cd i x y) = f cd i x y
  match _ f _ _ _ _ (Choices_C_Success cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_Success cd i@(FreeID _ _) xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_Success cd i@(ChoiceID _) _) = internalError ("Prelude.Success.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ (Fail_C_Success cd info) = f cd info
  match _ _ _ _ f _ (Guard_C_Success cd cs e) = f cd cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Success where
  generate s = Choices_C_Success defCover (freeID [0] s) [C_Success]

instance NormalForm C_Success where
  ($!!) cont C_Success = cont C_Success
  ($!!) cont (Choice_C_Success cd i x y) = nfChoice cont cd i x y
  ($!!) cont (Choices_C_Success cd i xs) = nfChoices cont cd i xs
  ($!!) cont (Guard_C_Success cd c x) = guardCons cd c (cont $!! x)
  ($!!) _    (Fail_C_Success cd info) = failCons cd info
  ($##) cont C_Success = cont C_Success
  ($##) cont (Choice_C_Success cd i x y) = gnfChoice cont cd i x y
  ($##) cont (Choices_C_Success cd i xs) = gnfChoices cont cd i xs
  ($##) cont (Guard_C_Success cd c x) = guardCons cd c (cont $## x)
  ($##) _    (Fail_C_Success cd info) = failCons cd info
  searchNF _ cont C_Success = cont C_Success
  searchNF _ _ x = internalError ("Prelude.Success.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Success where
  (=.=) C_Success C_Success _ = C_Success
  (=.=) _ _ _ = Fail_C_Success defCover defFailInfo
  (=.<=) C_Success C_Success _ = C_Success
  (=.<=) _ _ _ = Fail_C_Success defCover defFailInfo
  bind i C_Success = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_C_Success cd j l r) = [(ConstraintChoice cd j (bind i l) (bind i r))]
  bind i (Choices_C_Success cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs 
  bind i (Choices_C_Success cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Choices_C_Success cd i@(ChoiceID _) _) = internalError ("Prelude.Success.bind: Choices with ChoiceID: " ++ (show i))
  bind _ (Fail_C_Success cd info) = [Unsolvable info]
  bind i (Guard_C_Success _ cs e) = (getConstrList cs) ++ (bind i e)
  lazyBind i C_Success = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_C_Success cd j l r) = [(ConstraintChoice cd j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Success cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs 
  lazyBind i (Choices_C_Success cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Choices_C_Success cd i@(ChoiceID _) _) = internalError ("Prelude.Success.lazyBind: Choices with ChoiceID: " ++ (show i))
  lazyBind _ (Fail_C_Success cd info) = [Unsolvable info]
  lazyBind i (Guard_C_Success cd cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind i e)))]

-- END GENERATED FROM PrimTypes.curry

-- ---------------------------------------------------------------------------
-- Functions
-- ---------------------------------------------------------------------------

-- Higher Order functions
instance Show (a -> b) where
  show _ = "<<function>>"
--   show = internalError "show for function is undefined"

instance Read (a -> b) where
  readsPrec = internalError "read for function is undefined"

instance NonDet b => NonDet (a -> b) where
  choiceCons  cd i f g = \ x -> choiceCons  cd i (f x) (g x)
  choicesCons cd i fs  = \ x -> choicesCons cd i (map ($x) fs)
  failCons    cd info  = \ _ -> failCons cd info
  guardCons   cd  c f  = \ x -> guardCons cd c (f x)
  try                  = Val

instance NonDet b => Generable (a -> b) where
  generate = internalError "generate for function is undefined"

instance NonDet b => NormalForm (a -> b) where
  cont $!! f = cont f
  cont $## f = cont f
  searchNF _ cont f = cont f

instance NonDet b => Unifiable (a -> b) where
  (=.=)    = internalError "(=.=) for function is undefined"
  (=.<=)   = internalError "(=.<=) for function is undefined"
  bind     = internalError "bind for function is undefined"
  lazyBind = internalError "lazyBind for function is undefined"
