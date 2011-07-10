{-# LANGUAGE MultiParamTypeClasses, Rank2Types #-}
module Types where

import ID

-- ---------------------------------------------------------------------------
-- Success type
-- ---------------------------------------------------------------------------

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
  searchNF _      cont C_Success = cont C_Success
  searchNF _      _    s         = error $ "C_Success.searchNF: " ++ show s

instance Unifiable C_Success where
  (=.=) C_Success C_Success = C_Success
  (=.=) _ _ = Fail_C_Success
  (=.<=) C_Success C_Success = C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i C_Success = ((i :=: (ChooseN 0 0)):(concat []))
  bind i (Choice_C_Success j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Success j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Success j@(Narrowed _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Success = [Unsolvable]
  bind i (Guard_C_Success cs e) = cs ++ (bind i e)
  lazyBind i C_Success = [(i :=: (ChooseN 0 0))]
  lazyBind i (Choice_C_Success j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Success j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Success j@(Narrowed _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Success = [Unsolvable]
  lazyBind i (Guard_C_Success cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]
-- END GENERATED FROM PrimTypes.curry

-- Higher Order functions
instance Show (a -> b) where
  show = error "show for function is undefined"

instance Read (a -> b) where
  readsPrec = error "read for function is undefined"

instance NonDet (a -> b) where
  choiceCons  = error "choiceCons for function is undefined"
  choicesCons = error "choicesCons for function is undefined"
  failCons    = error "failed"
  guardCons   = error "guardCons for function is undefined"
  try         = error "try for function is undefined"

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
-- Try structure
-- ---------------------------------------------------------------------------

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
tryChoices i@(FreeID _ _) = Frees   i
tryChoices i@(Narrowed _ _) = Choices i

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
  searchNF :: (forall b . NormalForm b => (b -> c) -> b -> c) -> (a -> c) -> a -> c
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
nfChoices cont i@(FreeID _ _)   xs = cont (choicesCons i xs)
nfChoices cont i@(Narrowed _ _) xs = choicesCons i (map (cont $!!) xs)


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
nfChoicesIO cont i@(FreeID _ _) xs = lookupChoiceID i >>= choose
  where
  choose (ChooseN c _, _) = cont $!< (xs !! c)
  choose (LazyBind cs, _) = do
    setChoice i NoChoice
    cont (guardCons cs (choicesCons i xs))
  choose (NoChoice   , _) = cont (choicesCons i xs) -- TODO replace i with j?
  choose c                = error $ "Basics.nfChoicesIO.choose: " ++ show c
nfChoicesIO cont i@(Narrowed  _ _) xs = cont (choicesCons i xs)
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

-- ---------------------------------------------------------------------------
-- Auxiliaries for Show and Read
-- ---------------------------------------------------------------------------

showsChoice :: Show a => Int -> ID -> a -> a -> ShowS
{-
showsChoice d i@(ID _) x1 x2 =
  showChar '(' .
  showsPrec d x1 .
  showString " ?" . shows i . showChar ' ' .
  showsPrec d x2 .
  showChar ')'
-}
showsChoice d i@(ID _) x1 x2 =
  showString "(?" . shows i . showChar ' ' .
  showsPrec d x1 . showChar ' ' .
  showsPrec d x2 .
  showChar ')'
showsChoice _ _ _ _ = error "showsChoice with no ID"

showsChoices :: Show a => Int -> ID -> [a] -> ShowS
showsChoices d i@(ID _) _        = error "showsChoices with ID"
showsChoices d i@(FreeID _ _) _  = shows i
showsChoices d i@(Narrowed _ _) xs
  = showString "[?" . shows i
  . foldr (.) id (zipWith showNarrowing [(0 :: Int) ..] xs)
  . showChar ']'
  where showNarrowing n x = showString ", " . shows n
                          . showString "->" . showsPrec d x

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
