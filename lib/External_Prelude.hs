{-# LANGUAGE MagicHash, MultiParamTypeClasses #-}

import qualified Control.Exception as C

-- ATTENTION: Do not introduce line breaks in import declarations as these
-- are not recognized!
import GHC.Exts (Int (I#), Int#, (==#), (/=#), (<#), (>#), (<=#), (+#), (-#), (*#), quotInt#, remInt#, negateInt#)
import GHC.Exts (Float (F#), Float#, eqFloat#, leFloat#, negateFloat#)
import GHC.Exts (Char (C#), Char#, eqChar#, leChar#, ord#, chr#)
import System.IO

import PrimTypes

-- ---------------------------------------------------------------------------
-- Externals
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Curry types
-- ---------------------------------------------------------------------------

-- Class for Curry types
class (Show a, Read a, NonDet a, Generable a, NormalForm a, Unifiable a)
      => Curry a where
  -- implementation of strict equalit (==) for a data type
  (=?=) :: a -> a -> C_Bool
  (=?=) = error "(==) is undefined"

  -- implementation of less-or-equal (<=) for a data type
  (<?=) :: a -> a -> C_Bool
  (<?=) = error "(<=) is undefined"

instance Curry (PrimData a) where
  (=?=) = error "(==) is undefined for primitive data"
  (<?=) = error "(<=) is undefined for primitive data"

-- BEGIN GENERATED FROM PrimTypes.curry
instance Curry C_Success where
  (=?=) (Choice_C_Success i x y) z = narrow i (x =?= z) (y =?= z)
  (=?=) (Choices_C_Success i xs) y = narrows i (map ((=?= y)) xs)
  (=?=) (Guard_C_Success c x) y = guardCons c (x =?= y)
  (=?=) Fail_C_Success _ = failCons
  (=?=) z (Choice_C_Success i x y) = narrow i (z =?= x) (z =?= y)
  (=?=) y (Choices_C_Success i xs) = narrows i (map ((y =?=)) xs)
  (=?=) y (Guard_C_Success c x) = guardCons c (y =?= x)
  (=?=) _ Fail_C_Success = failCons
  (=?=) C_Success C_Success = C_True
  (<?=) (Choice_C_Success i x y) z = narrow i (x <?= z) (y <?= z)
  (<?=) (Choices_C_Success i xs) y = narrows i (map ((<?= y)) xs)
  (<?=) (Guard_C_Success c x) y = guardCons c (x <?= y)
  (<?=) Fail_C_Success _ = failCons
  (<?=) z (Choice_C_Success i x y) = narrow i (z <?= x) (z <?= y)
  (<?=) y (Choices_C_Success i xs) = narrows i (map ((y <?=)) xs)
  (<?=) y (Guard_C_Success c x) = guardCons c (y <?= x)
  (<?=) _ Fail_C_Success = failCons
  (<?=) C_Success C_Success = C_True
-- END GENERATED FROM PrimTypes.curry


instance (Curry t0,Curry t1) => Curry (Func t0 t1) where
  (=?=) = error "(==) is undefined for functions"
  (<?=) = error "(<=) is undefined for functions"

instance Curry t0 => Curry (C_IO t0) where
  (=?=) = error "(==) is undefined for I/O actions"
  (<?=) = error "(<=) is undefined for I/O actions"

instance Curry (a -> b) where
  (=?=) = error "(==) is undefined for functions"
  (<?=) = error "(<=) is undefined for functions"

-- ---------------------------------------------------------------------------
-- Int
-- ---------------------------------------------------------------------------

-- BEGIN GENERATED FROM PrimTypes.curry
data C_Int
     = C_Int Int#
     | C_CurryInt BinInt
     | Choice_C_Int ID C_Int C_Int
     | Choices_C_Int ID ([C_Int])
     | Fail_C_Int
     | Guard_C_Int ([Constraint]) C_Int

instance Show C_Int where
  showsPrec d (Choice_C_Int i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Int i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Int c e) = showsGuard d c e
  showsPrec _ Fail_C_Int = showChar '!'
  showsPrec d (C_Int x1) = shows (I# x1)
  showsPrec d (C_CurryInt x1) = case id $## x1 of
    Choice_BinInt _ _ _ -> shows x1
    Choices_BinInt _ _  -> shows x1
    Fail_BinInt         -> shows x1
    Guard_BinInt _ _    -> shows x1
    gnfBinInt           -> shows (I# (curryint2primint gnfBinInt))

instance Read C_Int where
  readsPrec d s = map readInt (readsPrec d s) where readInt (I# i, s) = (C_Int i, s)

instance NonDet C_Int where
  choiceCons = Choice_C_Int
  choicesCons = Choices_C_Int
  failCons = Fail_C_Int
  guardCons = Guard_C_Int
  try (Choice_C_Int i x y) = tryChoice i x y
  try (Choices_C_Int i xs) = tryChoices i xs
  try Fail_C_Int = Fail
  try (Guard_C_Int c e) = Guard c e
  try x = Val x

instance Generable C_Int where
  generate s = Choices_C_Int (freeID [1] s) [C_CurryInt (generate (leftSupply s))]

instance NormalForm C_Int where
  ($!!) cont x@(C_Int _) = cont x
  ($!!) cont (C_CurryInt x1) = (\y1 -> cont (C_CurryInt y1)) $!! x1
  ($!!) cont (Choice_C_Int i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_Int i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_Int c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_Int = failCons
  ($##) cont x@(C_Int _) = cont x
  ($##) cont (C_CurryInt x1) = (\y1 -> cont (C_CurryInt y1)) $## x1
  ($##) cont (Choice_C_Int i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_Int i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_Int c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_Int = failCons
  ($!<) cont (C_CurryInt x1) = (\y1 -> cont (C_CurryInt y1)) $!< x1
  ($!<) cont (Choice_C_Int i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Int i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont x@(C_Int _) = cont x
  searchNF search cont (C_CurryInt x1) = search (\y1 -> cont (C_CurryInt y1)) x1

instance Unifiable C_Int where
  (=.=) (C_Int      x1) (C_Int      y1) = if (x1 ==# y1) then C_Success else Fail_C_Success
  (=.=) (C_Int      x1) (C_CurryInt y1) = (primint2curryint x1) =:= y1
  (=.=) (C_CurryInt x1) (C_Int      y1) = x1 =:= (primint2curryint y1)
  (=.=) (C_CurryInt x1) (C_CurryInt y1) = x1 =:= y1
  (=.=) _ _ = Fail_C_Success
  (=.<=) (C_Int      x1) (C_Int      y1) = if (x1 ==# y1) then C_Success else Fail_C_Success
  (=.<=) (C_Int      x1) (C_CurryInt y1) = (primint2curryint x1) =:<= y1
  (=.<=) (C_CurryInt x1) (C_Int      y1) = x1 =:<= (primint2curryint y1)
  (=.<=) (C_CurryInt x1) (C_CurryInt y1) = x1 =:<= y1
  (=.<=) _ _ = Fail_C_Success
  bind i (C_Int      x2) = (i :=: ChooseN 0 1) : bind (leftID i) (primint2curryint x2)
  bind i (C_CurryInt x2) = (i :=: ChooseN 0 1) : bind (leftID i) x2
  bind i (Choice_C_Int j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Int j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Int j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Int = [Unsolvable]
  bind i (Guard_C_Int cs e) = cs ++ (bind i e)
  lazyBind i (C_Int      x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) (primint2curryint x2))]
  lazyBind i (C_CurryInt x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) x2)]
  lazyBind i (Choice_C_Int j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Int j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Int j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Int = [Unsolvable]
  lazyBind i (Guard_C_Int cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry_Prelude.Curry C_Int where
  (=?=) (Choice_C_Int i x y) z = narrow i (x =?= z) (y =?= z)
  (=?=) (Choices_C_Int i xs) y = narrows i (map ((=?= y)) xs)
  (=?=) (Guard_C_Int c x) y = guardCons c (x =?= y)
  (=?=) Fail_C_Int _ = failCons
  (=?=) z (Choice_C_Int i x y) = narrow i (z =?= x) (z =?= y)
  (=?=) y (Choices_C_Int i xs) = narrows i (map ((y =?=)) xs)
  (=?=) y (Guard_C_Int c x) = guardCons c (y =?= x)
  (=?=) _ Fail_C_Int = failCons
  (=?=) (C_Int      x1) (C_Int      y1) = toCurry (x1 ==# y1)
  (=?=) (C_Int      x1) (C_CurryInt y1) = (primint2curryint x1) =?= y1
  (=?=) (C_CurryInt x1) (C_Int      y1) = x1 =?= (primint2curryint y1)
  (=?=) (C_CurryInt x1) (C_CurryInt y1) = x1 =?= y1
  (<?=) (Choice_C_Int i x y) z = narrow i (x <?= z) (y <?= z)
  (<?=) (Choices_C_Int i xs) y = narrows i (map ((<?= y)) xs)
  (<?=) (Guard_C_Int c x) y = guardCons c (x <?= y)
  (<?=) Fail_C_Int _ = failCons
  (<?=) z (Choice_C_Int i x y) = narrow i (z <?= x) (z <?= y)
  (<?=) y (Choices_C_Int i xs) = narrows i (map ((y <?=)) xs)
  (<?=) y (Guard_C_Int c x) = guardCons c (y <?= x)
  (<?=) _ Fail_C_Int = failCons
  (<?=) (C_Int      x1) (C_Int      y1) = toCurry (x1 <=# y1)
  (<?=) (C_Int      x1) (C_CurryInt y1) = (primint2curryint x1) `d_C_lteqInteger` y1
  (<?=) (C_CurryInt x1) (C_Int      y1) = x1 `d_C_lteqInteger` (primint2curryint y1)
  (<?=) (C_CurryInt x1) (C_CurryInt y1) = x1 `d_C_lteqInteger` y1
-- END GENERATED FROM PrimTypes.curry

primint2curryint :: Int# -> BinInt
primint2curryint n
  | n <#  0#  = Neg (primint2currynat (negateInt# n))
  | n ==# 0#  = Zero
  | otherwise = Pos (primint2currynat n)

primint2currynat :: Int# -> Nat
primint2currynat n
  | n ==# 1#                = IHi
  | (n `remInt#` 2#) ==# 0# = O (primint2currynat (n `quotInt#` 2#))
  | otherwise               = I (primint2currynat (n `quotInt#` 2#))

currynat2primint :: Nat -> Int#
currynat2primint IHi   = 1#
currynat2primint (O n) = 2# *# currynat2primint n
currynat2primint (I n) = 2# *# currynat2primint n +# 1#
currynat2primint _ = error "KiCS2 error: Prelude.currynat2primint: no ground term"

curryint2primint :: BinInt -> Int#
curryint2primint Zero    = 0#
curryint2primint (Pos n) = currynat2primint n
curryint2primint (Neg n) = negateInt# (currynat2primint n)
curryint2primint _ = error "KiCS2 error: Prelude.curryint2primint: no ground term"




--   match valF _ _ _ _ v@(C_Int _) = valF v
--   match _ fail _ _ _ Fail_C_Int  = fail
--   match _ _ choiceF _ _ (Choice_C_Int i@(ID _) x y) = choiceF i x y
--   match _ _ _ freeF _ (Choice_C_Int i@(FreeID _) x y) = freeF i x y
--   match _ _ _ _ guardF (Guard_C_Int c x) = guardF c x

-- ---------------------------------------------------------------------------
-- Float
-- ---------------------------------------------------------------------------
data C_Float
     = C_Float Float#
     | Choice_C_Float ID C_Float C_Float
     | Choices_C_Float ID ([C_Float])
     | Fail_C_Float
     | Guard_C_Float ([Constraint]) C_Float

instance Show C_Float where
  showsPrec d (Choice_C_Float i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Float i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Float c e) = showsGuard d c e
  showsPrec d Fail_C_Float = showChar '!'
  showsPrec d (C_Float x1) = shows (F# x1)

instance Read C_Float where
  readsPrec d s = map readFloat (readsPrec d s) where readFloat (F# f, s) = (C_Float f, s)

instance NonDet C_Float where
  choiceCons = Choice_C_Float
  choicesCons = Choices_C_Float
  failCons = Fail_C_Float
  guardCons = Guard_C_Float
  try (Choice_C_Float i x y) = tryChoice i x y
  try (Choices_C_Float i xs) = tryChoices i xs
  try Fail_C_Float = Fail
  try (Guard_C_Float c e) = Guard c e
  try x = Val x

instance Generable C_Float where
  generate _ = error "No generator for C_Float"

instance NormalForm C_Float where
  ($!!) cont x@(C_Float _) = cont x
  ($!!) cont (Choice_C_Float i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_Float i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_Float c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_Float = failCons
  ($##) cont x@(C_Float _) = cont x
  ($##) cont (Choice_C_Float i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_Float i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_Float c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_Float = failCons
  ($!<) cont (Choice_C_Float i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Float i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont x@(C_Float _) = cont x

instance Unifiable C_Float where
  (=.=) _ _ = Fail_C_Success
  (=.<=) _ _ = Fail_C_Success
  bind i (Choice_C_Float j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Float j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Float j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Float = [Unsolvable]
  bind i (Guard_C_Float cs e) = cs ++ (bind i e)
  lazyBind i (Choice_C_Float j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Float j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Float j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Float = [Unsolvable]
  lazyBind i (Guard_C_Float cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry C_Float where
  (=?=) (Choice_C_Float i x y) z = narrow i (x =?= z) (y =?= z)
  (=?=) (Choices_C_Float i xs) y = narrows i (map ((=?= y)) xs)
  (=?=) (Guard_C_Float c x) y = guardCons c (x =?= y)
  (=?=) Fail_C_Float _ = failCons
  (=?=) z (Choice_C_Float i x y) = narrow i (z =?= x) (z =?= y)
  (=?=) y (Choices_C_Float i xs) = narrows i (map ((y =?=)) xs)
  (=?=) y (Guard_C_Float c x) = guardCons c (y =?= x)
  (=?=) _ Fail_C_Float = failCons
  (=?=) (C_Float x1) (C_Float y1) = toCurry (x1 `eqFloat#` y1)
  (<?=) (Choice_C_Float i x y) z = narrow i (x <?= z) (y <?= z)
  (<?=) (Choices_C_Float i xs) y = narrows i (map ((<?= y)) xs)
  (<?=) (Guard_C_Float c x) y = guardCons c (x <?= y)
  (<?=) Fail_C_Float _ = failCons
  (<?=) z (Choice_C_Float i x y) = narrow i (z <?= x) (z <?= y)
  (<?=) y (Choices_C_Float i xs) = narrows i (map ((y <?=)) xs)
  (<?=) y (Guard_C_Float c x) = guardCons c (y <?= x)
  (<?=) _ Fail_C_Float = failCons
  (<?=) (C_Float x1) (C_Float y1) = toCurry (x1 `leFloat#` y1)

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------
data C_Char
     = C_Char Char#
     | CurryChar BinInt
     | Choice_C_Char ID C_Char C_Char
     | Choices_C_Char ID ([C_Char])
     | Fail_C_Char
     | Guard_C_Char ([Constraint]) C_Char

instance Show C_Char where
  showsPrec d (Choice_C_Char i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Char i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Char c e) = showsGuard d c e
  showsPrec d Fail_C_Char = showChar '!'
  showsPrec d (C_Char x1) = showString (show (C# x1))
  showsPrec d (CurryChar x1) = case id $## x1 of
    Choice_BinInt _ _ _ -> showString "chr " . shows x1
    Choices_BinInt _ _  -> showString "chr " . shows x1
    Fail_BinInt         -> shows x1
    Guard_BinInt _ _    -> shows x1
    gnfBinInt           -> shows (C# (curryChar2primChar gnfBinInt))

  showList cs = showList (map convert cs)
   where
    convert (C_Char c) = C# c
    convert (CurryChar c) = C# (curryChar2primChar c)

instance Read C_Char where
  readsPrec d s = map readChar (readsPrec d s) where readChar (C# c, s) = (C_Char c, s)

  readList s = map readString (readList s) where readString (cs, s) = (map (\(C# c) -> C_Char c) cs, s)

instance NonDet C_Char where
  choiceCons = Choice_C_Char
  choicesCons = Choices_C_Char
  failCons = Fail_C_Char
  guardCons = Guard_C_Char
  try (Choice_C_Char i x y) = tryChoice i x y
  try (Choices_C_Char i xs) = tryChoices i xs
  try Fail_C_Char = Fail
  try (Guard_C_Char c e) = Guard c e
  try x = Val x

instance Generable C_Char where
  generate s = Choices_C_Char (freeID [1] s) [CurryChar (generate (leftSupply s))]

instance NormalForm C_Char where
  ($!!) cont x@(C_Char _) = cont x
  ($!!) cont (CurryChar x) = (cont . CurryChar) $!! x
  ($!!) cont (Choice_C_Char i x y) = nfChoice cont i x y
  ($!!) cont (Choices_C_Char i xs) = nfChoices cont i xs
  ($!!) cont (Guard_C_Char c x) = guardCons c (cont $!! x)
  ($!!) _ Fail_C_Char = failCons
  ($##) cont x@(C_Char _) = cont x
  ($##) cont (CurryChar x) = (cont . CurryChar) $## x
  ($##) cont (Choice_C_Char i x y) = gnfChoice cont i x y
  ($##) cont (Choices_C_Char i xs) = gnfChoices cont i xs
  ($##) cont (Guard_C_Char c x) = guardCons c (cont $## x)
  ($##) _ Fail_C_Char = failCons
  ($!<) cont (CurryChar x)         =( cont . CurryChar) $!< x
  ($!<) cont (Choice_C_Char i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Char i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont c@(C_Char _) = cont c
  searchNF search cont (CurryChar x) = search (cont . CurryChar) x

instance Unifiable C_Char where
  (=.=) (C_Char       x1) (C_Char      x2) | x1 `eqChar#` x2 = C_Success
                                           | otherwise = Fail_C_Success
  (=.=) (C_Char       x1) (CurryChar x2) = primChar2CurryChar x1 =:= x2
  (=.=) (CurryChar  x1) (C_Char      x2) = x1 =:= primChar2CurryChar x2
  (=.=) (CurryChar x1)    (CurryChar   x2) = x1 =:= x2
  (=.=) _                 _                = Fail_C_Success
  (=.<=) (C_Char       x1) (C_Char      x2) | x1 `eqChar#` x2 = C_Success
                                            | otherwise = Fail_C_Success
  (=.<=) (C_Char       x1) (CurryChar x2) = primChar2CurryChar x1 =:<= x2
  (=.<=) (CurryChar  x1) (C_Char      x2) = x1 =:<= primChar2CurryChar x2
  (=.<=) (CurryChar x1)    (CurryChar   x2) = x1 =:<= x2
  (=.<=) _                 _                = Fail_C_Success
  bind i (C_Char    x) = (i :=: ChooseN 0 1) : bind (leftID i) (primChar2CurryChar x)
  bind i (CurryChar x) = (i :=: ChooseN 0 1) : bind (leftID i) x
  bind i (Choice_C_Char j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Char j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Char j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Char = [Unsolvable]
  bind i (Guard_C_Char cs e) = cs ++ (bind i e)
  lazyBind i (C_Char    x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) (primChar2CurryChar x))]
  lazyBind i (CurryChar x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) x)]
  lazyBind i (Choice_C_Char j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Char j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Char j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Char = [Unsolvable]
  lazyBind i (Guard_C_Char cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry C_Char where
  (=?=) (Choice_C_Char i x y) z = narrow i (x =?= z) (y =?= z)
  (=?=) (Choices_C_Char i xs) y = narrows i (map ((=?= y)) xs)
  (=?=) (Guard_C_Char c x) y = guardCons c (x =?= y)
  (=?=) Fail_C_Char _ = failCons
  (=?=) z (Choice_C_Char i x y) = narrow i (z =?= x) (z =?= y)
  (=?=) y (Choices_C_Char i xs) = narrows i (map ((y =?=)) xs)
  (=?=) y (Guard_C_Char c x) = guardCons c (y =?= x)
  (=?=) _ Fail_C_Char = failCons
  (=?=) (C_Char x1) (C_Char y1) = toCurry (x1 `eqChar#` y1)
  (=?=) (C_Char      x1) (CurryChar y1) = (primChar2CurryChar x1) =?= y1
  (=?=) (CurryChar x1) (C_Char      y1) = x1 =?= (primChar2CurryChar y1)
  (=?=) (CurryChar x1) (CurryChar y1) = x1 =?= y1
  (<?=) (Choice_C_Char i x y) z = narrow i (x <?= z) (y <?= z)
  (<?=) (Choices_C_Char i xs) y = narrows i (map ((<?= y)) xs)
  (<?=) (Guard_C_Char c x) y = guardCons c (x <?= y)
  (<?=) Fail_C_Char _ = failCons
  (<?=) z (Choice_C_Char i x y) = narrow i (z <?= x) (z <?= y)
  (<?=) y (Choices_C_Char i xs) = narrows i (map ((y <?=)) xs)
  (<?=) y (Guard_C_Char c x) = guardCons c (y <?= x)
  (<?=) _ Fail_C_Char = failCons
  (<?=) (C_Char x1) (C_Char y1) = toCurry (x1 `leChar#` y1)
  (<?=) (C_Char      x1) (CurryChar y1) = (primChar2CurryChar x1) `d_C_lteqInteger` y1
  (<?=) (CurryChar x1) (C_Char      y1) = x1 `d_C_lteqInteger` (primChar2CurryChar y1)
  (<?=) (CurryChar x1) (CurryChar y1) = x1 `d_C_lteqInteger` y1


primChar2CurryChar :: Char# -> BinInt
primChar2CurryChar c = primint2curryint (ord# c)

curryChar2primChar :: BinInt -> Char#
curryChar2primChar c = chr# (curryint2primint c)
-- ---------------------------------------------------------------------------
-- Conversion from and to primitive Haskell types
-- ---------------------------------------------------------------------------

instance ConvertCurryHaskell C_Int Int where
  toCurry (I# i) = C_Int i

  fromCurry (C_Int i)      = I# i
  fromCurry (C_CurryInt i) = I# (curryint2primint i)
  fromCurry _              = error "KiCS2 error: Int data with no ground term"

instance ConvertCurryHaskell C_Int Integer where
  toCurry i = int2C_Int (fromInteger i)
   where
    int2C_Int (I# c) = C_Int c

  fromCurry (C_Int i) = toInteger (I# i)
  fromCurry (C_CurryInt i) = toInteger (I# (curryint2primint i))
  fromCurry _         = error "KiCS2 error: Int data with no ground term"

instance ConvertCurryHaskell C_Float Float where
  toCurry (F# f) = C_Float f

  fromCurry (C_Float f) = F# f
  fromCurry _           = error "KiCS2 error: Float data with no ground term"

instance ConvertCurryHaskell C_Char Char where
  toCurry (C# c) = C_Char c

  fromCurry (C_Char c) = C# c
  fromCurry (CurryChar c) = C# (curryChar2primChar c)
  fromCurry _          = error "KiCS2 error: Char data with no ground term"

instance (ConvertCurryHaskell ct ht) =>
         ConvertCurryHaskell (OP_List ct) [ht] where
  toCurry []     = OP_List
  toCurry (c:cs) = OP_Cons (toCurry c) (toCurry cs)

  fromCurry OP_List        = []
  fromCurry (OP_Cons c cs) = fromCurry c : fromCurry cs
  fromCurry _              = error "KiCS2 error: List data with no ground term"

instance ConvertCurryHaskell C_Bool Bool where
  toCurry True  = C_True
  toCurry False = C_False

  fromCurry C_True  = True
  fromCurry C_False = False
  fromCurry _       = error "KiCS2 error: Float data with no ground term"

instance ConvertCurryHaskell OP_Unit () where
  toCurry ()  = OP_Unit

  fromCurry OP_Unit = ()
  fromCurry _       = error "KiCS2 error: Unit data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2) =>
         ConvertCurryHaskell (OP_Tuple2 ct1 ct2) (ht1,ht2) where
  toCurry (x1,x2)  = OP_Tuple2 (toCurry x1) (toCurry x2)

  fromCurry (OP_Tuple2 x1 x2) = (fromCurry x1, fromCurry x2)
  fromCurry _       = error "KiCS2 error: Pair data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2,
          ConvertCurryHaskell ct3 ht3) =>
         ConvertCurryHaskell (OP_Tuple3 ct1 ct2 ct3) (ht1,ht2,ht3) where
  toCurry (x1,x2,x3)  = OP_Tuple3 (toCurry x1) (toCurry x2) (toCurry x3)

  fromCurry (OP_Tuple3 x1 x2 x3) = (fromCurry x1, fromCurry x2, fromCurry x3)
  fromCurry _       = error "KiCS2 error: Tuple3 data with no ground term occurred"

instance ConvertCurryHaskell ct ht =>
         ConvertCurryHaskell (C_Maybe ct) (Maybe ht) where
  toCurry Nothing  = C_Nothing
  toCurry (Just x) = C_Just (toCurry x)

  fromCurry C_Nothing  = Nothing
  fromCurry (C_Just x) = Just (fromCurry x)
  fromCurry _          = error "KiCS2 error: Maybe data with no ground term occurred"

--fromOrdering :: Ordering -> C_Ordering
--fromOrdering LT = C_LT
--fromOrdering EQ = C_EQ
--fromOrdering GT = C_GT


-- ---------------------------------------------------------------------------
-- Auxiliary operations for showing lists
-- ---------------------------------------------------------------------------

showsPrec4CurryList :: Show a => Int -> OP_List a -> ShowS
showsPrec4CurryList d cl =
  if isStandardCurryList cl
  then showsPrec d (clist2hlist cl)
  else showChar '(' . showsPrecRaw d cl . showChar ')'
 where
  isStandardCurryList OP_List = True
  isStandardCurryList (OP_Cons _ xs) = isStandardCurryList xs
  isStandardCurryList _ = False

  clist2hlist OP_List = []
  clist2hlist (OP_Cons x xs) = x : clist2hlist xs

  showsPrecRaw d (Choice_OP_List i x y) = showsChoice d i x y
  showsPrecRaw d (Choices_OP_List i xs) = showsChoices d i xs
  showsPrecRaw d (Guard_OP_List c e) = showsGuard d c e
  showsPrecRaw d Fail_OP_List = showChar '!'
  showsPrecRaw d OP_List = showString "[]"
  showsPrecRaw d (OP_Cons x xs) =
    showParen (d > 5) (showsPrec 6 x . showChar ':' . showsPrecRaw 5 xs)


--- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

-- External DFO
-- -------------

external_d_C_ensureNotFree :: Curry a => a -> a
external_d_C_ensureNotFree x =
  case try x of
    Choice i a b  -> choiceCons i (external_d_C_ensureNotFree a)
                                 (external_d_C_ensureNotFree b)
    Narrowed i xs -> choicesCons i (map external_d_C_ensureNotFree xs)
    Free i xs     -> narrows i (map external_d_C_ensureNotFree xs)
    -- TODO : reason about the case when there is a constraint
    --        for the free variable e
    Guard c e    -> guardCons c (external_d_C_ensureNotFree e)
    _            -> x

external_d_C_prim_error :: C_String -> a
external_d_C_prim_error s = error (fromCurry s)

external_d_C_failed :: NonDet a => a
external_d_C_failed = failCons

external_d_OP_eq_eq :: Curry a => a -> a -> C_Bool
external_d_OP_eq_eq  = (=?=)

external_d_OP_lt_eq :: Curry a => a -> a -> C_Bool
external_d_OP_lt_eq = (<?=)


external_d_C_prim_ord :: C_Char -> C_Int
external_d_C_prim_ord (C_Char c) = C_Int (ord# c)
external_d_C_prim_ord (CurryChar c) = C_CurryInt c

external_d_C_prim_chr :: C_Int -> C_Char
external_d_C_prim_chr (C_Int i)      = C_Char (chr# i)
external_d_C_prim_chr (C_CurryInt i) = CurryChar i

external_d_OP_plus :: C_Int -> C_Int -> C_Int
external_d_OP_plus (C_Int      x) (C_Int      y) = C_Int (x +# y)
external_d_OP_plus (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_OP_plus_hash` y)
external_d_OP_plus (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_OP_plus_hash` (primint2curryint y))
external_d_OP_plus (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_OP_plus_hash` y)
external_d_OP_plus x y = (\a -> (\b -> (a `external_d_OP_plus` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_OP_minus :: C_Int -> C_Int -> C_Int
external_d_OP_minus (C_Int      x) (C_Int      y) = C_Int (x -# y)
external_d_OP_minus (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_OP_minus_hash` y)
external_d_OP_minus (C_CurryInt x) (C_Int y)      = C_CurryInt (x `d_OP_minus_hash` (primint2curryint y))
external_d_OP_minus (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_OP_minus_hash` y)
external_d_OP_minus x y = (\a -> (\b -> (a `external_d_OP_minus` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_OP_star :: C_Int -> C_Int -> C_Int
external_d_OP_star (C_Int      x) (C_Int      y) = C_Int (x *# y)
external_d_OP_star (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_OP_star_hash` y)
external_d_OP_star (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_OP_star_hash` (primint2curryint y))
external_d_OP_star (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_OP_star_hash` y)
external_d_OP_star x y = (\a -> (\b -> (a `external_d_OP_star` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_quot :: C_Int -> C_Int -> C_Int
external_d_C_quot (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `quotInt#` y)
external_d_C_quot (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_C_quotInteger` y)
external_d_C_quot (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_C_quotInteger` (primint2curryint y))
external_d_C_quot (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_C_quotInteger` y)
external_d_C_quot x y = (\a -> (\b -> (a `external_d_C_quot` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_rem :: C_Int -> C_Int -> C_Int
external_d_C_rem (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `remInt#` y)
external_d_C_rem (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_C_remInteger` y)
external_d_C_rem (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_C_remInteger` (primint2curryint y))
external_d_C_rem (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_C_remInteger` y)
external_d_C_rem x y = (\a -> (\b -> (a `external_d_C_rem` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_quotRem :: C_Int -> C_Int -> OP_Tuple2 C_Int C_Int
external_d_C_quotRem (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_OP_Tuple2
  | otherwise = OP_Tuple2 (C_Int (x `quotInt#` y)) (C_Int (x `remInt#` y))
external_d_C_quotRem (C_Int      x) (C_CurryInt y) = mkIntTuple `d_dollar_bang` ((primint2curryint x) `d_C_quotRemInteger` y)
external_d_C_quotRem (C_CurryInt x) (C_Int      y) = mkIntTuple `d_dollar_bang` (x `d_C_quotRemInteger` (primint2curryint y))
external_d_C_quotRem (C_CurryInt x) (C_CurryInt y) = mkIntTuple `d_dollar_bang` (x `d_C_quotRemInteger` y)
external_d_C_quotRem x y = (\a -> (\b -> (a `external_d_C_quotRem` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

-- ---------------------------------------------------------------------------
-- PrimOps taken from GHC.Base
-- ---------------------------------------------------------------------------
divInt# :: Int# -> Int# -> Int#
x# `divInt#` y#
        -- Be careful NOT to overflow if we do any additional arithmetic
        -- on the arguments...  the following  previous version of this
        -- code has problems with overflow:
--    | (x# ># 0#) && (y# <# 0#) = ((x# -# y#) -# 1#) `quotInt#` y#
--    | (x# <# 0#) && (y# ># 0#) = ((x# -# y#) +# 1#) `quotInt#` y#
    | (x# ># 0#) && (y# <# 0#) = ((x# -# 1#) `quotInt#` y#) -# 1#
    | (x# <# 0#) && (y# ># 0#) = ((x# +# 1#) `quotInt#` y#) -# 1#
    | otherwise                = x# `quotInt#` y#

modInt# :: Int# -> Int# -> Int#
x# `modInt#` y#
    | (x# ># 0#) && (y# <# 0#) ||
      (x# <# 0#) && (y# ># 0#)    = if r# /=# 0# then r# +# y# else 0#
    | otherwise                   = r#
    where
    !r# = x# `remInt#` y#

external_d_C_div :: C_Int -> C_Int -> C_Int
external_d_C_div (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `divInt#` y)
external_d_C_div (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_C_divInteger` y)
external_d_C_div (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_C_divInteger` (primint2curryint y))
external_d_C_div (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_C_divInteger` y)
external_d_C_div x y = (\a -> (\b -> (a `external_d_C_div` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_mod :: C_Int -> C_Int -> C_Int
external_d_C_mod (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `modInt#` y)
external_d_C_mod (C_Int      x) (C_CurryInt y) = C_CurryInt ((primint2curryint x) `d_C_modInteger` y)
external_d_C_mod (C_CurryInt x) (C_Int      y) = C_CurryInt (x `d_C_modInteger` (primint2curryint y))
external_d_C_mod (C_CurryInt x) (C_CurryInt y) = C_CurryInt (x `d_C_modInteger` y)
external_d_C_mod x y = (\a -> (\b -> (a `external_d_C_mod` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

-- TODO: $! instead of $#?
external_d_C_divMod :: C_Int -> C_Int -> OP_Tuple2 C_Int C_Int
external_d_C_divMod (C_Int      x) (C_Int      y)
  | y ==# 0#  = Fail_OP_Tuple2
  | otherwise = OP_Tuple2 (C_Int (x `divInt#` y)) (C_Int (x `modInt#` y))
external_d_C_divMod (C_Int      x) (C_CurryInt y) = mkIntTuple `d_OP_dollar_hash` ((primint2curryint x) `d_C_divModInteger` y)
external_d_C_divMod (C_CurryInt x) (C_Int      y) = mkIntTuple `d_OP_dollar_hash` (x `d_C_divModInteger` (primint2curryint y))
external_d_C_divMod (C_CurryInt x) (C_CurryInt y) = mkIntTuple `d_OP_dollar_hash` (x `d_C_divModInteger` y)
external_d_C_divMod x y = (\a -> (\b -> (a `external_d_C_divMod` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

mkIntTuple :: OP_Tuple2 BinInt BinInt -> OP_Tuple2 C_Int C_Int
mkIntTuple (OP_Tuple2 d m) = OP_Tuple2 (C_CurryInt d) (C_CurryInt m)

external_d_C_negateFloat :: C_Float -> C_Float
external_d_C_negateFloat (C_Float x) = C_Float (negateFloat# x)
external_d_C_negateFloat x = external_d_C_negateFloat `d_OP_dollar_hash` x

external_d_OP_eq_colon_eq :: Unifiable a => a -> a -> C_Success
external_d_OP_eq_colon_eq = (=:=)

external_d_C_success :: C_Success
external_d_C_success = C_Success

external_d_OP_ampersand :: C_Success -> C_Success -> C_Success
external_d_OP_ampersand = (&)

external_d_C_return :: a -> C_IO a
external_d_C_return a = fromIO (return a)

external_d_C_prim_putChar :: C_Char -> C_IO OP_Unit
external_d_C_prim_putChar = fromHaskellIO1 putChar

external_d_C_getChar :: C_IO C_Char
external_d_C_getChar = fromHaskellIO0 getChar

external_d_C_prim_readFile :: C_String -> C_IO C_String
external_d_C_prim_readFile = fromHaskellIO1 readFile

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_writeFile :: C_String -> C_String -> C_IO OP_Unit
external_d_C_prim_writeFile = fromHaskellIO2 writeFile

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_appendFile :: C_String -> C_String -> C_IO OP_Unit
external_d_C_prim_appendFile = fromHaskellIO2 appendFile

external_d_C_catchFail :: C_IO a -> C_IO a -> C_IO a
external_d_C_catchFail act err = fromIO $ C.catch (toIOWithFailCheck act) handle
  where handle e = hPutStrLn stderr (show (e :: C.SomeException)) >> (toIO err)
        toIOWithFailCheck act =
          case act of Fail_C_IO -> ioError (userError "I/O action failed")
                      _         -> toIO act

external_d_C_prim_show :: Show a => a -> C_String
external_d_C_prim_show a = toCurry (show a)

external_d_C_cond :: Curry a => C_Success -> a -> a
external_d_C_cond succ a = const a `d_dollar_bang` succ

external_d_OP_eq_colon_lt_eq :: Curry a => a -> a -> C_Success
external_d_OP_eq_colon_lt_eq = (=:<=)

external_d_OP_eq_colon_lt_lt_eq :: Curry a => a -> a -> C_Success
external_d_OP_eq_colon_lt_lt_eq = error "external_d_OP_eq_colon_lt_lt_eq"

-- External ND
-- -----------

external_nd_OP_qmark :: NonDet a => a -> a -> IDSupply -> a
external_nd_OP_qmark x y ids = let i = thisID ids in i `seq` choiceCons i x y

-- External HO
-- -----------

external_d_OP_dollar_bang :: (NonDet a, NonDet b) => (a -> b) -> a -> b
external_d_OP_dollar_bang = d_dollar_bang

external_nd_OP_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> b
external_nd_OP_dollar_bang = nd_dollar_bang

external_d_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => (a -> b) -> a -> b
external_d_OP_dollar_bang_bang = ($!!)

external_nd_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> b
external_nd_OP_dollar_bang_bang f x s = (\y -> nd_apply f y s) $!! x

external_d_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => (a -> b) -> a -> b
external_d_OP_dollar_hash_hash = ($##)

external_nd_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> b
external_nd_OP_dollar_hash_hash f x s = (\y -> nd_apply f y s) $## x

external_d_C_apply :: (a -> b) -> a -> b
external_d_C_apply = d_apply

external_nd_C_apply :: NonDet b => Func a b -> a -> IDSupply -> b
external_nd_C_apply = nd_apply

external_d_C_catch :: C_IO a -> (C_IOError -> C_IO a) -> C_IO a
external_d_C_catch act cont = fromIO $ C.catch (toIO act) handle where
  handle e = toIO $ cont $ C_IOError $ toCurry $ show (e :: C.SomeException)

external_nd_C_catch :: C_IO a -> Func C_IOError (C_IO a) -> IDSupply -> C_IO a
external_nd_C_catch act cont s = fromIO $ C.catch (toIO act) handle where
  handle e = toIO $ nd_apply cont (C_IOError $ toCurry $ show (e :: C.SomeException)) s

-- TODO: Support non-deterministic IO ?
external_d_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> (t0 -> C_IO t1) -> C_IO t1
external_d_OP_gt_gt_eq m f = fromIO $ (toIO m) >>= toIO . f

-- TODO: Support non-deterministic IO ?
external_nd_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> Func t0 (C_IO t1) -> IDSupply -> C_IO t1
external_nd_OP_gt_gt_eq m f s = fromIO $ (toIO m) >>= \x -> toIO (nd_apply f x s)

-- Encapsulated search
-- -------------------

-- external_d_C_try :: (a -> Success) -> [a -> Success]
external_d_C_try = error "external_dho_C_try"

-- external_nd_C_try :: Func a Success -> [Func a Success]
external_nd_C_try = error "external_ndho_C_try"





-- Functions on Integer and Nat added from PrimTypes
-- -------------------------------------------------

instance Curry_Prelude.Curry Nat where
  (=?=) (Choice_Nat i x y) z = narrow i (x Curry_Prelude.=?= z) (y Curry_Prelude.=?= z)
  (=?=) (Choices_Nat i xs) y = narrows i (map ((Curry_Prelude.=?= y)) xs)
  (=?=) (Guard_Nat c x) y = guardCons c (x Curry_Prelude.=?= y)
  (=?=) Fail_Nat _ = failCons
  (=?=) z (Choice_Nat i x y) = narrow i (z Curry_Prelude.=?= x) (z Curry_Prelude.=?= y)
  (=?=) y (Choices_Nat i xs) = narrows i (map ((y Curry_Prelude.=?=)) xs)
  (=?=) y (Guard_Nat c x) = guardCons c (y Curry_Prelude.=?= x)
  (=?=) _ Fail_Nat = failCons
  (=?=) IHi IHi = Curry_Prelude.C_True
  (=?=) (O x1) (O y1) = x1 Curry_Prelude.=?= y1
  (=?=) (I x1) (I y1) = x1 Curry_Prelude.=?= y1
  (=?=) _ _ = Curry_Prelude.C_False
  (<?=) (Choice_Nat i x y) z = narrow i (x Curry_Prelude.<?= z) (y Curry_Prelude.<?= z)
  (<?=) (Choices_Nat i xs) y = narrows i (map ((Curry_Prelude.<?= y)) xs)
  (<?=) (Guard_Nat c x) y = guardCons c (x Curry_Prelude.<?= y)
  (<?=) Fail_Nat _ = failCons
  (<?=) z (Choice_Nat i x y) = narrow i (z Curry_Prelude.<?= x) (z Curry_Prelude.<?= y)
  (<?=) y (Choices_Nat i xs) = narrows i (map ((y Curry_Prelude.<?=)) xs)
  (<?=) y (Guard_Nat c x) = guardCons c (y Curry_Prelude.<?= x)
  (<?=) _ Fail_Nat = failCons
  (<?=) IHi IHi = Curry_Prelude.C_True
  (<?=) IHi (O _) = Curry_Prelude.C_True
  (<?=) IHi (I _) = Curry_Prelude.C_True
  (<?=) (O x1) (O y1) = x1 Curry_Prelude.<?= y1
  (<?=) (O _) (I _) = Curry_Prelude.C_True
  (<?=) (I x1) (I y1) = x1 Curry_Prelude.<?= y1
  (<?=) _ _ = Curry_Prelude.C_False


instance Curry_Prelude.Curry BinInt where
  (=?=) (Choice_BinInt i x y) z = narrow i (x Curry_Prelude.=?= z) (y Curry_Prelude.=?= z)
  (=?=) (Choices_BinInt i xs) y = narrows i (map ((Curry_Prelude.=?= y)) xs)
  (=?=) (Guard_BinInt c x) y = guardCons c (x Curry_Prelude.=?= y)
  (=?=) Fail_BinInt _ = failCons
  (=?=) z (Choice_BinInt i x y) = narrow i (z Curry_Prelude.=?= x) (z Curry_Prelude.=?= y)
  (=?=) y (Choices_BinInt i xs) = narrows i (map ((y Curry_Prelude.=?=)) xs)
  (=?=) y (Guard_BinInt c x) = guardCons c (y Curry_Prelude.=?= x)
  (=?=) _ Fail_BinInt = failCons
  (=?=) (Neg x1) (Neg y1) = x1 Curry_Prelude.=?= y1
  (=?=) Zero Zero = Curry_Prelude.C_True
  (=?=) (Pos x1) (Pos y1) = x1 Curry_Prelude.=?= y1
  (=?=) _ _ = Curry_Prelude.C_False
  (<?=) (Choice_BinInt i x y) z = narrow i (x Curry_Prelude.<?= z) (y Curry_Prelude.<?= z)
  (<?=) (Choices_BinInt i xs) y = narrows i (map ((Curry_Prelude.<?= y)) xs)
  (<?=) (Guard_BinInt c x) y = guardCons c (x Curry_Prelude.<?= y)
  (<?=) Fail_BinInt _ = failCons
  (<?=) z (Choice_BinInt i x y) = narrow i (z Curry_Prelude.<?= x) (z Curry_Prelude.<?= y)
  (<?=) y (Choices_BinInt i xs) = narrows i (map ((y Curry_Prelude.<?=)) xs)
  (<?=) y (Guard_BinInt c x) = guardCons c (y Curry_Prelude.<?= x)
  (<?=) _ Fail_BinInt = failCons
  (<?=) (Neg x1) (Neg y1) = x1 Curry_Prelude.<?= y1
  (<?=) (Neg _) Zero = Curry_Prelude.C_True
  (<?=) (Neg _) (Pos _) = Curry_Prelude.C_True
  (<?=) Zero Zero = Curry_Prelude.C_True
  (<?=) Zero (Pos _) = Curry_Prelude.C_True
  (<?=) (Pos x1) (Pos y1) = x1 Curry_Prelude.<?= y1
  (<?=) _ _ = Curry_Prelude.C_False



d_C_cmpNat :: Nat -> Nat -> Curry_Prelude.C_Ordering
d_C_cmpNat x1 x2 = case x1 of
     IHi -> d_OP__casePT_33 x2
     (O x5) -> d_OP__casePT_32 x5 x2
     (I x8) -> d_OP__casePT_30 x8 x2
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_cmpNat x1001 x2) (d_C_cmpNat x1002 x2)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_C_cmpNat z x2) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_cmpNat x1001 x2)
     _ -> failCons

d_C_succ :: Nat -> Nat
d_C_succ x1 = case x1 of
     IHi -> O IHi
     (O x2) -> I x2
     (I x3) -> O (d_C_succ x3)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_succ x1001) (d_C_succ x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_C_succ z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_succ x1001)
     _ -> failCons

d_C_pred :: Nat -> Nat
d_C_pred x1 = case x1 of
     IHi -> Curry_Prelude.d_C_failed
     (O x2) -> d_OP__casePT_28 x2
     (I x5) -> O x5
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_pred x1001) (d_C_pred x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_C_pred z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_pred x1001)
     _ -> failCons

d_OP_plus_caret :: Nat -> Nat -> Nat
d_OP_plus_caret x1 x2 = case x1 of
     IHi -> d_C_succ x2
     (O x3) -> d_OP__casePT_27 x3 x2
     (I x6) -> d_OP__casePT_26 x6 x2
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP_plus_caret x1001 x2) (d_OP_plus_caret x1002 x2)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP_plus_caret z x2) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP_plus_caret x1001 x2)
     _ -> failCons

d_OP_minus_caret :: Nat -> Nat -> BinInt
d_OP_minus_caret x1 x2 = case x1 of
     IHi -> d_Inc (Neg x2)
     (O x3) -> d_OP__casePT_25 x1 x3 x2
     (I x6) -> d_OP__casePT_24 x6 x2
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP_minus_caret x1001 x2) (d_OP_minus_caret x1002 x2)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP_minus_caret z x2) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP_minus_caret x1001 x2)
     _ -> failCons

d_C_mult2 :: BinInt -> BinInt
d_C_mult2 x1 = case x1 of
     (Pos x2) -> Pos (O x2)
     Zero -> Zero
     (Neg x3) -> Neg (O x3)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_mult2 x1001) (d_C_mult2 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_C_mult2 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_mult2 x1001)
     _ -> failCons

d_OP_star_caret :: Nat -> Nat -> Nat
d_OP_star_caret x1 x2 = case x1 of
     IHi -> x2
     (O x3) -> O (d_OP_star_caret x3 x2)
     (I x4) -> d_OP_plus_caret x2 (O (d_OP_star_caret x4 x2))
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP_star_caret x1001 x2) (d_OP_star_caret x1002 x2)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP_star_caret z x2) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP_star_caret x1001 x2)
     _ -> failCons

d_C_div2 :: Nat -> Nat
d_C_div2 x1 = case x1 of
     IHi -> Curry_Prelude.d_C_failed
     (O x2) -> x2
     (I x3) -> x3
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_div2 x1001) (d_C_div2 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_C_div2 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_div2 x1001)
     _ -> failCons

d_C_mod2 :: Nat -> BinInt
d_C_mod2 x1 = case x1 of
     IHi -> Pos IHi
     (O x2) -> Zero
     (I x3) -> Pos IHi
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_mod2 x1001) (d_C_mod2 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_C_mod2 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_mod2 x1001)
     _ -> failCons

d_C_quotRemNat :: Nat -> Nat -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_quotRemNat x1 x2 = d_OP__casePT_23 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 IHi)

d_OP_quotRemNat_dot_shift_dot_104 :: Nat -> Nat -> Nat
d_OP_quotRemNat_dot_shift_dot_104 x1 x2 = case x1 of
     (O x3) -> O x2
     (I x4) -> I x2
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemNat_dot_shift_dot_104 x1001 x2) (d_OP_quotRemNat_dot_shift_dot_104 x1002 x2)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP_quotRemNat_dot_shift_dot_104 z x2) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP_quotRemNat_dot_shift_dot_104 x1001 x2)
     _ -> failCons

d_C_lteqInteger :: BinInt -> BinInt -> Curry_Prelude.C_Bool
d_C_lteqInteger x1 x2 = Curry_Prelude.d_OP_slash_eq (d_C_cmpInteger x1 x2) Curry_Prelude.C_GT

d_C_cmpInteger :: BinInt -> BinInt -> Curry_Prelude.C_Ordering
d_C_cmpInteger x1 x2 = case x1 of
     Zero -> d_OP__casePT_14 x2
     (Pos x5) -> d_OP__casePT_13 x5 x2
     (Neg x8) -> d_OP__casePT_12 x8 x2
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_cmpInteger x1001 x2) (d_C_cmpInteger x1002 x2)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_C_cmpInteger z x2) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_cmpInteger x1001 x2)
     _ -> failCons

d_Neg :: BinInt -> BinInt
d_Neg x1 = case x1 of
     Zero -> Zero
     (Pos x2) -> Neg x2
     (Neg x3) -> Pos x3
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_Neg x1001) (d_Neg x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_Neg z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_Neg x1001)
     _ -> failCons

d_Inc :: BinInt -> BinInt
d_Inc x1 = case x1 of
     Zero -> Pos IHi
     (Pos x2) -> Pos (d_C_succ x2)
     (Neg x3) -> d_OP__casePT_11 x3
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_Inc x1001) (d_Inc x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_Inc z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_Inc x1001)
     _ -> failCons

d_C_dec :: BinInt -> BinInt
d_C_dec x1 = case x1 of
     Zero -> Neg IHi
     (Pos x2) -> d_OP__casePT_10 x2
     (Neg x5) -> Neg (d_C_succ x5)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_dec x1001) (d_C_dec x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_C_dec z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_dec x1001)
     _ -> failCons

d_OP_plus_hash :: BinInt -> BinInt -> BinInt
d_OP_plus_hash x1 x2 = case x1 of
     Zero -> x2
     (Pos x3) -> d_OP__casePT_9 x1 x3 x2
     (Neg x6) -> d_OP__casePT_8 x1 x6 x2
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP_plus_hash x1001 x2) (d_OP_plus_hash x1002 x2)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP_plus_hash z x2) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP_plus_hash x1001 x2)
     _ -> failCons

d_OP_minus_hash :: BinInt -> BinInt -> BinInt
d_OP_minus_hash x1 x2 = case x2 of
     Zero -> x1
     (Pos x3) -> d_OP_plus_hash x1 (Neg x3)
     (Neg x4) -> d_OP_plus_hash x1 (Pos x4)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP_minus_hash x1 x1001) (d_OP_minus_hash x1 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP_minus_hash x1 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP_minus_hash x1 x1001)
     _ -> failCons

d_OP_star_hash :: BinInt -> BinInt -> BinInt
d_OP_star_hash x1 x2 = case x1 of
     Zero -> Zero
     (Pos x3) -> d_OP__casePT_7 x3 x2
     (Neg x6) -> d_OP__casePT_6 x6 x2
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP_star_hash x1001 x2) (d_OP_star_hash x1002 x2)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP_star_hash z x2) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP_star_hash x1001 x2)
     _ -> failCons

d_C_quotRemInteger :: BinInt -> BinInt -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_quotRemInteger x1 x2 = case x2 of
     Zero -> Curry_Prelude.d_C_failed
     (Pos x3) -> d_OP__casePT_5 x3 x1
     (Neg x9) -> d_OP__casePT_4 x9 x1
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_quotRemInteger x1 x1001) (d_C_quotRemInteger x1 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_C_quotRemInteger x1 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_quotRemInteger x1 x1001)
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP2_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1001) (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_quotRemInteger_dot___hash_selFP2_hash_d z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1001)
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP3_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1001) (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_quotRemInteger_dot___hash_selFP3_hash_m z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1001)
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP5_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1001) (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_quotRemInteger_dot___hash_selFP5_hash_d z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1001)
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP6_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1001) (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_quotRemInteger_dot___hash_selFP6_hash_m z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1001)
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP8_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1001) (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_quotRemInteger_dot___hash_selFP8_hash_d z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1001)
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP9_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1001) (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_quotRemInteger_dot___hash_selFP9_hash_m z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1001)
     _ -> failCons

d_C_divModInteger :: BinInt -> BinInt -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_divModInteger x1 x2 = case x2 of
     Zero -> Curry_Prelude.d_C_failed
     (Pos x3) -> d_OP__casePT_3 x3 x1
     (Neg x11) -> d_OP__casePT_1 x11 x1
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_divModInteger x1 x1001) (d_C_divModInteger x1 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_C_divModInteger x1 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_divModInteger x1 x1001)
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP11_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_divModInteger_dot___hash_selFP11_hash_d x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP11_hash_d x1001) (d_OP_divModInteger_dot___hash_selFP11_hash_d x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_divModInteger_dot___hash_selFP11_hash_d z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP11_hash_d x1001)
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP12_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_divModInteger_dot___hash_selFP12_hash_m x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP12_hash_m x1001) (d_OP_divModInteger_dot___hash_selFP12_hash_m x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_divModInteger_dot___hash_selFP12_hash_m z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP12_hash_m x1001)
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP14_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_divModInteger_dot___hash_selFP14_hash_d x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP14_hash_d x1001) (d_OP_divModInteger_dot___hash_selFP14_hash_d x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_divModInteger_dot___hash_selFP14_hash_d z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP14_hash_d x1001)
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP15_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_divModInteger_dot___hash_selFP15_hash_m x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP15_hash_m x1001) (d_OP_divModInteger_dot___hash_selFP15_hash_m x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_divModInteger_dot___hash_selFP15_hash_m z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP15_hash_m x1001)
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP17_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_divModInteger_dot___hash_selFP17_hash_d x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP17_hash_d x1001) (d_OP_divModInteger_dot___hash_selFP17_hash_d x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_divModInteger_dot___hash_selFP17_hash_d z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP17_hash_d x1001)
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP18_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> BinInt
d_OP_divModInteger_dot___hash_selFP18_hash_m x1 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP18_hash_m x1001) (d_OP_divModInteger_dot___hash_selFP18_hash_m x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP_divModInteger_dot___hash_selFP18_hash_m z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP18_hash_m x1001)
     _ -> failCons

d_C_divInteger :: BinInt -> BinInt -> BinInt
d_C_divInteger x1 x2 = Curry_Prelude.d_C_fst (d_C_divModInteger x1 x2)

d_C_modInteger :: BinInt -> BinInt -> BinInt
d_C_modInteger x1 x2 = Curry_Prelude.d_C_snd (d_C_divModInteger x1 x2)

d_C_quotInteger :: BinInt -> BinInt -> BinInt
d_C_quotInteger x1 x2 = Curry_Prelude.d_C_fst (d_C_quotRemInteger x1 x2)

d_C_remInteger :: BinInt -> BinInt -> BinInt
d_C_remInteger x1 x2 = Curry_Prelude.d_C_snd (d_C_quotRemInteger x1 x2)

d_OP__casePT_1 x11 x1 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x12) -> let
          x13 = d_C_quotRemNat x12 x11
          x14 = d_OP_divModInteger_dot___hash_selFP14_hash_d x13
          x15 = d_OP_divModInteger_dot___hash_selFP15_hash_m x13
           in (d_OP__casePT_0 x11 x14 x15)
     (Neg x18) -> let
          x19 = d_C_quotRemNat x18 x11
          x20 = d_OP_divModInteger_dot___hash_selFP17_hash_d x19
          x21 = d_OP_divModInteger_dot___hash_selFP18_hash_m x19
           in (Curry_Prelude.OP_Tuple2 x20 (d_Neg x21))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_1 x11 x1001) (d_OP__casePT_1 x11 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_1 x11 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_1 x11 x1001)
     _ -> failCons

d_OP__casePT_0 x11 x14 x15 = case x15 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_Neg x14) x15
     (Neg x16) -> Curry_Prelude.OP_Tuple2 (d_Neg (d_Inc x14)) (d_OP_minus_hash x15 (Pos x11))
     (Pos x17) -> Curry_Prelude.OP_Tuple2 (d_Neg (d_Inc x14)) (d_OP_minus_hash x15 (Pos x11))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_0 x11 x14 x1001) (d_OP__casePT_0 x11 x14 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_0 x11 x14 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_0 x11 x14 x1001)
     _ -> failCons

d_OP__casePT_3 x3 x1 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3
          x7 = d_OP_divModInteger_dot___hash_selFP11_hash_d x6
          x8 = d_OP_divModInteger_dot___hash_selFP12_hash_m x6
           in (d_OP__casePT_2 x3 x7 x8)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_3 x3 x1001) (d_OP__casePT_3 x3 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_3 x3 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_3 x3 x1001)
     _ -> failCons

d_OP__casePT_2 x3 x7 x8 = case x8 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_Neg x7) x8
     (Neg x9) -> Curry_Prelude.OP_Tuple2 (d_Neg (d_Inc x7)) (d_OP_minus_hash (Pos x3) x8)
     (Pos x10) -> Curry_Prelude.OP_Tuple2 (d_Neg (d_Inc x7)) (d_OP_minus_hash (Pos x3) x8)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_2 x3 x7 x1001) (d_OP__casePT_2 x3 x7 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_2 x3 x7 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_2 x3 x7 x1001)
     _ -> failCons

d_OP__casePT_4 x9 x1 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x10) -> let
          x11 = d_C_quotRemNat x10 x9
          x12 = d_OP_quotRemInteger_dot___hash_selFP5_hash_d x11
          x13 = d_OP_quotRemInteger_dot___hash_selFP6_hash_m x11
           in (Curry_Prelude.OP_Tuple2 (d_Neg x12) x13)
     (Neg x14) -> let
          x15 = d_C_quotRemNat x14 x9
          x16 = d_OP_quotRemInteger_dot___hash_selFP8_hash_d x15
          x17 = d_OP_quotRemInteger_dot___hash_selFP9_hash_m x15
           in (Curry_Prelude.OP_Tuple2 x16 (d_Neg x17))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_4 x9 x1001) (d_OP__casePT_4 x9 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_4 x9 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_4 x9 x1001)
     _ -> failCons

d_OP__casePT_5 x3 x1 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3
          x7 = d_OP_quotRemInteger_dot___hash_selFP2_hash_d x6
          x8 = d_OP_quotRemInteger_dot___hash_selFP3_hash_m x6
           in (Curry_Prelude.OP_Tuple2 (d_Neg x7) (d_Neg x8))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_5 x3 x1001) (d_OP__casePT_5 x3 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_5 x3 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_5 x3 x1001)
     _ -> failCons

d_OP__casePT_6 x6 x2 = case x2 of
     Zero -> Zero
     (Pos x7) -> Neg (d_OP_star_caret x6 x7)
     (Neg x8) -> Pos (d_OP_star_caret x6 x8)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_6 x6 x1001) (d_OP__casePT_6 x6 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_6 x6 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_6 x6 x1001)
     _ -> failCons

d_OP__casePT_7 x3 x2 = case x2 of
     Zero -> Zero
     (Pos x4) -> Pos (d_OP_star_caret x3 x4)
     (Neg x5) -> Neg (d_OP_star_caret x3 x5)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_7 x3 x1001) (d_OP__casePT_7 x3 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_7 x3 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_7 x3 x1001)
     _ -> failCons

d_OP__casePT_8 x1 x6 x2 = case x2 of
     Zero -> x1
     (Pos x7) -> d_OP_minus_caret x7 x6
     (Neg x8) -> Neg (d_OP_plus_caret x6 x8)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_8 x1 x6 x1001) (d_OP__casePT_8 x1 x6 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_8 x1 x6 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_8 x1 x6 x1001)
     _ -> failCons

d_OP__casePT_9 x1 x3 x2 = case x2 of
     Zero -> x1
     (Pos x4) -> Pos (d_OP_plus_caret x3 x4)
     (Neg x5) -> d_OP_minus_caret x3 x5
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_9 x1 x3 x1001) (d_OP__casePT_9 x1 x3 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_9 x1 x3 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_9 x1 x3 x1001)
     _ -> failCons

d_OP__casePT_10 x2 = case x2 of
     IHi -> Zero
     (O x3) -> Pos (d_C_pred (O x3))
     (I x4) -> Pos (O x4)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_10 x1001) (d_OP__casePT_10 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_10 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_10 x1001)
     _ -> failCons

d_OP__casePT_11 x3 = case x3 of
     IHi -> Zero
     (O x4) -> Neg (d_C_pred (O x4))
     (I x5) -> Neg (O x5)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_11 x1001) (d_OP__casePT_11 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_11 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_11 x1001)
     _ -> failCons

d_OP__casePT_12 x8 x2 = case x2 of
     Zero -> Curry_Prelude.C_LT
     (Pos x9) -> Curry_Prelude.C_LT
     (Neg x10) -> d_C_cmpNat x10 x8
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_12 x8 x1001) (d_OP__casePT_12 x8 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_12 x8 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_12 x8 x1001)
     _ -> failCons

d_OP__casePT_13 x5 x2 = case x2 of
     Zero -> Curry_Prelude.C_GT
     (Pos x6) -> d_C_cmpNat x5 x6
     (Neg x7) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_13 x5 x1001) (d_OP__casePT_13 x5 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_13 x5 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_13 x5 x1001)
     _ -> failCons

d_OP__casePT_14 x2 = case x2 of
     Zero -> Curry_Prelude.C_EQ
     (Pos x3) -> Curry_Prelude.C_LT
     (Neg x4) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_14 x1001) (d_OP__casePT_14 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_14 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_14 x1001)
     _ -> failCons

d_OP__casePT_23 x1 x2 x3 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Pos x1) Zero
     Curry_Prelude.C_False -> d_OP__casePT_22 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 IHi)
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_23 x1 x2 x1001) (d_OP__casePT_23 x1 x2 x1002)
     (Curry_Prelude.Choices_C_Bool x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_23 x1 x2 z) x1001)
     (Curry_Prelude.Guard_C_Bool x1000 x1001) -> guardCons x1000 (d_OP__casePT_23 x1 x2 x1001)
     _ -> failCons

d_OP__casePT_22 x1 x2 x3 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Zero (Pos x2)
     Curry_Prelude.C_False -> d_OP__casePT_21 x1 x2 Curry_Prelude.d_C_otherwise
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_22 x1 x2 x1001) (d_OP__casePT_22 x1 x2 x1002)
     (Curry_Prelude.Choices_C_Bool x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_22 x1 x2 z) x1001)
     (Curry_Prelude.Guard_C_Bool x1000 x1001) -> guardCons x1000 (d_OP__casePT_22 x1 x2 x1001)
     _ -> failCons

d_OP__casePT_21 x1 x2 x3 = case x3 of
     Curry_Prelude.C_True -> d_OP__casePT_20 x1 x2 (d_C_cmpNat x1 x2)
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_21 x1 x2 x1001) (d_OP__casePT_21 x1 x2 x1002)
     (Curry_Prelude.Choices_C_Bool x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_21 x1 x2 z) x1001)
     (Curry_Prelude.Guard_C_Bool x1000 x1001) -> guardCons x1000 (d_OP__casePT_21 x1 x2 x1001)
     _ -> failCons

d_OP__casePT_20 x1 x2 x3 = case x3 of
     Curry_Prelude.C_EQ -> Curry_Prelude.OP_Tuple2 (Pos IHi) Zero
     Curry_Prelude.C_LT -> Curry_Prelude.OP_Tuple2 Zero (Pos x1)
     Curry_Prelude.C_GT -> d_OP__casePT_19 x1 x2 (d_C_quotRemNat (d_C_div2 x1) x2)
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_20 x1 x2 x1001) (d_OP__casePT_20 x1 x2 x1002)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_20 x1 x2 z) x1001)
     (Curry_Prelude.Guard_C_Ordering x1000 x1001) -> guardCons x1000 (d_OP__casePT_20 x1 x2 x1001)
     _ -> failCons

d_OP__casePT_19 x1 x2 x5 = case x5 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__casePT_18 x1 x2 x4 x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_19 x1 x2 x1001) (d_OP__casePT_19 x1 x2 x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_19 x1 x2 z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP__casePT_19 x1 x2 x1001)
     _ -> failCons

d_OP__casePT_18 x1 x2 x4 x3 = case x3 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2)
     (Pos x5) -> d_OP__casePT_17 x1 x2 x5 x4
     (Neg x12) -> Curry_Prelude.d_C_failed
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_18 x1 x2 x4 x1001) (d_OP__casePT_18 x1 x2 x4 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_18 x1 x2 x4 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_18 x1 x2 x4 x1001)
     _ -> failCons

d_OP__casePT_17 x1 x2 x5 x4 = case x4 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) (d_C_mod2 x1)
     (Pos x6) -> d_OP__casePT_16 x1 x2 x5 x6 (d_C_quotRemNat (d_OP_quotRemNat_dot_shift_dot_104 x1 x6) x2)
     (Neg x11) -> Curry_Prelude.d_C_failed
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_17 x1 x2 x5 x1001) (d_OP__casePT_17 x1 x2 x5 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_17 x1 x2 x5 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_17 x1 x2 x5 x1001)
     _ -> failCons

d_OP__casePT_16 x1 x2 x5 x6 x9 = case x9 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> d_OP__casePT_15 x5 x8 x7
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_16 x1 x2 x5 x6 x1001) (d_OP__casePT_16 x1 x2 x5 x6 x1002)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_16 x1 x2 x5 x6 z) x1001)
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP__casePT_16 x1 x2 x5 x6 x1001)
     _ -> failCons

d_OP__casePT_15 x5 x8 x7 = case x7 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) x8
     (Pos x9) -> Curry_Prelude.OP_Tuple2 (Pos (d_OP_plus_caret (O x5) x9)) x8
     (Neg x10) -> Curry_Prelude.d_C_failed
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_15 x5 x8 x1001) (d_OP__casePT_15 x5 x8 x1002)
     (Choices_BinInt x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_15 x5 x8 z) x1001)
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_15 x5 x8 x1001)
     _ -> failCons

d_OP__casePT_24 x6 x2 = case x2 of
     IHi -> Pos (O x6)
     (O x7) -> d_Inc (d_C_mult2 (d_OP_minus_caret x6 x7))
     (I x8) -> d_C_mult2 (d_OP_minus_caret x6 x8)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_24 x6 x1001) (d_OP__casePT_24 x6 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_24 x6 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_24 x6 x1001)
     _ -> failCons

d_OP__casePT_25 x1 x3 x2 = case x2 of
     IHi -> Pos (d_C_pred x1)
     (O x4) -> d_C_mult2 (d_OP_minus_caret x3 x4)
     (I x5) -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5))
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_25 x1 x3 x1001) (d_OP__casePT_25 x1 x3 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_25 x1 x3 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_25 x1 x3 x1001)
     _ -> failCons

d_OP__casePT_26 x6 x2 = case x2 of
     IHi -> O (d_C_succ x6)
     (O x7) -> I (d_OP_plus_caret x6 x7)
     (I x8) -> O (d_OP_plus_caret (d_C_succ x6) x8)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_26 x6 x1001) (d_OP__casePT_26 x6 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_26 x6 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_26 x6 x1001)
     _ -> failCons

d_OP__casePT_27 x3 x2 = case x2 of
     IHi -> I x3
     (O x4) -> O (d_OP_plus_caret x3 x4)
     (I x5) -> I (d_OP_plus_caret x3 x5)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_27 x3 x1001) (d_OP__casePT_27 x3 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_27 x3 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_27 x3 x1001)
     _ -> failCons

d_OP__casePT_28 x2 = case x2 of
     IHi -> IHi
     (O x3) -> I (d_C_pred x2)
     (I x4) -> I (O x4)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_28 x1001) (d_OP__casePT_28 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_28 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_28 x1001)
     _ -> failCons

d_OP__casePT_30 x8 x2 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x9) -> d_OP__casePT_29 x8 x9 (d_C_cmpNat x8 x9)
     (I x10) -> d_C_cmpNat x8 x10
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_30 x8 x1001) (d_OP__casePT_30 x8 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_30 x8 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_30 x8 x1001)
     _ -> failCons

d_OP__casePT_29 x8 x9 x10 = case x10 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_GT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_29 x8 x9 x1001) (d_OP__casePT_29 x8 x9 x1002)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_29 x8 x9 z) x1001)
     (Curry_Prelude.Guard_C_Ordering x1000 x1001) -> guardCons x1000 (d_OP__casePT_29 x8 x9 x1001)
     _ -> failCons

d_OP__casePT_32 x5 x2 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x6) -> d_C_cmpNat x5 x6
     (I x7) -> d_OP__casePT_31 x5 x7 (d_C_cmpNat x5 x7)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_32 x5 x1001) (d_OP__casePT_32 x5 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_32 x5 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_32 x5 x1001)
     _ -> failCons

d_OP__casePT_31 x5 x7 x8 = case x8 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_LT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_31 x5 x7 x1001) (d_OP__casePT_31 x5 x7 x1002)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_31 x5 x7 z) x1001)
     (Curry_Prelude.Guard_C_Ordering x1000 x1001) -> guardCons x1000 (d_OP__casePT_31 x5 x7 x1001)
     _ -> failCons

d_OP__casePT_33 x2 = case x2 of
     IHi -> Curry_Prelude.C_EQ
     (O x3) -> Curry_Prelude.C_LT
     (I x4) -> Curry_Prelude.C_LT
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_33 x1001) (d_OP__casePT_33 x1002)
     (Choices_Nat x1000 x1001) -> narrows x1000 (map (\z -> d_OP__casePT_33 z) x1001)
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_33 x1001)
     _ -> failCons
