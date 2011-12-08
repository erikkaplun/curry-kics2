{-# LANGUAGE BangPatterns, MagicHash, MultiParamTypeClasses #-}

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
  (=?=) :: a -> a -> ConstStore -> C_Bool
  (=?=) = error "(==) is undefined"

  -- implementation of less-or-equal (<=) for a data type
  (<?=) :: a -> a -> ConstStore -> C_Bool
  (<?=) = error "(<=) is undefined"

instance Curry (PrimData a) where
  (=?=) = error "(==) is undefined for primitive data"
  (<?=) = error "(<=) is undefined for primitive data"

-- BEGIN GENERATED FROM PrimTypes.curry
instance Curry_Prelude.Curry C_Success where
  (=?=) (Choice_C_Success i x y) z cs = narrow i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_C_Success i xs) y cs = narrows cs i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_C_Success c e) y cs = guardCons c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) Fail_C_Success _ _ = failCons
  (=?=) z (Choice_C_Success i x y) cs = narrow i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_C_Success i xs) cs = narrows cs i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_C_Success c e) cs = guardCons c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ Fail_C_Success _ = failCons
  (=?=) C_Success C_Success cs = Curry_Prelude.C_True
  (<?=) (Choice_C_Success i x y) z cs = narrow i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_C_Success i xs) y cs = narrows cs i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_C_Success c e) y cs = guardCons c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) Fail_C_Success _ _ = failCons
  (<?=) z (Choice_C_Success i x y) cs = narrow i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_C_Success i xs) cs = narrows cs i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_C_Success c e) cs = guardCons c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ Fail_C_Success _ = failCons
  (<?=) C_Success C_Success cs = Curry_Prelude.C_True
-- END GENERATED FROM PrimTypes.curry


instance (Curry t0,Curry t1) => Curry (Func t0 t1) where
  (=?=) = error "(==) is undefined for functions"
  (<?=) = error "(<=) is undefined for functions"

instance Curry t0 => Curry (C_IO t0) where
  (=?=) = error "(==) is undefined for I/O actions"
  (<?=) = error "(<=) is undefined for I/O actions"

instance NonDet b => Curry (a -> b) where
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
     | Guard_C_Int (Constraints) C_Int

instance Show C_Int where
  showsPrec d (Choice_C_Int i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Int i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Int c e) = showsGuard d c e
  showsPrec _ Fail_C_Int = showChar '!'
  showsPrec d (C_Int x1) = shows (I# x1)
  showsPrec d (C_CurryInt x1) = case (const $## x1) emptyCs of
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
  match f _ _ _ _ _ (Choice_C_Int i x y) = f i x y
  match _ f _ _ _ _ (Choices_C_Int i@(NarrowedID _ _) xs) = f i xs
  match _ _ f _ _ _ (Choices_C_Int i@(FreeID _ _) xs) = f i xs
  match _ _ _ _ _ _ (Choices_C_Int i@(ChoiceID _) _) = error ("Prelude.Int.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ Fail_C_Int = f
  match _ _ _ _ f _ (Guard_C_Int cs e) = f cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Int where
  generate s = Choices_C_Int (freeID [1] s) [C_CurryInt (generate (leftSupply s))]

instance NormalForm C_Int where
  ($!!) cont x@(C_Int _) cs = cont x cs
  ($!!) cont (C_CurryInt x1) cs = ((\y1 -> cont (C_CurryInt y1)) $!! x1) cs
  ($!!) cont (Choice_C_Int i x y) cs = nfChoice cont i x y cs
  ($!!) cont (Choices_C_Int i xs) cs = nfChoices cont i xs cs
  ($!!) cont (Guard_C_Int c x) cs = guardCons c ((cont $!! x) (addCs c cs))
  ($!!) _ Fail_C_Int _ = failCons
  ($##) cont x@(C_Int _) cs = cont x cs
  ($##) cont (C_CurryInt x1) cs = ((\y1 -> cont (C_CurryInt y1)) $## x1) cs
  ($##) cont (Choice_C_Int i x y) cs = gnfChoice cont i x y cs
  ($##) cont (Choices_C_Int i xs) cs = gnfChoices cont i xs cs
  ($##) cont (Guard_C_Int c x) cs = guardCons c ((cont $## x) (addCs c cs))
  ($##) _ Fail_C_Int _ = failCons
  ($!<) cont (C_CurryInt x1) = (\y1 -> cont (C_CurryInt y1)) $!< x1
  ($!<) cont (Choice_C_Int i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Int i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont x@(C_Int _) = cont x
  searchNF search cont (C_CurryInt x1) = search (\y1 -> cont (C_CurryInt y1)) x1
  searchNF _ _ x = error ("Prelude.Int.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Int where
  (=.=) (C_Int      x1) (C_Int      y1) _ = if (x1 ==# y1) then C_Success else Fail_C_Success
  (=.=) (C_Int      x1) (C_CurryInt y1) cs = ((primint2curryint x1) =:= y1) cs
  (=.=) (C_CurryInt x1) (C_Int      y1) cs = (x1 =:= (primint2curryint y1)) cs
  (=.=) (C_CurryInt x1) (C_CurryInt y1) cs = (x1 =:= y1) cs
  (=.=) _ _ _ = Fail_C_Success
  (=.<=) (C_Int      x1) (C_Int      y1) _ = if (x1 ==# y1) then C_Success else Fail_C_Success
  (=.<=) (C_Int      x1) (C_CurryInt y1) cs = ((primint2curryint x1) =:<= y1) cs
  (=.<=) (C_CurryInt x1) (C_Int      y1) cs = (x1 =:<= (primint2curryint y1)) cs
  (=.<=) (C_CurryInt x1) (C_CurryInt y1) cs = (x1 =:<= y1) cs
  (=.<=) _ _ _= Fail_C_Success
  bind i (C_Int      x2) = (i :=: ChooseN 0 1) : bind (leftID i) (primint2curryint x2)
  bind i (C_CurryInt x2) = (i :=: ChooseN 0 1) : bind (leftID i) x2
  bind i (Choice_C_Int j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Int j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Int j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ c@(Choices_C_Int i@(ChoiceID _) _) = error ("Prelude.Int.bind: Choices with ChoiceID: " ++ (show c))
  bind _ Fail_C_Int = [Unsolvable]
  bind i (Guard_C_Int cs e) = getConstrList cs ++ (bind i e)
  lazyBind i (C_Int      x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) (primint2curryint x2))]
  lazyBind i (C_CurryInt x2) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) x2)]
  lazyBind i (Choice_C_Int j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Int j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Int j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ c@(Choices_C_Int i@(ChoiceID _) _) = error ("Prelude.Int.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _ Fail_C_Int = [Unsolvable]
  lazyBind i (Guard_C_Int cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry_Prelude.Curry C_Int where
  (=?=) (Choice_C_Int i x y) z cs = narrow i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Int i xs) y cs = narrows cs i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Int c x) y cs = guardCons c ((x =?= y) (addCs c cs))
  (=?=) Fail_C_Int _ _ = failCons
  (=?=) z (Choice_C_Int i x y) cs = narrow i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Int i xs) cs = narrows cs i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Int c x) cs = guardCons c ((y =?= x) (addCs c cs))
  (=?=) _ Fail_C_Int _ = failCons
  (=?=) (C_Int      x1) (C_Int      y1) _ = toCurry (x1 ==# y1)
  (=?=) (C_Int      x1) (C_CurryInt y1) cs = ((primint2curryint x1) =?= y1) cs
  (=?=) (C_CurryInt x1) (C_Int      y1) cs = (x1 =?= (primint2curryint y1)) cs
  (=?=) (C_CurryInt x1) (C_CurryInt y1) cs = (x1 =?= y1) cs
  (<?=) (Choice_C_Int i x y) z cs = narrow i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Int i xs) y cs = narrows cs i (\x -> (x<?= y) cs) xs
  (<?=) (Guard_C_Int c x) y cs = guardCons c ((x <?= y) (addCs c cs))
  (<?=) Fail_C_Int _ _ = failCons
  (<?=) z (Choice_C_Int i x y) cs = narrow i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Int i xs) cs = narrows cs i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Int c x) cs = guardCons c ((y <?= x) (addCs c cs))
  (<?=) _ Fail_C_Int _ = failCons
  (<?=) (C_Int      x1) (C_Int      y1) _ = toCurry (x1 <=# y1)
  (<?=) (C_Int      x1) (C_CurryInt y1) cs = ((primint2curryint x1) `d_C_lteqInteger` y1) cs
  (<?=) (C_CurryInt x1) (C_Int      y1) cs = (x1 `d_C_lteqInteger` (primint2curryint y1)) cs
  (<?=) (C_CurryInt x1) (C_CurryInt y1) cs = (x1 `d_C_lteqInteger` y1) cs
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


-- ---------------------------------------------------------------------------
-- Float
-- ---------------------------------------------------------------------------
data C_Float
     = C_Float Float#
     | Choice_C_Float ID C_Float C_Float
     | Choices_C_Float ID ([C_Float])
     | Fail_C_Float
     | Guard_C_Float (Constraints) C_Float

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
  match f _ _ _ _ _ (Choice_C_Float i x y) = f i x y
  match _ f _ _ _ _ (Choices_C_Float i@(NarrowedID _ _) xs) = f i xs
  match _ _ f _ _ _ (Choices_C_Float i@(FreeID _ _) xs) = f i xs
  match _ _ _ _ _ _ (Choices_C_Float i@(ChoiceID _) _) = error ("Prelude.Float.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ Fail_C_Float = f
  match _ _ _ _ f _ (Guard_C_Float cs e) = f cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Float where
  generate _ = error "No generator for C_Float"

instance NormalForm C_Float where
  ($!!) cont x@(C_Float _) cs = cont x cs
  ($!!) cont (Choice_C_Float i x y) cs = nfChoice cont i x y cs
  ($!!) cont (Choices_C_Float i xs) cs = nfChoices cont i xs cs
  ($!!) cont (Guard_C_Float c x) cs = guardCons c ((cont $!! x) (addCs c cs))
  ($!!) _ Fail_C_Float _ = failCons
  ($##) cont x@(C_Float _) cs = cont x cs
  ($##) cont (Choice_C_Float i x y) cs = gnfChoice cont i x y cs
  ($##) cont (Choices_C_Float i xs) cs = gnfChoices cont i xs cs
  ($##) cont (Guard_C_Float c x) cs = guardCons c ((cont $## x) (addCs c cs))
  ($##) _ Fail_C_Float _ = failCons
  ($!<) cont (Choice_C_Float i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Float i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont x@(C_Float _) = cont x
  searchNF _ _ x = error ("Prelude.Float.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Float where
  (=.=) _ _ _  = Fail_C_Success
  (=.<=) _ _ _ = Fail_C_Success
  bind i (Choice_C_Float j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Float j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Float j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ c@(Choices_C_Float i@(ChoiceID _) _) = error ("Prelude.Float.bind: Choices with ChoiceID: " ++ (show c))
  bind _ Fail_C_Float = [Unsolvable]
  bind i (Guard_C_Float cs e) = getConstrList cs ++ (bind i e)
  lazyBind i (Choice_C_Float j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Float j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Float j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ c@(Choices_C_Float i@(ChoiceID _) _) = error ("Prelude.Float.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _ Fail_C_Float = [Unsolvable]
  lazyBind i (Guard_C_Float cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry C_Float where
  (=?=) (Choice_C_Float i x y) z cs = narrow i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Float i xs) y cs = narrows cs i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Float c x) y cs = guardCons c ((x =?= y) (addCs c cs))
  (=?=) Fail_C_Float _ _= failCons
  (=?=) z (Choice_C_Float i x y) cs = narrow i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Float i xs) cs = narrows cs i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Float c x) cs = guardCons c ((y =?= x) (addCs c cs))
  (=?=) _ Fail_C_Float _ = failCons
  (=?=) (C_Float x1) (C_Float y1) _ = toCurry (x1 `eqFloat#` y1)
  (<?=) (Choice_C_Float i x y) z cs = narrow i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Float i xs) y cs = narrows cs i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_C_Float c x) y cs = guardCons c ((x <?= y) (addCs c cs))
  (<?=) Fail_C_Float _ _ = failCons
  (<?=) z (Choice_C_Float i x y) cs = narrow i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Float i xs) cs = narrows cs i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Float c x) cs = guardCons c ((y <?= x) (addCs c cs))
  (<?=) _ Fail_C_Float _ = failCons
  (<?=) (C_Float x1) (C_Float y1) _ = toCurry (x1 `leFloat#` y1)

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------
data C_Char
     = C_Char Char#
     | CurryChar BinInt
     | Choice_C_Char ID C_Char C_Char
     | Choices_C_Char ID ([C_Char])
     | Fail_C_Char
     | Guard_C_Char (Constraints) C_Char

instance Show C_Char where
  showsPrec d (Choice_C_Char i x y) = showsChoice d i x y
  showsPrec d (Choices_C_Char i xs) = showsChoices d i xs
  showsPrec d (Guard_C_Char c e) = showsGuard d c e
  showsPrec d Fail_C_Char = showChar '!'
  showsPrec d (C_Char x1) = showString (show (C# x1))
  showsPrec d (CurryChar x1) = case (const $## x1) emptyCs of
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
  match f _ _ _ _ _ (Choice_C_Char i x y) = f i x y
  match _ f _ _ _ _ (Choices_C_Char i@(NarrowedID _ _) xs) = f i xs
  match _ _ f _ _ _ (Choices_C_Char i@(FreeID _ _) xs) = f i xs
  match _ _ _ _ _ _ (Choices_C_Char i@(ChoiceID _) _) = error ("Prelude.Char.match: Choices with ChoiceID " ++ (show i))
  match _ _ _ f _ _ Fail_C_Char = f
  match _ _ _ _ f _ (Guard_C_Char cs e) = f cs e
  match _ _ _ _ _ f x = f x

instance Generable C_Char where
  generate s = Choices_C_Char (freeID [1] s) [CurryChar (generate (leftSupply s))]

instance NormalForm C_Char where
  ($!!) cont x@(C_Char _) cs = cont x cs
  ($!!) cont (CurryChar x) cs = ((cont . CurryChar) $!! x) cs
  ($!!) cont (Choice_C_Char i x y) cs = nfChoice cont i x y cs
  ($!!) cont (Choices_C_Char i xs) cs = nfChoices cont i xs cs
  ($!!) cont (Guard_C_Char c x) cs = guardCons c ((cont $!! x) (addCs c cs))
  ($!!) _ Fail_C_Char _ = failCons
  ($##) cont x@(C_Char _) cs = cont x cs
  ($##) cont (CurryChar x) cs = ((cont . CurryChar) $## x) cs
  ($##) cont (Choice_C_Char i x y) cs = gnfChoice cont i x y cs
  ($##) cont (Choices_C_Char i xs) cs = gnfChoices cont i xs cs
  ($##) cont (Guard_C_Char c x) cs = guardCons c ((cont $## x) (addCs c cs))
  ($##) _ Fail_C_Char _ = failCons
  ($!<) cont (CurryChar x)         =( cont . CurryChar) $!< x
  ($!<) cont (Choice_C_Char i x y) = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_Char i xs) = nfChoicesIO cont i xs
  ($!<) cont x = cont x
  searchNF search cont c@(C_Char _) = cont c
  searchNF search cont (CurryChar x) = search (cont . CurryChar) x
  searchNF _ _ x = error ("Prelude.Char.searchNF: no constructor: " ++ (show x))

instance Unifiable C_Char where
  (=.=) (C_Char       x1) (C_Char      x2) _ | x1 `eqChar#` x2 = C_Success
                                             | otherwise = Fail_C_Success
  (=.=) (C_Char       x1) (CurryChar x2)   cs = (primChar2CurryChar x1 =:= x2) cs
  (=.=) (CurryChar  x1) (C_Char      x2)   cs = (x1 =:= primChar2CurryChar x2) cs
  (=.=) (CurryChar x1)    (CurryChar   x2) cs = (x1 =:= x2) cs
  (=.=) _                 _                _  = Fail_C_Success
  (=.<=) (C_Char       x1) (C_Char      x2) _ | x1 `eqChar#` x2 = C_Success
                                              | otherwise = Fail_C_Success
  (=.<=) (C_Char       x1) (CurryChar x2)   cs = (primChar2CurryChar x1 =:<= x2) cs
  (=.<=) (CurryChar  x1) (C_Char      x2)   cs = (x1 =:<= primChar2CurryChar x2) cs
  (=.<=) (CurryChar x1)    (CurryChar   x2) cs = (x1 =:<= x2) cs
  (=.<=) _                 _                _  = Fail_C_Success
  bind i (C_Char    x) = (i :=: ChooseN 0 1) : bind (leftID i) (primChar2CurryChar x)
  bind i (CurryChar x) = (i :=: ChooseN 0 1) : bind (leftID i) x
  bind i (Choice_C_Char j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Char j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Char j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ c@(Choices_C_Char i@(ChoiceID _) _) = error ("Prelude.Char.bind: Choices with ChoiceID: " ++ (show c))
  bind _ Fail_C_Char = [Unsolvable]
  bind i (Guard_C_Char cs e) = getConstrList cs ++ (bind i e)
  lazyBind i (C_Char    x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) (primChar2CurryChar x))]
  lazyBind i (CurryChar x) = [i :=: ChooseN 0 1, leftID i :=: LazyBind (lazyBind (leftID i) x)]
  lazyBind i (Choice_C_Char j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Char j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Char j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ c@(Choices_C_Char i@(ChoiceID _) _) = error ("Prelude.Char.lazyBind: Choices with ChoiceID: " ++ (show c))
  lazyBind _ Fail_C_Char = [Unsolvable]
  lazyBind i (Guard_C_Char cs e) = getConstrList cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance Curry C_Char where
  (=?=) (Choice_C_Char i x y) z cs = narrow i ((x =?= z) cs) ((y =?= z) cs)
  (=?=) (Choices_C_Char i xs) y cs = narrows cs i (\x -> (x =?= y) cs) xs
  (=?=) (Guard_C_Char c x) y cs = guardCons c ((x =?= y) (addCs c cs))
  (=?=) Fail_C_Char _ _ = failCons
  (=?=) z (Choice_C_Char i x y) cs = narrow i ((z =?= x) cs) ((z =?= y) cs)
  (=?=) y (Choices_C_Char i xs) cs = narrows cs i (\x -> (y =?= x) cs) xs
  (=?=) y (Guard_C_Char c x) cs = guardCons c ((y =?= x) (addCs c cs))
  (=?=) _ Fail_C_Char _ = failCons
  (=?=) (C_Char x1) (C_Char y1) _ = toCurry (x1 `eqChar#` y1)
  (=?=) (C_Char      x1) (CurryChar y1) cs = ((primChar2CurryChar x1) =?= y1) cs
  (=?=) (CurryChar x1) (C_Char      y1) cs = (x1 =?= (primChar2CurryChar y1)) cs
  (=?=) (CurryChar x1) (CurryChar y1) cs = (x1 =?= y1) cs
  (<?=) (Choice_C_Char i x y) z cs = narrow i ((x <?= z) cs) ((y <?= z) cs)
  (<?=) (Choices_C_Char i xs) y cs = narrows cs i (\x -> (x <?= y) cs) xs
  (<?=) (Guard_C_Char c x) y cs = guardCons c ((x <?= y) (addCs c cs))
  (<?=) Fail_C_Char _ _ = failCons
  (<?=) z (Choice_C_Char i x y) cs = narrow i ((z <?= x) cs) ((z <?= y) cs)
  (<?=) y (Choices_C_Char i xs) cs = narrows cs i (\x -> (y <?= x) cs) xs
  (<?=) y (Guard_C_Char c x) cs = guardCons c ((y <?= x) (addCs c cs))
  (<?=) _ Fail_C_Char _ = failCons
  (<?=) (C_Char x1) (C_Char y1) _ = toCurry (x1 `leChar#` y1)
  (<?=) (C_Char      x1) (CurryChar y1) cs = ((primChar2CurryChar x1) `d_C_lteqInteger` y1) cs
  (<?=) (CurryChar x1) (C_Char      y1) cs = (x1 `d_C_lteqInteger` (primChar2CurryChar y1)) cs
  (<?=) (CurryChar x1) (CurryChar y1) cs = (x1 `d_C_lteqInteger` y1) cs


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

external_d_C_ensureNotFree :: Curry a => a -> ConstStore -> a
external_d_C_ensureNotFree x cs =
  case try x of
    Choice i a b  -> choiceCons i (external_d_C_ensureNotFree a cs)
                                 (external_d_C_ensureNotFree b cs)
    Narrowed i xs -> choicesCons i (map (flip external_d_C_ensureNotFree cs) xs)
    Free i xs     -> narrows cs i (flip external_d_C_ensureNotFree cs) xs
    Guard c e    -> guardCons c (external_d_C_ensureNotFree e (addCs c cs))
    _            -> x

external_d_C_prim_error :: C_String -> ConstStore -> a
external_d_C_prim_error s _ = error (fromCurry s)

external_d_C_failed :: NonDet a => ConstStore -> a
external_d_C_failed _ = failCons

external_d_OP_eq_eq :: Curry a => a -> a -> ConstStore -> C_Bool
external_d_OP_eq_eq  = (=?=)

external_d_OP_lt_eq :: Curry a => a -> a -> ConstStore -> C_Bool
external_d_OP_lt_eq = (<?=)


external_d_C_prim_ord :: C_Char -> ConstStore -> C_Int
external_d_C_prim_ord (C_Char c)    _ = C_Int (ord# c)
external_d_C_prim_ord (CurryChar c) _ = C_CurryInt c

external_d_C_prim_chr :: C_Int -> ConstStore -> C_Char
external_d_C_prim_chr (C_Int i)      _ = C_Char (chr# i)
external_d_C_prim_chr (C_CurryInt i) _ = CurryChar i

external_d_OP_plus :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_OP_plus (C_Int      x) (C_Int      y) _  = C_Int (x +# y)
external_d_OP_plus (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_OP_plus_hash` y) cs)
external_d_OP_plus (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_OP_plus_hash` (primint2curryint y)) cs)
external_d_OP_plus (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_OP_plus_hash` y) cs)
external_d_OP_plus x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_OP_plus` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_OP_minus :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_OP_minus (C_Int      x) (C_Int      y) _  = C_Int (x -# y)
external_d_OP_minus (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_OP_minus_hash` y) cs)
external_d_OP_minus (C_CurryInt x) (C_Int y)      cs = C_CurryInt ((x `d_OP_minus_hash` (primint2curryint y)) cs)
external_d_OP_minus (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_OP_minus_hash` y) cs)
external_d_OP_minus x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_OP_minus` b) cs2 )) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_OP_star :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_OP_star (C_Int      x) (C_Int      y) _  = C_Int (x *# y)
external_d_OP_star (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_OP_star_hash` y) cs)
external_d_OP_star (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_OP_star_hash` (primint2curryint y)) cs)
external_d_OP_star (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_OP_star_hash` y) cs)
external_d_OP_star x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_OP_star` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_C_quot :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_C_quot (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `quotInt#` y)
external_d_C_quot (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_C_quotInteger` y) cs)
external_d_C_quot (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_C_quotInteger` (primint2curryint y)) cs)
external_d_C_quot (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_C_quotInteger` y) cs)
external_d_C_quot x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_quot` b) cs2 )) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_C_rem :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_C_rem (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `remInt#` y)
external_d_C_rem (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_C_remInteger` y) cs)
external_d_C_rem (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_C_remInteger` (primint2curryint y)) cs)
external_d_C_rem (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_C_remInteger` y) cs)
external_d_C_rem x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_rem` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_C_quotRem :: C_Int -> C_Int -> ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_quotRem (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_OP_Tuple2
  | otherwise = OP_Tuple2 (C_Int (x `quotInt#` y)) (C_Int (x `remInt#` y))
external_d_C_quotRem (C_Int      x) (C_CurryInt y) cs = (mkIntTuple `d_dollar_bang` (((primint2curryint x) `d_C_quotRemInteger` y) cs)) cs
external_d_C_quotRem (C_CurryInt x) (C_Int      y) cs = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` (primint2curryint y)) cs)) cs
external_d_C_quotRem (C_CurryInt x) (C_CurryInt y) cs = (mkIntTuple `d_dollar_bang` ((x `d_C_quotRemInteger` y) cs)) cs
external_d_C_quotRem x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_quotRem` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

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

external_d_C_div :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_C_div (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `divInt#` y)
external_d_C_div (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_C_divInteger` y) cs)
external_d_C_div (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_C_divInteger` (primint2curryint y)) cs)
external_d_C_div (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_C_divInteger` y) cs)
external_d_C_div x y cs = ((\a cs1-> ((\b cs2-> ((a `external_d_C_div` b) cs2)) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

external_d_C_mod :: C_Int -> C_Int -> ConstStore -> C_Int
external_d_C_mod (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_C_Int
  | otherwise = C_Int (x `modInt#` y)
external_d_C_mod (C_Int      x) (C_CurryInt y) cs = C_CurryInt (((primint2curryint x) `d_C_modInteger` y) cs)
external_d_C_mod (C_CurryInt x) (C_Int      y) cs = C_CurryInt ((x `d_C_modInteger` (primint2curryint y)) cs)
external_d_C_mod (C_CurryInt x) (C_CurryInt y) cs = C_CurryInt ((x `d_C_modInteger` y) cs)
external_d_C_mod x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_mod` b)) cs2) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

-- TODO: $! instead of $#?
external_d_C_divMod :: C_Int -> C_Int ->  ConstStore -> OP_Tuple2 C_Int C_Int
external_d_C_divMod (C_Int      x) (C_Int      y) _
  | y ==# 0#  = Fail_OP_Tuple2
  | otherwise = OP_Tuple2 (C_Int (x `divInt#` y)) (C_Int (x `modInt#` y))
external_d_C_divMod (C_Int      x) (C_CurryInt y) cs = (mkIntTuple `d_OP_dollar_hash` (((primint2curryint x) `d_C_divModInteger` y) cs)) cs
external_d_C_divMod (C_CurryInt x) (C_Int      y) cs = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` (primint2curryint y)) cs)) cs
external_d_C_divMod (C_CurryInt x) (C_CurryInt y) cs = (mkIntTuple `d_OP_dollar_hash` ((x `d_C_divModInteger` y) cs)) cs
external_d_C_divMod x y cs = ((\a cs1 -> ((\b cs2 -> ((a `external_d_C_divMod` b) cs2 )) `d_OP_dollar_hash` y) cs1) `d_OP_dollar_hash` x) cs

mkIntTuple :: OP_Tuple2 BinInt BinInt -> ConstStore -> OP_Tuple2 C_Int C_Int
mkIntTuple (OP_Tuple2 d m) _ = OP_Tuple2 (C_CurryInt d) (C_CurryInt m)

external_d_C_negateFloat :: C_Float -> ConstStore -> C_Float
external_d_C_negateFloat (C_Float x) _ = C_Float (negateFloat# x)
external_d_C_negateFloat x cs          = (external_d_C_negateFloat `d_OP_dollar_hash` x) cs

external_d_OP_eq_colon_eq :: Unifiable a => a -> a -> ConstStore -> C_Success
external_d_OP_eq_colon_eq = (=:=)

external_d_C_success :: ConstStore -> C_Success
external_d_C_success _ = C_Success

external_d_OP_ampersand :: C_Success -> C_Success -> ConstStore -> C_Success
external_d_OP_ampersand = (&)

external_d_C_return :: a -> ConstStore -> C_IO a
external_d_C_return a _ = fromIO (return a)

external_d_C_prim_putChar :: C_Char -> ConstStore -> C_IO OP_Unit
external_d_C_prim_putChar c _ = fromHaskellIO1 putChar c

external_d_C_getChar :: ConstStore -> C_IO C_Char
external_d_C_getChar _ = fromHaskellIO0 getChar

external_d_C_prim_readFile :: C_String -> ConstStore -> C_IO C_String
external_d_C_prim_readFile s cs = fromHaskellIO1 readFile s

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_writeFile :: C_String -> C_String -> ConstStore -> C_IO OP_Unit
external_d_C_prim_writeFile s1 s2 _ = fromHaskellIO2 writeFile s1 s2

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_appendFile :: C_String -> C_String -> ConstStore -> C_IO OP_Unit
external_d_C_prim_appendFile s1 s2 _ = fromHaskellIO2 appendFile s1 s2

external_d_C_catchFail :: C_IO a -> C_IO a -> ConstStore -> C_IO a
external_d_C_catchFail act err cs = fromIO $ C.catch (toIOWithFailCheck act) handle
  where handle e = hPutStrLn stderr (show (e :: C.SomeException)) >> (toIO err cs)
        toIOWithFailCheck act =
          case act of Fail_C_IO -> ioError (userError "I/O action failed")
                      _         -> toIO act cs

external_d_C_prim_show :: Show a => a -> ConstStore -> C_String
external_d_C_prim_show a _ = toCurry (show a)

external_d_C_cond :: Curry a => C_Success -> a -> ConstStore -> a
external_d_C_cond succ a cs = ((\_ _ -> a) `d_OP_dollar_hash` succ) cs

external_d_OP_eq_colon_lt_eq :: Curry a => a -> a -> ConstStore -> C_Success
external_d_OP_eq_colon_lt_eq = (=:<=)

external_d_OP_eq_colon_lt_lt_eq :: Curry a => a -> a -> ConstStore -> C_Success
external_d_OP_eq_colon_lt_lt_eq = error "external_d_OP_eq_colon_lt_lt_eq"

-- External ND
-- -----------

external_nd_OP_qmark :: NonDet a => a -> a -> IDSupply -> ConstStore -> a
external_nd_OP_qmark x y ids _ = let i = thisID ids in i `seq` choiceCons i x y

-- External HO
-- -----------

external_d_OP_dollar_bang :: (NonDet a, NonDet b) => (a -> ConstStore -> b) -> a -> ConstStore -> b
external_d_OP_dollar_bang = d_dollar_bang

external_nd_OP_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> ConstStore -> b
external_nd_OP_dollar_bang = nd_dollar_bang

external_d_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => (a -> ConstStore -> b) -> a -> ConstStore -> b
external_d_OP_dollar_bang_bang = ($!!)

external_nd_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> ConstStore -> b
external_nd_OP_dollar_bang_bang f x s cs = ((\y cs1-> nd_apply f y s cs1) $!! x) cs

external_d_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => (a -> ConstStore -> b) -> a -> ConstStore -> b
external_d_OP_dollar_hash_hash = ($##)

external_nd_OP_dollar_hash_hash :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> ConstStore -> b
external_nd_OP_dollar_hash_hash f x s cs = ((\y cs1 -> nd_apply f y s cs1) $## x) cs

external_d_C_apply :: (a -> ConstStore -> b) -> a -> ConstStore -> b
external_d_C_apply = d_apply

external_nd_C_apply :: NonDet b => Func a b -> a -> IDSupply -> ConstStore -> b
external_nd_C_apply = nd_apply

external_d_C_catch :: C_IO a -> (C_IOError -> ConstStore -> C_IO a) -> ConstStore -> C_IO a
external_d_C_catch act cont cs = fromIO $ C.catch (toIO act cs) handle where
  handle e = toIO  (cont  (C_IOError  (toCurry  (show (e :: C.SomeException)))) cs) cs

external_nd_C_catch :: C_IO a -> Func C_IOError (C_IO a) -> IDSupply -> ConstStore -> C_IO a
external_nd_C_catch act cont s cs = fromIO $ C.catch (toIO act cs) handle where
  handle e = toIO  (nd_apply cont (C_IOError $ toCurry $ show (e :: C.SomeException)) s cs) cs

external_d_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> (t0 -> ConstStore -> C_IO t1) -> ConstStore -> C_IO t1
external_d_OP_gt_gt_eq m f cs = fromIO $ do
  x <- toIO m cs 
  cs1 <- lookupGlobalCs 
  let cs2 = combineCs cs cs1
  toIO  (f x cs2) cs2

external_nd_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> Func t0 (C_IO t1) -> IDSupply -> ConstStore -> C_IO t1
external_nd_OP_gt_gt_eq m f s cs = fromIO $ do
 x <- toIO m cs
 cs1 <- lookupGlobalCs 
 let cs2 = combineCs cs cs1 
 toIO (nd_apply f x s cs2) cs2

-- Encapsulated search
-- -------------------

-- external_d_C_try :: (a -> Success) -> [a -> Success]
external_d_C_try = error "external_dho_C_try"

-- external_nd_C_try :: Func a Success -> [Func a Success]
external_nd_C_try = error "external_ndho_C_try"





-- Functions on Integer and Nat added from PrimTypes
-- -------------------------------------------------

instance Curry_Prelude.Curry Nat where
  (=?=) (Choice_Nat i x y) z cs = narrow i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_Nat i xs) y cs = narrows cs i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_Nat c e) y cs = guardCons c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) Fail_Nat _ _ = failCons
  (=?=) z (Choice_Nat i x y) cs = narrow i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_Nat i xs) cs = narrows cs i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_Nat c e) cs = guardCons c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ Fail_Nat _ = failCons
  (=?=) IHi IHi cs = Curry_Prelude.C_True
  (=?=) (O x1) (O y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) (I x1) (I y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_Nat i x y) z cs = narrow i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_Nat i xs) y cs = narrows cs i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_Nat c e) y cs = guardCons c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) Fail_Nat _ _ = failCons
  (<?=) z (Choice_Nat i x y) cs = narrow i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_Nat i xs) cs = narrows cs i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_Nat c e) cs = guardCons c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ Fail_Nat _ = failCons
  (<?=) IHi IHi cs = Curry_Prelude.C_True
  (<?=) IHi (O _) _ = Curry_Prelude.C_True
  (<?=) IHi (I _) _ = Curry_Prelude.C_True
  (<?=) (O x1) (O y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (O _) (I _) _ = Curry_Prelude.C_True
  (<?=) (I x1) (I y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False

instance Curry_Prelude.Curry BinInt where
  (=?=) (Choice_BinInt i x y) z cs = narrow i ((x Curry_Prelude.=?= z) cs) ((y Curry_Prelude.=?= z) cs)
  (=?=) (Choices_BinInt i xs) y cs = narrows cs i (\x -> (x Curry_Prelude.=?= y) cs) xs
  (=?=) (Guard_BinInt c e) y cs = guardCons c ((e Curry_Prelude.=?= y) (addCs c cs))
  (=?=) Fail_BinInt _ _ = failCons
  (=?=) z (Choice_BinInt i x y) cs = narrow i ((z Curry_Prelude.=?= x) cs) ((z Curry_Prelude.=?= y) cs)
  (=?=) y (Choices_BinInt i xs) cs = narrows cs i (\x -> (y Curry_Prelude.=?= x) cs) xs
  (=?=) y (Guard_BinInt c e) cs = guardCons c ((y Curry_Prelude.=?= e) (addCs c cs))
  (=?=) _ Fail_BinInt _ = failCons
  (=?=) (Neg x1) (Neg y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) Zero Zero cs = Curry_Prelude.C_True
  (=?=) (Pos x1) (Pos y1) cs = (x1 Curry_Prelude.=?= y1) cs
  (=?=) _ _ _ = Curry_Prelude.C_False
  (<?=) (Choice_BinInt i x y) z cs = narrow i ((x Curry_Prelude.<?= z) cs) ((y Curry_Prelude.<?= z) cs)
  (<?=) (Choices_BinInt i xs) y cs = narrows cs i (\x -> (x Curry_Prelude.<?= y) cs) xs
  (<?=) (Guard_BinInt c e) y cs = guardCons c ((e Curry_Prelude.<?= y) (addCs c cs))
  (<?=) Fail_BinInt _ _ = failCons
  (<?=) z (Choice_BinInt i x y) cs = narrow i ((z Curry_Prelude.<?= x) cs) ((z Curry_Prelude.<?= y) cs)
  (<?=) y (Choices_BinInt i xs) cs = narrows cs i (\x -> (y Curry_Prelude.<?= x) cs) xs
  (<?=) y (Guard_BinInt c e) cs = guardCons c ((y Curry_Prelude.<?= e) (addCs c cs))
  (<?=) _ Fail_BinInt _ = failCons
  (<?=) (Neg x1) (Neg y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) (Neg _) Zero _ = Curry_Prelude.C_True
  (<?=) (Neg _) (Pos _) _ = Curry_Prelude.C_True
  (<?=) Zero Zero cs = Curry_Prelude.C_True
  (<?=) Zero (Pos _) _ = Curry_Prelude.C_True
  (<?=) (Pos x1) (Pos y1) cs = (x1 Curry_Prelude.<?= y1) cs
  (<?=) _ _ _ = Curry_Prelude.C_False



d_C_cmpNat :: Nat -> Nat -> ConstStore -> Curry_Prelude.C_Ordering
d_C_cmpNat x1 x2 x3500 = case x1 of
     IHi -> d_OP__casePT_33 x2 x3500
     (O x5) -> d_OP__casePT_32 x5 x2 x3500
     (I x8) -> d_OP__casePT_30 x8 x2 x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_cmpNat x1001 x2 x3500) (d_C_cmpNat x1002 x2 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_cmpNat z x2 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_cmpNat x1001 x2 (addCs x1000 x3500))
     _ -> failCons

d_C_succ :: Nat -> ConstStore -> Nat
d_C_succ x1 x3500 = case x1 of
     IHi -> O IHi
     (O x2) -> I x2
     (I x3) -> O (d_C_succ x3 x3500)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_succ x1001 x3500) (d_C_succ x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_succ z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_succ x1001 (addCs x1000 x3500))
     _ -> failCons

d_C_pred :: Nat -> ConstStore -> Nat
d_C_pred x1 x3500 = case x1 of
     IHi -> Curry_Prelude.d_C_failed x3500
     (O x2) -> d_OP__casePT_28 x2 x3500
     (I x5) -> O x5
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_pred x1001 x3500) (d_C_pred x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_pred z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_pred x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_plus_caret :: Nat -> Nat -> ConstStore -> Nat
d_OP_plus_caret x1 x2 x3500 = case x1 of
     IHi -> d_C_succ x2 x3500
     (O x3) -> d_OP__casePT_27 x3 x2 x3500
     (I x6) -> d_OP__casePT_26 x6 x2 x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP_plus_caret x1001 x2 x3500) (d_OP_plus_caret x1002 x2 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_plus_caret z x2 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP_plus_caret x1001 x2 (addCs x1000 x3500))
     _ -> failCons

d_OP_minus_caret :: Nat -> Nat -> ConstStore -> BinInt
d_OP_minus_caret x1 x2 x3500 = case x1 of
     IHi -> d_C_inc (Neg x2) x3500
     (O x3) -> d_OP__casePT_25 x1 x3 x2 x3500
     (I x6) -> d_OP__casePT_24 x6 x2 x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP_minus_caret x1001 x2 x3500) (d_OP_minus_caret x1002 x2 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_minus_caret z x2 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP_minus_caret x1001 x2 (addCs x1000 x3500))
     _ -> failCons

d_C_mult2 :: BinInt -> ConstStore -> BinInt
d_C_mult2 x1 x3500 = case x1 of
     (Pos x2) -> Pos (O x2)
     Zero -> Zero
     (Neg x3) -> Neg (O x3)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_mult2 x1001 x3500) (d_C_mult2 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_mult2 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_mult2 x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_star_caret :: Nat -> Nat -> ConstStore -> Nat
d_OP_star_caret x1 x2 x3500 = case x1 of
     IHi -> x2
     (O x3) -> O (d_OP_star_caret x3 x2 x3500)
     (I x4) -> d_OP_plus_caret x2 (O (d_OP_star_caret x4 x2 x3500)) x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP_star_caret x1001 x2 x3500) (d_OP_star_caret x1002 x2 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_star_caret z x2 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP_star_caret x1001 x2 (addCs x1000 x3500))
     _ -> failCons

d_C_div2 :: Nat -> ConstStore -> Nat
d_C_div2 x1 x3500 = case x1 of
     IHi -> Curry_Prelude.d_C_failed x3500
     (O x2) -> x2
     (I x3) -> x3
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_div2 x1001 x3500) (d_C_div2 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_div2 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_div2 x1001 (addCs x1000 x3500))
     _ -> failCons

d_C_mod2 :: Nat -> ConstStore -> BinInt
d_C_mod2 x1 x3500 = case x1 of
     IHi -> Pos IHi
     (O x2) -> Zero
     (I x3) -> Pos IHi
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_C_mod2 x1001 x3500) (d_C_mod2 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_mod2 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_C_mod2 x1001 (addCs x1000 x3500))
     _ -> failCons

d_C_quotRemNat :: Nat -> Nat -> ConstStore -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_quotRemNat x1 x2 x3500 = d_OP__casePT_23 x1 x2 (Curry_Prelude.d_OP_eq_eq x2 IHi x3500) x3500

d_OP_quotRemNat_dot_shift_dot_104 :: Nat -> Nat -> ConstStore -> Nat
d_OP_quotRemNat_dot_shift_dot_104 x1 x2 x3500 = case x1 of
     (O x3) -> O x2
     (I x4) -> I x2
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemNat_dot_shift_dot_104 x1001 x2 x3500) (d_OP_quotRemNat_dot_shift_dot_104 x1002 x2 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_quotRemNat_dot_shift_dot_104 z x2 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP_quotRemNat_dot_shift_dot_104 x1001 x2 (addCs x1000 x3500))
     _ -> failCons

d_C_lteqInteger :: BinInt -> BinInt -> ConstStore -> Curry_Prelude.C_Bool
d_C_lteqInteger x1 x2 x3500 = Curry_Prelude.d_OP_slash_eq (d_C_cmpInteger x1 x2 x3500) Curry_Prelude.C_GT x3500

d_C_cmpInteger :: BinInt -> BinInt -> ConstStore -> Curry_Prelude.C_Ordering
d_C_cmpInteger x1 x2 x3500 = case x1 of
     Zero -> d_OP__casePT_14 x2 x3500
     (Pos x5) -> d_OP__casePT_13 x5 x2 x3500
     (Neg x8) -> d_OP__casePT_12 x8 x2 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_cmpInteger x1001 x2 x3500) (d_C_cmpInteger x1002 x2 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_cmpInteger z x2 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_cmpInteger x1001 x2 (addCs x1000 x3500))
     _ -> failCons

d_C_neg :: BinInt -> ConstStore -> BinInt
d_C_neg x1 x3500 = case x1 of
     Zero -> Zero
     (Pos x2) -> Neg x2
     (Neg x3) -> Pos x3
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_neg x1001 x3500) (d_C_neg x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_neg z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_neg x1001 (addCs x1000 x3500))
     _ -> failCons

d_C_inc :: BinInt -> ConstStore -> BinInt
d_C_inc x1 x3500 = case x1 of
     Zero -> Pos IHi
     (Pos x2) -> Pos (d_C_succ x2 x3500)
     (Neg x3) -> d_OP__casePT_11 x3 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_inc x1001 x3500) (d_C_inc x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_inc z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_inc x1001 (addCs x1000 x3500))
     _ -> failCons

d_C_dec :: BinInt -> ConstStore -> BinInt
d_C_dec x1 x3500 = case x1 of
     Zero -> Neg IHi
     (Pos x2) -> d_OP__casePT_10 x2 x3500
     (Neg x5) -> Neg (d_C_succ x5 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_dec x1001 x3500) (d_C_dec x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_dec z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_dec x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_plus_hash :: BinInt -> BinInt -> ConstStore -> BinInt
d_OP_plus_hash x1 x2 x3500 = case x1 of
     Zero -> x2
     (Pos x3) -> d_OP__casePT_9 x1 x3 x2 x3500
     (Neg x6) -> d_OP__casePT_8 x1 x6 x2 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP_plus_hash x1001 x2 x3500) (d_OP_plus_hash x1002 x2 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_plus_hash z x2 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP_plus_hash x1001 x2 (addCs x1000 x3500))
     _ -> failCons

d_OP_minus_hash :: BinInt -> BinInt -> ConstStore -> BinInt
d_OP_minus_hash x1 x2 x3500 = case x2 of
     Zero -> x1
     (Pos x3) -> d_OP_plus_hash x1 (Neg x3) x3500
     (Neg x4) -> d_OP_plus_hash x1 (Pos x4) x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP_minus_hash x1 x1001 x3500) (d_OP_minus_hash x1 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_minus_hash x1 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP_minus_hash x1 x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_star_hash :: BinInt -> BinInt -> ConstStore -> BinInt
d_OP_star_hash x1 x2 x3500 = case x1 of
     Zero -> Zero
     (Pos x3) -> d_OP__casePT_7 x3 x2 x3500
     (Neg x6) -> d_OP__casePT_6 x6 x2 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP_star_hash x1001 x2 x3500) (d_OP_star_hash x1002 x2 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_star_hash z x2 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP_star_hash x1001 x2 (addCs x1000 x3500))
     _ -> failCons

d_C_quotRemInteger :: BinInt -> BinInt -> ConstStore -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_quotRemInteger x1 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.d_C_failed x3500
     (Pos x3) -> d_OP__casePT_5 x3 x1 x3500
     (Neg x9) -> d_OP__casePT_4 x9 x1 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_quotRemInteger x1 x1001 x3500) (d_C_quotRemInteger x1 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_quotRemInteger x1 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_quotRemInteger x1 x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP2_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1001 x3500) (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_quotRemInteger_dot___hash_selFP2_hash_d z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP2_hash_d x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP3_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1001 x3500) (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_quotRemInteger_dot___hash_selFP3_hash_m z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP3_hash_m x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP5_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1001 x3500) (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_quotRemInteger_dot___hash_selFP5_hash_d z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP5_hash_d x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP6_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1001 x3500) (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_quotRemInteger_dot___hash_selFP6_hash_m z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP6_hash_m x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP8_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1001 x3500) (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_quotRemInteger_dot___hash_selFP8_hash_d z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP8_hash_d x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_quotRemInteger_dot___hash_selFP9_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1001 x3500) (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_quotRemInteger_dot___hash_selFP9_hash_m z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_quotRemInteger_dot___hash_selFP9_hash_m x1001 (addCs x1000 x3500))
     _ -> failCons

d_C_divModInteger :: BinInt -> BinInt -> ConstStore -> Curry_Prelude.OP_Tuple2 BinInt BinInt
d_C_divModInteger x1 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.d_C_failed x3500
     (Pos x3) -> d_OP__casePT_3 x3 x1 x3500
     (Neg x11) -> d_OP__casePT_1 x11 x1 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_C_divModInteger x1 x1001 x3500) (d_C_divModInteger x1 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_C_divModInteger x1 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_C_divModInteger x1 x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP11_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP11_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP11_hash_d x1001 x3500) (d_OP_divModInteger_dot___hash_selFP11_hash_d x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_divModInteger_dot___hash_selFP11_hash_d z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP11_hash_d x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP12_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP12_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP12_hash_m x1001 x3500) (d_OP_divModInteger_dot___hash_selFP12_hash_m x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_divModInteger_dot___hash_selFP12_hash_m z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP12_hash_m x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP14_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP14_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP14_hash_d x1001 x3500) (d_OP_divModInteger_dot___hash_selFP14_hash_d x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_divModInteger_dot___hash_selFP14_hash_d z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP14_hash_d x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP15_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP15_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP15_hash_m x1001 x3500) (d_OP_divModInteger_dot___hash_selFP15_hash_m x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_divModInteger_dot___hash_selFP15_hash_m z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP15_hash_m x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP17_hash_d :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP17_hash_d x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x2
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP17_hash_d x1001 x3500) (d_OP_divModInteger_dot___hash_selFP17_hash_d x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_divModInteger_dot___hash_selFP17_hash_d z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP17_hash_d x1001 (addCs x1000 x3500))
     _ -> failCons

d_OP_divModInteger_dot___hash_selFP18_hash_m :: Curry_Prelude.OP_Tuple2 BinInt BinInt -> ConstStore -> BinInt
d_OP_divModInteger_dot___hash_selFP18_hash_m x1 x3500 = case x1 of
     (Curry_Prelude.OP_Tuple2 x2 x3) -> x3
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP_divModInteger_dot___hash_selFP18_hash_m x1001 x3500) (d_OP_divModInteger_dot___hash_selFP18_hash_m x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP_divModInteger_dot___hash_selFP18_hash_m z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP_divModInteger_dot___hash_selFP18_hash_m x1001 (addCs x1000 x3500))
     _ -> failCons

d_C_divInteger :: BinInt -> BinInt -> ConstStore -> BinInt
d_C_divInteger x1 x2 x3500 = Curry_Prelude.d_C_fst (d_C_divModInteger x1 x2 x3500) x3500

d_C_modInteger :: BinInt -> BinInt -> ConstStore -> BinInt
d_C_modInteger x1 x2 x3500 = Curry_Prelude.d_C_snd (d_C_divModInteger x1 x2 x3500) x3500

d_C_quotInteger :: BinInt -> BinInt -> ConstStore -> BinInt
d_C_quotInteger x1 x2 x3500 = Curry_Prelude.d_C_fst (d_C_quotRemInteger x1 x2 x3500) x3500

d_C_remInteger :: BinInt -> BinInt -> ConstStore -> BinInt
d_C_remInteger x1 x2 x3500 = Curry_Prelude.d_C_snd (d_C_quotRemInteger x1 x2 x3500) x3500

d_OP__casePT_1 x11 x1 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x12) -> let
          x13 = d_C_quotRemNat x12 x11 x3500
          x14 = d_OP_divModInteger_dot___hash_selFP14_hash_d x13 x3500
          x15 = d_OP_divModInteger_dot___hash_selFP15_hash_m x13 x3500
           in (d_OP__casePT_0 x11 x14 x15 x3500)
     (Neg x18) -> let
          x19 = d_C_quotRemNat x18 x11 x3500
          x20 = d_OP_divModInteger_dot___hash_selFP17_hash_d x19 x3500
          x21 = d_OP_divModInteger_dot___hash_selFP18_hash_m x19 x3500
           in (Curry_Prelude.OP_Tuple2 x20 (d_C_neg x21 x3500))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_1 x11 x1001 x3500) (d_OP__casePT_1 x11 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_1 x11 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_1 x11 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_1 x11 x1 x3000 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x12) -> let
          x2000 = x3000
           in (seq x2000 (let
               x13 = d_C_quotRemNat x12 x11 x3500
               x14 = d_OP_divModInteger_dot___hash_selFP14_hash_d x13 x3500
               x15 = d_OP_divModInteger_dot___hash_selFP15_hash_m x13 x3500
                in (nd_OP__casePT_0 x11 x14 x15 x2000 x3500)))
     (Neg x18) -> let
          x19 = d_C_quotRemNat x18 x11 x3500
          x20 = d_OP_divModInteger_dot___hash_selFP17_hash_d x19 x3500
          x21 = d_OP_divModInteger_dot___hash_selFP18_hash_m x19 x3500
           in (Curry_Prelude.OP_Tuple2 x20 (d_C_neg x21 x3500))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_1 x11 x1001 x3000 x3500) (nd_OP__casePT_1 x11 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_1 x11 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_1 x11 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_0 x11 x14 x15 x3500 = case x15 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x14 x3500) x15
     (Neg x16) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3500) x3500) (d_OP_minus_hash x15 (Pos x11) x3500)
     (Pos x17) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3500) x3500) (d_OP_minus_hash x15 (Pos x11) x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_0 x11 x14 x1001 x3500) (d_OP__casePT_0 x11 x14 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_0 x11 x14 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_0 x11 x14 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_0 x11 x14 x15 x3000 x3500 = case x15 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x14 x3500) x15
     (Neg x16) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3500) x3500) (d_OP_minus_hash x15 (Pos x11) x3500)
     (Pos x17) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x14 x3500) x3500) (d_OP_minus_hash x15 (Pos x11) x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_0 x11 x14 x1001 x3000 x3500) (nd_OP__casePT_0 x11 x14 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_0 x11 x14 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_0 x11 x14 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_3 x3 x1 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3500
          x7 = d_OP_divModInteger_dot___hash_selFP11_hash_d x6 x3500
          x8 = d_OP_divModInteger_dot___hash_selFP12_hash_m x6 x3500
           in (d_OP__casePT_2 x3 x7 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_3 x3 x1001 x3500) (d_OP__casePT_3 x3 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_3 x3 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_3 x3 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_3 x3 x1 x3000 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3500
     (Neg x5) -> let
          x2000 = x3000
           in (seq x2000 (let
               x6 = d_C_quotRemNat x5 x3 x3500
               x7 = d_OP_divModInteger_dot___hash_selFP11_hash_d x6 x3500
               x8 = d_OP_divModInteger_dot___hash_selFP12_hash_m x6 x3500
                in (nd_OP__casePT_2 x3 x7 x8 x2000 x3500)))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_3 x3 x1001 x3000 x3500) (nd_OP__casePT_3 x3 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_3 x3 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_3 x3 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_2 x3 x7 x8 x3500 = case x8 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3500) x8
     (Neg x9) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3500) x3500) (d_OP_minus_hash (Pos x3) x8 x3500)
     (Pos x10) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3500) x3500) (d_OP_minus_hash (Pos x3) x8 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_2 x3 x7 x1001 x3500) (d_OP__casePT_2 x3 x7 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_2 x3 x7 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_2 x3 x7 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_2 x3 x7 x8 x3000 x3500 = case x8 of
     Zero -> Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3500) x8
     (Neg x9) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3500) x3500) (d_OP_minus_hash (Pos x3) x8 x3500)
     (Pos x10) -> Curry_Prelude.OP_Tuple2 (d_C_neg (d_C_inc x7 x3500) x3500) (d_OP_minus_hash (Pos x3) x8 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_2 x3 x7 x1001 x3000 x3500) (nd_OP__casePT_2 x3 x7 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_2 x3 x7 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_2 x3 x7 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_4 x9 x1 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x10) -> let
          x11 = d_C_quotRemNat x10 x9 x3500
          x12 = d_OP_quotRemInteger_dot___hash_selFP5_hash_d x11 x3500
          x13 = d_OP_quotRemInteger_dot___hash_selFP6_hash_m x11 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x12 x3500) x13)
     (Neg x14) -> let
          x15 = d_C_quotRemNat x14 x9 x3500
          x16 = d_OP_quotRemInteger_dot___hash_selFP8_hash_d x15 x3500
          x17 = d_OP_quotRemInteger_dot___hash_selFP9_hash_m x15 x3500
           in (Curry_Prelude.OP_Tuple2 x16 (d_C_neg x17 x3500))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_4 x9 x1001 x3500) (d_OP__casePT_4 x9 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_4 x9 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_4 x9 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_4 x9 x1 x3000 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x10) -> let
          x11 = d_C_quotRemNat x10 x9 x3500
          x12 = d_OP_quotRemInteger_dot___hash_selFP5_hash_d x11 x3500
          x13 = d_OP_quotRemInteger_dot___hash_selFP6_hash_m x11 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x12 x3500) x13)
     (Neg x14) -> let
          x15 = d_C_quotRemNat x14 x9 x3500
          x16 = d_OP_quotRemInteger_dot___hash_selFP8_hash_d x15 x3500
          x17 = d_OP_quotRemInteger_dot___hash_selFP9_hash_m x15 x3500
           in (Curry_Prelude.OP_Tuple2 x16 (d_C_neg x17 x3500))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_4 x9 x1001 x3000 x3500) (nd_OP__casePT_4 x9 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_4 x9 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_4 x9 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_5 x3 x1 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3500
          x7 = d_OP_quotRemInteger_dot___hash_selFP2_hash_d x6 x3500
          x8 = d_OP_quotRemInteger_dot___hash_selFP3_hash_m x6 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3500) (d_C_neg x8 x3500))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_5 x3 x1001 x3500) (d_OP__casePT_5 x3 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_5 x3 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_5 x3 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_5 x3 x1 x3000 x3500 = case x1 of
     Zero -> Curry_Prelude.OP_Tuple2 Zero Zero
     (Pos x4) -> d_C_quotRemNat x4 x3 x3500
     (Neg x5) -> let
          x6 = d_C_quotRemNat x5 x3 x3500
          x7 = d_OP_quotRemInteger_dot___hash_selFP2_hash_d x6 x3500
          x8 = d_OP_quotRemInteger_dot___hash_selFP3_hash_m x6 x3500
           in (Curry_Prelude.OP_Tuple2 (d_C_neg x7 x3500) (d_C_neg x8 x3500))
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_5 x3 x1001 x3000 x3500) (nd_OP__casePT_5 x3 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_5 x3 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_5 x3 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_6 x6 x2 x3500 = case x2 of
     Zero -> Zero
     (Pos x7) -> Neg (d_OP_star_caret x6 x7 x3500)
     (Neg x8) -> Pos (d_OP_star_caret x6 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_6 x6 x1001 x3500) (d_OP__casePT_6 x6 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_6 x6 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_6 x6 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_6 x6 x2 x3000 x3500 = case x2 of
     Zero -> Zero
     (Pos x7) -> Neg (d_OP_star_caret x6 x7 x3500)
     (Neg x8) -> Pos (d_OP_star_caret x6 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_6 x6 x1001 x3000 x3500) (nd_OP__casePT_6 x6 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_6 x6 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_6 x6 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_7 x3 x2 x3500 = case x2 of
     Zero -> Zero
     (Pos x4) -> Pos (d_OP_star_caret x3 x4 x3500)
     (Neg x5) -> Neg (d_OP_star_caret x3 x5 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_7 x3 x1001 x3500) (d_OP__casePT_7 x3 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_7 x3 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_7 x3 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_7 x3 x2 x3000 x3500 = case x2 of
     Zero -> Zero
     (Pos x4) -> Pos (d_OP_star_caret x3 x4 x3500)
     (Neg x5) -> Neg (d_OP_star_caret x3 x5 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_7 x3 x1001 x3000 x3500) (nd_OP__casePT_7 x3 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_7 x3 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_7 x3 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_8 x1 x6 x2 x3500 = case x2 of
     Zero -> x1
     (Pos x7) -> d_OP_minus_caret x7 x6 x3500
     (Neg x8) -> Neg (d_OP_plus_caret x6 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_8 x1 x6 x1001 x3500) (d_OP__casePT_8 x1 x6 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_8 x1 x6 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_8 x1 x6 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_8 x1 x6 x2 x3000 x3500 = case x2 of
     Zero -> x1
     (Pos x7) -> d_OP_minus_caret x7 x6 x3500
     (Neg x8) -> Neg (d_OP_plus_caret x6 x8 x3500)
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_8 x1 x6 x1001 x3000 x3500) (nd_OP__casePT_8 x1 x6 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_8 x1 x6 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_8 x1 x6 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_9 x1 x3 x2 x3500 = case x2 of
     Zero -> x1
     (Pos x4) -> Pos (d_OP_plus_caret x3 x4 x3500)
     (Neg x5) -> d_OP_minus_caret x3 x5 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_9 x1 x3 x1001 x3500) (d_OP__casePT_9 x1 x3 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_9 x1 x3 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_9 x1 x3 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_9 x1 x3 x2 x3000 x3500 = case x2 of
     Zero -> x1
     (Pos x4) -> Pos (d_OP_plus_caret x3 x4 x3500)
     (Neg x5) -> d_OP_minus_caret x3 x5 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_9 x1 x3 x1001 x3000 x3500) (nd_OP__casePT_9 x1 x3 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_9 x1 x3 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_9 x1 x3 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_10 x2 x3500 = case x2 of
     IHi -> Zero
     (O x3) -> Pos (d_C_pred (O x3) x3500)
     (I x4) -> Pos (O x4)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_10 x1001 x3500) (d_OP__casePT_10 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_10 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_10 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_10 x2 x3000 x3500 = case x2 of
     IHi -> Zero
     (O x3) -> Pos (d_C_pred (O x3) x3500)
     (I x4) -> Pos (O x4)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_10 x1001 x3000 x3500) (nd_OP__casePT_10 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_10 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_10 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_11 x3 x3500 = case x3 of
     IHi -> Zero
     (O x4) -> Neg (d_C_pred (O x4) x3500)
     (I x5) -> Neg (O x5)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_11 x1001 x3500) (d_OP__casePT_11 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_11 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_11 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_11 x3 x3000 x3500 = case x3 of
     IHi -> Zero
     (O x4) -> Neg (d_C_pred (O x4) x3500)
     (I x5) -> Neg (O x5)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_11 x1001 x3000 x3500) (nd_OP__casePT_11 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_11 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_11 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_12 x8 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.C_LT
     (Pos x9) -> Curry_Prelude.C_LT
     (Neg x10) -> d_C_cmpNat x10 x8 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_12 x8 x1001 x3500) (d_OP__casePT_12 x8 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_12 x8 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_12 x8 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_12 x8 x2 x3000 x3500 = case x2 of
     Zero -> Curry_Prelude.C_LT
     (Pos x9) -> Curry_Prelude.C_LT
     (Neg x10) -> d_C_cmpNat x10 x8 x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_12 x8 x1001 x3000 x3500) (nd_OP__casePT_12 x8 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_12 x8 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_12 x8 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_13 x5 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.C_GT
     (Pos x6) -> d_C_cmpNat x5 x6 x3500
     (Neg x7) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_13 x5 x1001 x3500) (d_OP__casePT_13 x5 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_13 x5 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_13 x5 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_13 x5 x2 x3000 x3500 = case x2 of
     Zero -> Curry_Prelude.C_GT
     (Pos x6) -> d_C_cmpNat x5 x6 x3500
     (Neg x7) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_13 x5 x1001 x3000 x3500) (nd_OP__casePT_13 x5 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_13 x5 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_13 x5 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_14 x2 x3500 = case x2 of
     Zero -> Curry_Prelude.C_EQ
     (Pos x3) -> Curry_Prelude.C_LT
     (Neg x4) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_14 x1001 x3500) (d_OP__casePT_14 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_14 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_14 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_14 x2 x3000 x3500 = case x2 of
     Zero -> Curry_Prelude.C_EQ
     (Pos x3) -> Curry_Prelude.C_LT
     (Neg x4) -> Curry_Prelude.C_GT
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_14 x1001 x3000 x3500) (nd_OP__casePT_14 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_14 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_14 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_23 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Pos x1) Zero
     Curry_Prelude.C_False -> d_OP__casePT_22 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 IHi x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_23 x1 x2 x1001 x3500) (d_OP__casePT_23 x1 x2 x1002 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_23 x1 x2 z x3500) x1001
     (Curry_Prelude.Guard_C_Bool x1000 x1001) -> guardCons x1000 (d_OP__casePT_23 x1 x2 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_23 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 (Pos x1) Zero
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_22 x1 x2 (Curry_Prelude.d_OP_eq_eq x1 IHi x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_23 x1 x2 x1001 x3000 x3500) (nd_OP__casePT_23 x1 x2 x1002 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_23 x1 x2 z x3000 x3500) x1001
     (Curry_Prelude.Guard_C_Bool x1000 x1001) -> guardCons x1000 (nd_OP__casePT_23 x1 x2 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_22 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Zero (Pos x2)
     Curry_Prelude.C_False -> d_OP__casePT_21 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_22 x1 x2 x1001 x3500) (d_OP__casePT_22 x1 x2 x1002 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_22 x1 x2 z x3500) x1001
     (Curry_Prelude.Guard_C_Bool x1000 x1001) -> guardCons x1000 (d_OP__casePT_22 x1 x2 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_22 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> Curry_Prelude.OP_Tuple2 Zero (Pos x2)
     Curry_Prelude.C_False -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_21 x1 x2 (Curry_Prelude.d_C_otherwise x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_22 x1 x2 x1001 x3000 x3500) (nd_OP__casePT_22 x1 x2 x1002 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_22 x1 x2 z x3000 x3500) x1001
     (Curry_Prelude.Guard_C_Bool x1000 x1001) -> guardCons x1000 (nd_OP__casePT_22 x1 x2 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_21 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_True -> d_OP__casePT_20 x1 x2 (d_C_cmpNat x1 x2 x3500) x3500
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_21 x1 x2 x1001 x3500) (d_OP__casePT_21 x1 x2 x1002 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_21 x1 x2 z x3500) x1001
     (Curry_Prelude.Guard_C_Bool x1000 x1001) -> guardCons x1000 (d_OP__casePT_21 x1 x2 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_21 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_True -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_20 x1 x2 (d_C_cmpNat x1 x2 x3500) x2000 x3500))
     Curry_Prelude.C_False -> Curry_Prelude.d_C_failed x3500
     (Curry_Prelude.Choice_C_Bool x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_21 x1 x2 x1001 x3000 x3500) (nd_OP__casePT_21 x1 x2 x1002 x3000 x3500)
     (Curry_Prelude.Choices_C_Bool x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_21 x1 x2 z x3000 x3500) x1001
     (Curry_Prelude.Guard_C_Bool x1000 x1001) -> guardCons x1000 (nd_OP__casePT_21 x1 x2 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_20 x1 x2 x3 x3500 = case x3 of
     Curry_Prelude.C_EQ -> Curry_Prelude.OP_Tuple2 (Pos IHi) Zero
     Curry_Prelude.C_LT -> Curry_Prelude.OP_Tuple2 Zero (Pos x1)
     Curry_Prelude.C_GT -> d_OP__casePT_19 x1 x2 (d_C_quotRemNat (d_C_div2 x1 x3500) x2 x3500) x3500
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_20 x1 x2 x1001 x3500) (d_OP__casePT_20 x1 x2 x1002 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_20 x1 x2 z x3500) x1001
     (Curry_Prelude.Guard_C_Ordering x1000 x1001) -> guardCons x1000 (d_OP__casePT_20 x1 x2 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_20 x1 x2 x3 x3000 x3500 = case x3 of
     Curry_Prelude.C_EQ -> Curry_Prelude.OP_Tuple2 (Pos IHi) Zero
     Curry_Prelude.C_LT -> Curry_Prelude.OP_Tuple2 Zero (Pos x1)
     Curry_Prelude.C_GT -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_19 x1 x2 (d_C_quotRemNat (d_C_div2 x1 x3500) x2 x3500) x2000 x3500))
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_20 x1 x2 x1001 x3000 x3500) (nd_OP__casePT_20 x1 x2 x1002 x3000 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_20 x1 x2 z x3000 x3500) x1001
     (Curry_Prelude.Guard_C_Ordering x1000 x1001) -> guardCons x1000 (nd_OP__casePT_20 x1 x2 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_19 x1 x2 x5 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> d_OP__casePT_18 x1 x2 x4 x3 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_19 x1 x2 x1001 x3500) (d_OP__casePT_19 x1 x2 x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_19 x1 x2 z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP__casePT_19 x1 x2 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_19 x1 x2 x5 x3000 x3500 = case x5 of
     (Curry_Prelude.OP_Tuple2 x3 x4) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_18 x1 x2 x4 x3 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_19 x1 x2 x1001 x3000 x3500) (nd_OP__casePT_19 x1 x2 x1002 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_19 x1 x2 z x3000 x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (nd_OP__casePT_19 x1 x2 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_18 x1 x2 x4 x3 x3500 = case x3 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2 x3500)
     (Pos x5) -> d_OP__casePT_17 x1 x2 x5 x4 x3500
     (Neg x12) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_18 x1 x2 x4 x1001 x3500) (d_OP__casePT_18 x1 x2 x4 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_18 x1 x2 x4 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_18 x1 x2 x4 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_18 x1 x2 x4 x3 x3000 x3500 = case x3 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos IHi) (d_OP_minus_caret x1 x2 x3500)
     (Pos x5) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_17 x1 x2 x5 x4 x2000 x3500))
     (Neg x12) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_18 x1 x2 x4 x1001 x3000 x3500) (nd_OP__casePT_18 x1 x2 x4 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_18 x1 x2 x4 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_18 x1 x2 x4 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_17 x1 x2 x5 x4 x3500 = case x4 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) (d_C_mod2 x1 x3500)
     (Pos x6) -> d_OP__casePT_16 x1 x2 x5 x6 (d_C_quotRemNat (d_OP_quotRemNat_dot_shift_dot_104 x1 x6 x3500) x2 x3500) x3500
     (Neg x11) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_17 x1 x2 x5 x1001 x3500) (d_OP__casePT_17 x1 x2 x5 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_17 x1 x2 x5 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_17 x1 x2 x5 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_17 x1 x2 x5 x4 x3000 x3500 = case x4 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) (d_C_mod2 x1 x3500)
     (Pos x6) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_16 x1 x2 x5 x6 (d_C_quotRemNat (d_OP_quotRemNat_dot_shift_dot_104 x1 x6 x3500) x2 x3500) x2000 x3500))
     (Neg x11) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_17 x1 x2 x5 x1001 x3000 x3500) (nd_OP__casePT_17 x1 x2 x5 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_17 x1 x2 x5 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_17 x1 x2 x5 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_16 x1 x2 x5 x6 x9 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> d_OP__casePT_15 x5 x8 x7 x3500
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_16 x1 x2 x5 x6 x1001 x3500) (d_OP__casePT_16 x1 x2 x5 x6 x1002 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_16 x1 x2 x5 x6 z x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (d_OP__casePT_16 x1 x2 x5 x6 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_16 x1 x2 x5 x6 x9 x3000 x3500 = case x9 of
     (Curry_Prelude.OP_Tuple2 x7 x8) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_15 x5 x8 x7 x2000 x3500))
     (Curry_Prelude.Choice_OP_Tuple2 x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_16 x1 x2 x5 x6 x1001 x3000 x3500) (nd_OP__casePT_16 x1 x2 x5 x6 x1002 x3000 x3500)
     (Curry_Prelude.Choices_OP_Tuple2 x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_16 x1 x2 x5 x6 z x3000 x3500) x1001
     (Curry_Prelude.Guard_OP_Tuple2 x1000 x1001) -> guardCons x1000 (nd_OP__casePT_16 x1 x2 x5 x6 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_15 x5 x8 x7 x3500 = case x7 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) x8
     (Pos x9) -> Curry_Prelude.OP_Tuple2 (Pos (d_OP_plus_caret (O x5) x9 x3500)) x8
     (Neg x10) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_15 x5 x8 x1001 x3500) (d_OP__casePT_15 x5 x8 x1002 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_15 x5 x8 z x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (d_OP__casePT_15 x5 x8 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_15 x5 x8 x7 x3000 x3500 = case x7 of
     Zero -> Curry_Prelude.OP_Tuple2 (Pos (O x5)) x8
     (Pos x9) -> Curry_Prelude.OP_Tuple2 (Pos (d_OP_plus_caret (O x5) x9 x3500)) x8
     (Neg x10) -> Curry_Prelude.d_C_failed x3500
     (Choice_BinInt x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_15 x5 x8 x1001 x3000 x3500) (nd_OP__casePT_15 x5 x8 x1002 x3000 x3500)
     (Choices_BinInt x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_15 x5 x8 z x3000 x3500) x1001
     (Guard_BinInt x1000 x1001) -> guardCons x1000 (nd_OP__casePT_15 x5 x8 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_24 x6 x2 x3500 = case x2 of
     IHi -> Pos (O x6)
     (O x7) -> d_C_inc (d_C_mult2 (d_OP_minus_caret x6 x7 x3500) x3500) x3500
     (I x8) -> d_C_mult2 (d_OP_minus_caret x6 x8 x3500) x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_24 x6 x1001 x3500) (d_OP__casePT_24 x6 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_24 x6 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_24 x6 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_24 x6 x2 x3000 x3500 = case x2 of
     IHi -> Pos (O x6)
     (O x7) -> d_C_inc (d_C_mult2 (d_OP_minus_caret x6 x7 x3500) x3500) x3500
     (I x8) -> d_C_mult2 (d_OP_minus_caret x6 x8 x3500) x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_24 x6 x1001 x3000 x3500) (nd_OP__casePT_24 x6 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_24 x6 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_24 x6 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_25 x1 x3 x2 x3500 = case x2 of
     IHi -> Pos (d_C_pred x1 x3500)
     (O x4) -> d_C_mult2 (d_OP_minus_caret x3 x4 x3500) x3500
     (I x5) -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5 x3500) x3500) x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_25 x1 x3 x1001 x3500) (d_OP__casePT_25 x1 x3 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_25 x1 x3 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_25 x1 x3 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_25 x1 x3 x2 x3000 x3500 = case x2 of
     IHi -> Pos (d_C_pred x1 x3500)
     (O x4) -> d_C_mult2 (d_OP_minus_caret x3 x4 x3500) x3500
     (I x5) -> d_C_dec (d_C_mult2 (d_OP_minus_caret x3 x5 x3500) x3500) x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_25 x1 x3 x1001 x3000 x3500) (nd_OP__casePT_25 x1 x3 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_25 x1 x3 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_25 x1 x3 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_26 x6 x2 x3500 = case x2 of
     IHi -> O (d_C_succ x6 x3500)
     (O x7) -> I (d_OP_plus_caret x6 x7 x3500)
     (I x8) -> O (d_OP_plus_caret (d_C_succ x6 x3500) x8 x3500)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_26 x6 x1001 x3500) (d_OP__casePT_26 x6 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_26 x6 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_26 x6 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_26 x6 x2 x3000 x3500 = case x2 of
     IHi -> O (d_C_succ x6 x3500)
     (O x7) -> I (d_OP_plus_caret x6 x7 x3500)
     (I x8) -> O (d_OP_plus_caret (d_C_succ x6 x3500) x8 x3500)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_26 x6 x1001 x3000 x3500) (nd_OP__casePT_26 x6 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_26 x6 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_26 x6 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_27 x3 x2 x3500 = case x2 of
     IHi -> I x3
     (O x4) -> O (d_OP_plus_caret x3 x4 x3500)
     (I x5) -> I (d_OP_plus_caret x3 x5 x3500)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_27 x3 x1001 x3500) (d_OP__casePT_27 x3 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_27 x3 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_27 x3 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_27 x3 x2 x3000 x3500 = case x2 of
     IHi -> I x3
     (O x4) -> O (d_OP_plus_caret x3 x4 x3500)
     (I x5) -> I (d_OP_plus_caret x3 x5 x3500)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_27 x3 x1001 x3000 x3500) (nd_OP__casePT_27 x3 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_27 x3 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_27 x3 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_28 x2 x3500 = case x2 of
     IHi -> IHi
     (O x3) -> I (d_C_pred x2 x3500)
     (I x4) -> I (O x4)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_28 x1001 x3500) (d_OP__casePT_28 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_28 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_28 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_28 x2 x3000 x3500 = case x2 of
     IHi -> IHi
     (O x3) -> I (d_C_pred x2 x3500)
     (I x4) -> I (O x4)
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_28 x1001 x3000 x3500) (nd_OP__casePT_28 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_28 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_28 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_30 x8 x2 x3500 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x9) -> d_OP__casePT_29 x8 x9 (d_C_cmpNat x8 x9 x3500) x3500
     (I x10) -> d_C_cmpNat x8 x10 x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_30 x8 x1001 x3500) (d_OP__casePT_30 x8 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_30 x8 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_30 x8 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_30 x8 x2 x3000 x3500 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x9) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_29 x8 x9 (d_C_cmpNat x8 x9 x3500) x2000 x3500))
     (I x10) -> d_C_cmpNat x8 x10 x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_30 x8 x1001 x3000 x3500) (nd_OP__casePT_30 x8 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_30 x8 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_30 x8 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_29 x8 x9 x10 x3500 = case x10 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_GT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_29 x8 x9 x1001 x3500) (d_OP__casePT_29 x8 x9 x1002 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_29 x8 x9 z x3500) x1001
     (Curry_Prelude.Guard_C_Ordering x1000 x1001) -> guardCons x1000 (d_OP__casePT_29 x8 x9 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_29 x8 x9 x10 x3000 x3500 = case x10 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_GT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_29 x8 x9 x1001 x3000 x3500) (nd_OP__casePT_29 x8 x9 x1002 x3000 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_29 x8 x9 z x3000 x3500) x1001
     (Curry_Prelude.Guard_C_Ordering x1000 x1001) -> guardCons x1000 (nd_OP__casePT_29 x8 x9 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_32 x5 x2 x3500 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x6) -> d_C_cmpNat x5 x6 x3500
     (I x7) -> d_OP__casePT_31 x5 x7 (d_C_cmpNat x5 x7 x3500) x3500
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_32 x5 x1001 x3500) (d_OP__casePT_32 x5 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_32 x5 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_32 x5 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_32 x5 x2 x3000 x3500 = case x2 of
     IHi -> Curry_Prelude.C_GT
     (O x6) -> d_C_cmpNat x5 x6 x3500
     (I x7) -> let
          x2000 = x3000
           in (seq x2000 (nd_OP__casePT_31 x5 x7 (d_C_cmpNat x5 x7 x3500) x2000 x3500))
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_32 x5 x1001 x3000 x3500) (nd_OP__casePT_32 x5 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_32 x5 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_32 x5 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_31 x5 x7 x8 x3500 = case x8 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_LT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_31 x5 x7 x1001 x3500) (d_OP__casePT_31 x5 x7 x1002 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_31 x5 x7 z x3500) x1001
     (Curry_Prelude.Guard_C_Ordering x1000 x1001) -> guardCons x1000 (d_OP__casePT_31 x5 x7 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_31 x5 x7 x8 x3000 x3500 = case x8 of
     Curry_Prelude.C_EQ -> Curry_Prelude.C_LT
     Curry_Prelude.C_LT -> Curry_Prelude.C_LT
     Curry_Prelude.C_GT -> Curry_Prelude.C_GT
     (Curry_Prelude.Choice_C_Ordering x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_31 x5 x7 x1001 x3000 x3500) (nd_OP__casePT_31 x5 x7 x1002 x3000 x3500)
     (Curry_Prelude.Choices_C_Ordering x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_31 x5 x7 z x3000 x3500) x1001
     (Curry_Prelude.Guard_C_Ordering x1000 x1001) -> guardCons x1000 (nd_OP__casePT_31 x5 x7 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons

d_OP__casePT_33 x2 x3500 = case x2 of
     IHi -> Curry_Prelude.C_EQ
     (O x3) -> Curry_Prelude.C_LT
     (I x4) -> Curry_Prelude.C_LT
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (d_OP__casePT_33 x1001 x3500) (d_OP__casePT_33 x1002 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> d_OP__casePT_33 z x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (d_OP__casePT_33 x1001 (addCs x1000 x3500))
     _ -> failCons

nd_OP__casePT_33 x2 x3000 x3500 = case x2 of
     IHi -> Curry_Prelude.C_EQ
     (O x3) -> Curry_Prelude.C_LT
     (I x4) -> Curry_Prelude.C_LT
     (Choice_Nat x1000 x1001 x1002) -> narrow x1000 (nd_OP__casePT_33 x1001 x3000 x3500) (nd_OP__casePT_33 x1002 x3000 x3500)
     (Choices_Nat x1000 x1001) -> narrows x3500 x1000 (\z -> nd_OP__casePT_33 z x3000 x3500) x1001
     (Guard_Nat x1000 x1001) -> guardCons x1000 (nd_OP__casePT_33 x1001 x3000 (addCs x1000 x3500))
     _ -> failCons