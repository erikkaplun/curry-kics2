{-# LANGUAGE MagicHash #-}

import GHC.IO.Exception (IOException (..))

-- ATTENTION: Do not introduce line breaks in import declarations as these
-- are not recognized!
import GHC.Exts (Int (I#), Int#, (==#), (<=#), (+#), (-#), (*#), quotInt#, remInt#)
import GHC.Exts (Float (F#), Float#, eqFloat#, leFloat#, negateFloat#)
import GHC.Exts (Char (C#), Char#, eqChar#, leChar#, ord#, chr#)

-- ---------------------------------------------------------------------------
-- Externals
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Curry types
-- ---------------------------------------------------------------------------

-- Class for curry types
class (Show a, Read a, NonDet a, Generable a, NormalForm a, Unifiable a) => Curry a where
  (=?=) :: a -> a -> C_Bool
  (=?=) = error "(=?=) is undefined"

  (<?=) :: a -> a -> C_Bool
  (<?=) = error "(<?=) is undefined"

instance Curry (PrimData a) where
  (=?=) = error "(=?=) is undefined for primitive data"

  (<?=) = error "(<?=) is undefined for primitive data"


instance Curry C_Success where
  Choice_C_Success i x y =?= z                      = Choice_C_Bool i (x =?= z) (y =?= z)
  Guard_C_Success c x    =?= y                      = Guard_C_Bool c (x =?= y)
  Fail_C_Success         =?= _                      = Fail_C_Bool
  x                      =?= Choice_C_Success i y z = Choice_C_Bool i (x =?= y) (x =?= z)
  x                      =?= Guard_C_Success c y    = Guard_C_Bool c (x =?= y)
  _                      =?= Fail_C_Success         = Fail_C_Bool
  C_Success              =?= C_Success              = C_True

  Choice_C_Success i x y <?= z                      = Choice_C_Bool i (x <?= z) (y <?= z)
  Guard_C_Success c x    <?= y                      = Guard_C_Bool c (x <?= y)
  Fail_C_Success         <?= _                      = Fail_C_Bool
  x                      <?= Choice_C_Success i y z = Choice_C_Bool i (x <?= y) (x <?= z)
  x                      <?= Guard_C_Success c y    = Guard_C_Bool c (x <?= y)
  _                      <?= Fail_C_Success         = Fail_C_Bool
  C_Success              <?= C_Success              = C_True

instance Curry (Func a b) where

instance Curry (a -> b) where

instance Curry (C_IO a) where

-- ---------------------------------------------------------------------------
-- Int
-- ---------------------------------------------------------------------------
data C_Int
  = Choice_C_Int ID C_Int C_Int
  | Fail_C_Int
  | Guard_C_Int [Constraint] C_Int
  | C_Int Int#
--   | C_Integer Integer

instance Show C_Int where
  showsPrec d (Choice_C_Int i x y) = showsChoice d i x y
  showsPrec d (Guard_C_Int c e) = showsGuard d c e
  showsPrec d Fail_C_Int = showChar '!'
  showsPrec d (C_Int i) = shows (I# i)
--   showsPrec d (C_Integer i) = shows i

instance Read C_Int where
  readsPrec d s = map readInt (readsPrec d s)
    where
     readInt (I# i,s) = (C_Int i, s)

instance NonDet C_Int where
  choiceCons = Choice_C_Int
  failCons = Fail_C_Int
  guardCons = Guard_C_Int
  try (Choice_C_Int i x y) = tryChoice i x y
  try Fail_C_Int = Fail
  try (Guard_C_Int c e) = Guard c e
  try x = Val x

  match valF _ _ _ _ v@(C_Int _) = valF v
  match _ fail _ _ _ Fail_C_Int  = fail
  match _ _ choiceF _ _ (Choice_C_Int i@(ID _) x y) = choiceF i x y
  match _ _ _ freeF _ (Choice_C_Int i@(FreeID _) x y) = freeF i x y
  match _ _ _ _ guardF (Guard_C_Int c x) = guardF c x

instance Generable C_Int where
  generate _ = error "No generator for C_Int"

instance NormalForm C_Int where
  cont $!! i@(C_Int _) = cont i
  cont $!! Choice_C_Int i x y = nfChoice cont i x y
  cont $!! Guard_C_Int c x = guardCons c (cont $!! x)
  _    $!! Fail_C_Int = failCons

instance Unifiable C_Int where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_Int j@(FreeID _) _ _) = [i :=: BindTo j]

instance Curry C_Int where
  Choice_C_Int i x y =?= z                  = Choice_C_Bool i (x =?= z) (y =?= z)
  Guard_C_Int c x    =?= y                  = Guard_C_Bool c (x =?= y)
  Fail_C_Int         =?= _                  = Fail_C_Bool
  x                  =?= Choice_C_Int i y z = Choice_C_Bool i (x =?= y) (x =?= z)
  x                  =?= Guard_C_Int c y    = Guard_C_Bool c (x =?= y)
  _                  =?= Fail_C_Int         = Fail_C_Bool
  C_Int x            =?= C_Int y            = toCurry (x ==# y)
--  x =?= y =  (\ (C_Int a) -> (\ (C_Int b) -> toCurry (a==#b)) `d_dollar_bang_test` y)
--             `d_dollar_bang_test` x

  Choice_C_Int i x y <?= z                  = Choice_C_Bool i (x <?= z) (y <?= z)
  Guard_C_Int c x    <?= y                  = Guard_C_Bool c (x <?= y)
  Fail_C_Int         <?= _                  = Fail_C_Bool
  x                  <?= Choice_C_Int i y z = Choice_C_Bool i (x <?= y) (x <?= z)
  x                  <?= Guard_C_Int c y    = Guard_C_Bool c (x <?= y)
  _                  <?= Fail_C_Int         = Fail_C_Bool
  C_Int x            <?= C_Int y            = toCurry (x <=# y)
--  x <?= y =  (\ (C_Int a) -> (\ (C_Int b) -> toCurry (a <=# b)) `d_dollar_bang_test` y)
--             `d_dollar_bang_test` x

-- ---------------------------------------------------------------------------
-- Float
-- ---------------------------------------------------------------------------
data C_Float
     = Choice_C_Float ID C_Float C_Float
     | Fail_C_Float
     | Guard_C_Float [Constraint] C_Float
     | C_Float Float#

instance Show C_Float where
  showsPrec d (Choice_C_Float i x y) = showsChoice d i x y
  showsPrec d (Guard_C_Float c e) = showsGuard d c e
  showsPrec d Fail_C_Float = showChar '!'
  showsPrec d (C_Float f) = shows (F# f)

instance Read C_Float where
  readsPrec d s = map readFloat (readsPrec d s)
    where
     readFloat (F# f,s) = (C_Float f, s)

instance NonDet C_Float where
  choiceCons = Choice_C_Float
  failCons = Fail_C_Float
  guardCons = Guard_C_Float
  try (Choice_C_Float i x y) = tryChoice i x y
  try Fail_C_Float = Fail
  try (Guard_C_Float c e) = Guard c e
  try x = Val x

instance Generable C_Float where
  generate i = error "No generator for C_Float"

instance NormalForm C_Float where
  cont $!! f@(C_Float _)        = cont f
  cont $!! Choice_C_Float i x y = nfChoice cont i x y
  cont $!! Guard_C_Float c x    = guardCons c (cont $!! x)
  _    $!! Fail_C_Float         = failCons

instance Unifiable C_Float where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_Float j@(FreeID _) _ _) = [i :=: BindTo j]

instance Curry C_Float where
  Choice_C_Float i x y =?= z                    = Choice_C_Bool i (x =?= z) (y =?= z)
  Guard_C_Float c x    =?= y                    = Guard_C_Bool c (x =?= y)
  Fail_C_Float         =?= _                    = Fail_C_Bool
  x                    =?= Choice_C_Float i y z = Choice_C_Bool i (x =?= y) (x =?= z)
  x                    =?= Guard_C_Float c y    = Guard_C_Bool c (x =?= y)
  _                    =?= Fail_C_Float         = Fail_C_Bool
  C_Float x            =?= C_Float y            = toCurry (x `eqFloat#` y)

  Choice_C_Float i x y <?= z                    = Choice_C_Bool i (x <?= z) (y <?= z)
  Guard_C_Float c x    <?= y                    = Guard_C_Bool c (x <?= y)
  Fail_C_Float         <?= _                    = Fail_C_Bool
  x                    <?= Choice_C_Float i y z = Choice_C_Bool i (x <?= y) (x <?= z)
  x                    <?= Guard_C_Float c y    = Guard_C_Bool c (x <?= y)
  _                    <?= Fail_C_Float         = Fail_C_Bool
  C_Float x            <?= C_Float y            = toCurry (x `leFloat#` y)

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------
data C_Char
     = Choice_C_Char ID C_Char C_Char
     | Fail_C_Char
     | Guard_C_Char [Constraint] C_Char
     | C_Char Char#

instance Show C_Char where
  showsPrec d (Choice_C_Char i x y) = showsChoice d i x y
  showsPrec d (Guard_C_Char c e) = showsGuard d c e
  showsPrec d Fail_C_Char = showChar '!'
  showsPrec d (C_Char c) = showString (show (C# c))

  -- to show strings in the standard string notation:
  showList cs = showList (map (\ (C_Char c) -> (C# c)) cs)


instance Read C_Char where
  readsPrec d s = map readChar (readsPrec d s)
    where
     readChar (C# c,s) = (C_Char c, s)

  -- to read strings in the standard string notation:
  readList s = map readString (readList s)
   where
     readString (cs,s) = (map (\ (C# c) -> C_Char c) cs, s)


instance NonDet C_Char where
  choiceCons = Choice_C_Char
  failCons = Fail_C_Char
  guardCons = Guard_C_Char
  try (Choice_C_Char i x y) = tryChoice i x y
  try Fail_C_Char = Fail
  try (Guard_C_Char c e) = Guard c e
  try x = Val x

instance Generable C_Char where
  generate i = error "No generator for C_Char"

instance NormalForm C_Char where
  cont $!! c@(C_Char _)          = cont c
  cont $!! Choice_C_Char i c1 c2 = nfChoice cont i c1 c2
  cont $!! Guard_C_Char c char   = guardCons c (cont $!! char)
  _    $!! Fail_C_Char           = failCons

instance Unifiable C_Char where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_Char j@(FreeID _) _ _) = [i :=: BindTo j]

instance Curry C_Char where
  Choice_C_Char i x y =?= z                   = Choice_C_Bool i (x =?= z) (y =?= z)
  Guard_C_Char c x    =?= y                   = Guard_C_Bool c (x =?= y)
  Fail_C_Char         =?= _                   = Fail_C_Bool
  x                   =?= Choice_C_Char i y z = Choice_C_Bool i (x =?= y) (x =?= z)
  x                   =?= Guard_C_Char c y    = Guard_C_Bool c (x =?= y)
  _                   =?= Fail_C_Char         = Fail_C_Bool
  C_Char x            =?= C_Char y            = toCurry (x `eqChar#` y)

  Choice_C_Char i x y <?= z                   = Choice_C_Bool i (x <?= z) (y <?= z)
  Guard_C_Char c x    <?= y                   = Guard_C_Bool c (x <?= y)
  Fail_C_Char         <?= _                   = Fail_C_Bool
  x                   <?= Choice_C_Char i y z = Choice_C_Bool i (x <?= y) (x <?= z)
  x                   <?= Guard_C_Char c y    = Guard_C_Bool c (x <?= y)
  _                   <?= Fail_C_Char         = Fail_C_Bool
  C_Char x            <?= C_Char y            = toCurry (x `leChar#` y)

-- ---------------------------------------------------------------------------
-- Conversion from and to primitive Haskell types
-- ---------------------------------------------------------------------------

instance ConvertCurryHaskell C_Int Int where
  toCurry (I# i) = C_Int i

  fromCurry (C_Int i) = I# i
  fromCurry _         = error "Int data with no ground term"

instance ConvertCurryHaskell C_Int Integer where
  toCurry i = int2C_Int (fromInteger i)

  fromCurry (C_Int i) = toInteger (I# i)
  fromCurry _         = error "Int data with no ground term"

int2C_Int :: Int -> C_Int
int2C_Int (I# c) = C_Int c

instance ConvertCurryHaskell C_Float Float where
  toCurry (F# f) = C_Float f

  fromCurry (C_Float f) = F# f
  fromCurry _           = error "Float data with no ground term"

instance ConvertCurryHaskell C_Char Char where
  toCurry (C# c) = C_Char c

  fromCurry (C_Char c) = C# c
  fromCurry _          = error "Char data with no ground term"

instance (ConvertCurryHaskell ct ht) =>
         ConvertCurryHaskell (OP_List ct) [ht] where
  toCurry []     = OP_List
  toCurry (c:cs) = OP_Cons (toCurry c) (toCurry cs)

  fromCurry OP_List        = []
  fromCurry (OP_Cons c cs) = fromCurry c : fromCurry cs
  fromCurry _              = error "List data with no ground term"

instance ConvertCurryHaskell C_Bool Bool where
  toCurry True  = C_True
  toCurry False = C_False

  fromCurry C_True  = True
  fromCurry C_False = False
  fromCurry _       = error "Float data with no ground term"

instance ConvertCurryHaskell OP_Unit () where
  toCurry ()  = OP_Unit

  fromCurry OP_Unit = ()
  fromCurry _       = error "Unit data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2) =>
         ConvertCurryHaskell (OP_Tuple2 ct1 ct2) (ht1,ht2) where
  toCurry (x1,x2)  = OP_Tuple2 (toCurry x1) (toCurry x2)

  fromCurry (OP_Tuple2 x1 x2) = (fromCurry x1, fromCurry x2)
  fromCurry _       = error "Pair data with no ground term"

instance (ConvertCurryHaskell ct1 ht1, ConvertCurryHaskell ct2 ht2,
          ConvertCurryHaskell ct3 ht3) =>
         ConvertCurryHaskell (OP_Tuple3 ct1 ct2 ct3) (ht1,ht2,ht3) where
  toCurry (x1,x2,x3)  = OP_Tuple3 (toCurry x1) (toCurry x2) (toCurry x3)

  fromCurry (OP_Tuple3 x1 x2 x3) = (fromCurry x1, fromCurry x2, fromCurry x3)
  fromCurry _       = error "Tuple3 data with no ground term occurred"

instance ConvertCurryHaskell ct ht =>
         ConvertCurryHaskell (C_Maybe ct) (Maybe ht) where
  toCurry Nothing  = C_Nothing
  toCurry (Just x) = C_Just (toCurry x)

  fromCurry C_Nothing  = Nothing
  fromCurry (C_Just x) = Just (fromCurry x)
  fromCurry _          = error "Maybe data with no ground term occurred"

--fromOrdering :: Ordering -> C_Ordering
--fromOrdering LT = C_LT
--fromOrdering EQ = C_EQ
--fromOrdering GT = C_GT

-- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

-- External DFO
-- -------------

external_d_C_ensureNotFree :: Curry a => a -> a
external_d_C_ensureNotFree x =
  case try x of
    Choice i a b -> choiceCons i (external_d_C_ensureNotFree a)
                                 (external_d_C_ensureNotFree b)
    Free i a b   -> narrow i (external_d_C_ensureNotFree a)
                             (external_d_C_ensureNotFree b)
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

external_d_C_prim_chr :: C_Int -> C_Char
external_d_C_prim_chr (C_Int i) = C_Char (chr# i)

external_d_OP_plus :: C_Int -> C_Int -> C_Int
external_d_OP_plus (C_Int x) (C_Int y) = C_Int (x +# y)
external_d_OP_plus x y = (\a -> (\b -> (a `external_d_OP_plus` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_OP_minus :: C_Int -> C_Int -> C_Int
external_d_OP_minus (C_Int x) (C_Int y) = C_Int (x -# y)
external_d_OP_minus x y = (\a -> (\b -> (a `external_d_OP_minus` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_OP_star :: C_Int -> C_Int -> C_Int
external_d_OP_star (C_Int x) (C_Int y) = C_Int (x *# y)
external_d_OP_star x y = (\a -> (\b -> (a `external_d_OP_star` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_div :: C_Int -> C_Int -> C_Int
external_d_C_div (C_Int x) (C_Int y) = C_Int (quotInt# x y)
external_d_C_div x y = (\a -> (\b -> (a `external_d_C_div` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

external_d_C_mod :: C_Int -> C_Int -> C_Int
external_d_C_mod (C_Int x) (C_Int y) = C_Int (remInt# x y)
external_d_C_mod x y = (\a -> (\b -> (a `external_d_C_mod` b)) `d_OP_dollar_hash` y) `d_OP_dollar_hash` x

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
external_d_C_catchFail act err = fromIO $ catch (toIO act) handle
  where handle ioErr = print ioErr >> (toIO err)

external_d_C_prim_show :: Show a => a -> C_String
external_d_C_prim_show a = toCurry (show a)

external_d_C_cond :: Curry a => C_Success -> a -> a
external_d_C_cond succ a = const a `d_dollar_bang` succ

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

external_d_C_apply :: (a -> b) -> a -> b
external_d_C_apply = d_apply

external_nd_C_apply :: NonDet b => Func a b -> a -> IDSupply -> b
external_nd_C_apply = nd_apply

external_d_C_catch :: C_IO a -> (C_IOError -> C_IO a) -> C_IO a
external_d_C_catch act cont = fromIO $ catch (toIO act) handle where
  handle = toIO . cont . C_IOError . toCurry . ioe_description

external_nd_C_catch :: C_IO a -> Func C_IOError (C_IO a) -> IDSupply -> C_IO a
external_nd_C_catch act cont s = C_IO $ catch (toIO act) handle where
  handle e = toIO (nd_apply cont (C_IOError (toCurry (ioe_description e))) s)

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