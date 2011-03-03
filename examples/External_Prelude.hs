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
class ( Show a -- TODO remove Eq and Ord later
      , NonDet a, Generable a, NormalForm a, Unifiable a) => Curry a where
  (=?=) :: a -> a -> C_Bool
  (=?=) = error "(=?=) is undefined"

  (<?=) :: a -> a -> C_Bool
  (<?=) = error "(<?=) is undefined"

instance Curry C_Success where
  C_Success =?= C_Success = C_True
  _         =?= _         = C_False

  C_Success <?= C_Success = C_True
  _         <?= _         = C_False

instance Curry (Func a b) where

instance Curry (a -> b) where

instance Curry (C_IO a) where

-- ---------------------------------------------------------------------------
-- Int
-- ---------------------------------------------------------------------------
data C_Int
  = Choice_C_Int ID C_Int C_Int
  | Fail_C_Int
  | Guard_C_Int Constraint C_Int
  | C_Int Int#
--   | C_Integer Integer

instance Eq C_Int where
  C_Int x == C_Int y = x ==# y
  x       == y       = error $ "(==) for C_Int with " ++ show x ++ " and " ++ show y
--   C_Integer i == C_Integer j = i == j

instance Ord C_Int where
  C_Int x <= C_Int y = x <=# y
  x       <= y       = error $ "(<=) for C_Int with " ++ show x ++ " and " ++ show y

instance Show C_Int where
  showsPrec d (Choice_C_Int i x y) = showsChoice d i x y
  showsPrec d (Guard_C_Int c e) = showsGuard d c e
  showsPrec d Fail_C_Int = showChar '!'
  showsPrec d (C_Int i) = shows (I# i)
--   showsPrec d (C_Integer i) = shows i

instance NonDet C_Int where
  choiceCons = Choice_C_Int
  failCons = Fail_C_Int
  guardCons = Guard_C_Int
  try (Choice_C_Int i x y) = tryChoice i x y
  try Fail_C_Int = Fail
  try (Guard_C_Int c e) = Guard c e
  try x = Val x

instance Generable C_Int where
  generate _ = error "No constructors for C_Int"

instance NormalForm C_Int where
  cont $!! i@(C_Int _) = cont i
  cont $!! v           = cont $$!! v

instance Unifiable C_Int where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_Int j@(FreeID _) _ _) = [(i :=: (BindTo j))]

instance Curry C_Int where
  C_Int x =?= C_Int y = fromBool (x ==# y)
  x       =?= y       = error $ "(=?=) for C_Int with " ++ show x ++ " and " ++ show y

  C_Int x <?= C_Int y = fromBool (x <=# y)
  x       <?= y       = error $ "(<?=) for C_Int with " ++ show x ++ " and " ++ show y

-- ---------------------------------------------------------------------------
-- Float
-- ---------------------------------------------------------------------------
data C_Float
     = Choice_C_Float ID C_Float C_Float
     | Fail_C_Float
     | Guard_C_Float Constraint C_Float
     | C_Float Float#

instance Eq C_Float where
  C_Float x == C_Float y = x `eqFloat#` y
  x         == y         = error $ "(==) for C_Float with " ++ show x ++ " and " ++ show y

instance Ord C_Float where
  C_Float x <= C_Float y = x `leFloat#` y
  x         <= y         = error $ "(<=) for C_Float with " ++ show x ++ " and " ++ show y

instance Show C_Float where
  showsPrec d (Choice_C_Float i x y) = showsChoice d i x y
  showsPrec d (Guard_C_Float c e) = showsGuard d c e
  showsPrec d Fail_C_Float = showChar '!'
  showsPrec d (C_Float f) = shows (F# f)

instance NonDet C_Float where
  choiceCons = Choice_C_Float
  failCons = Fail_C_Float
  guardCons = Guard_C_Float
  try (Choice_C_Float i x y) = tryChoice i x y
  try Fail_C_Float = Fail
  try (Guard_C_Float c e) = Guard c e
  try x = Val x

instance Generable C_Float where
  generate i = error "No constructors for C_Float"

instance NormalForm C_Float where
  cont $!! f@(C_Float _) = cont f
  cont $!! v             = cont $$!! v

instance Unifiable C_Float where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_Float j@(FreeID _) _ _) = [(i :=: (BindTo j))]

instance Curry C_Float where
  C_Float x =?= C_Float y = fromBool (x `eqFloat#` y)
  x         =?= y         = error $ "(=?=) for C_Float with " ++ show x ++ " and " ++ show y

  C_Float x <?= C_Float y = fromBool (x `leFloat#` y)
  x         <?= y         = error $ "(<?=) for C_Float with " ++ show x ++ " and " ++ show y

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------
data C_Char
     = Choice_C_Char ID C_Char C_Char
     | Fail_C_Char
     | Guard_C_Char Constraint C_Char
     | C_Char Char#

instance Eq C_Char where
  C_Char x == C_Char y = x `eqChar#` y
  x        == y        = error $ "(==) for C_Char with " ++ show x ++ " and " ++ show y

instance Ord C_Char where
  C_Char x <= C_Char y = x `leChar#` y
  x        <= y        = error $ "(<=) for C_Char with " ++ show x ++ " and " ++ show y

instance Show C_Char where
  showsPrec d (Choice_C_Char i x y) = showsChoice d i x y
  showsPrec d (Guard_C_Char c e) = showsGuard d c e
  showsPrec d Fail_C_Char = showChar '!'
  showsPrec d (C_Char c) = showChar (C# c)

instance NonDet C_Char where
  choiceCons = Choice_C_Char
  failCons = Fail_C_Char
  guardCons = Guard_C_Char
  try (Choice_C_Char i x y) = tryChoice i x y
  try Fail_C_Char = Fail
  try (Guard_C_Char c e) = Guard c e
  try x = Val x

instance Generable C_Char where
  generate i = error "No constructors for C_Char"

instance NormalForm C_Char where
  cont $!! c@(C_Char _) = cont c
  cont $!! v            = cont $$!! v

instance Unifiable C_Char where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_Char j@(FreeID _) _ _) = [(i :=: (BindTo j))]

instance Curry C_Char where
  C_Char x =?= C_Char y = fromBool (x `eqChar#` y)
  x        =?= y        = error $ "(=?=) for C_Char with " ++ show x ++ " and " ++ show y

  C_Char x <?= C_Char y = fromBool (x `leChar#` y)
  x        <?= y        = error $ "(<?=) for C_Char with " ++ show x ++ " and " ++ show y

-- ---------------------------------------------------------------------------
-- Conversion from and to primitive Haskell types
-- ---------------------------------------------------------------------------

fromChar :: Char -> C_Char
fromChar (C# c) = C_Char c

toChar :: C_Char -> Char
toChar (C_Char c) = C# c
toChar _          = error "Curry_Prelude.toChar with no ground term"

fromString :: String -> C_String
fromString [] = OP_List
fromString (c:cs) = OP_Cons (fromChar c) (fromString cs)

toString :: C_String -> String
toString OP_List = []
toString (OP_Cons c cs) = toChar c : toString cs
toString _ = error "Curry_Prelude.toString with no ground term"

fromBool :: Bool -> C_Bool
fromBool True  = C_True
fromBool False = C_False

fromOrdering :: Ordering -> C_Ordering
fromOrdering LT = C_LT
fromOrdering EQ = C_EQ
fromOrdering GT = C_GT

-- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

-- External DFO
-- -------------

external_d_C_ensureNotFree :: Curry a => a -> a
external_d_C_ensureNotFree x = id `dho_OP_dollar_bang` x

external_d_C_prim_error :: C_String -> a
external_d_C_prim_error s = error (toString s)

external_d_C_failed :: NonDet a => a
external_d_C_failed = failCons

external_d_OP_eq_eq :: Curry a => a -> a -> C_Bool
external_d_OP_eq_eq x y = (\a -> (\b -> a =?= b) `dho_OP_dollar_bang` y) `dho_OP_dollar_bang` x

external_d_C_compare :: Curry a => a -> a -> C_Ordering
external_d_C_compare x y = (\a -> (\b -> (a `comp` b)) `dho_OP_dollar_bang` y) `dho_OP_dollar_bang` x where
  comp a b = (\x -> case x of
                C_True  -> C_EQ
                C_False ->
                  (\y -> case y of
                          C_True  -> C_LT
                          C_False -> C_GT )
                  `dho_OP_dollar_bang` (a <?= b))
              `dho_OP_dollar_bang` (a =?= b)

external_d_C_prim_ord :: C_Char -> C_Int
external_d_C_prim_ord (C_Char c) = C_Int (ord# c)

external_d_C_prim_chr :: C_Int -> C_Char
external_d_C_prim_chr (C_Int i) = C_Char (chr# i)

external_d_C_prim_Int_plus :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_plus (C_Int x) (C_Int y) = C_Int (y +# x)

external_d_C_prim_Int_minus :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_minus (C_Int x) (C_Int y) = C_Int (y -# x)

external_d_C_prim_Int_times :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_times (C_Int x) (C_Int y) = C_Int (y *# x)

external_d_C_prim_Int_div :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_div (C_Int x) (C_Int y) = C_Int (quotInt# y x)

external_d_C_prim_Int_mod :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_mod (C_Int x) (C_Int y) = C_Int (remInt# y x)

external_d_C_prim_negateFloat :: C_Float -> C_Float
external_d_C_prim_negateFloat (C_Float x) = C_Float (negateFloat# x)

external_d_OP_eq_colon_eq :: Unifiable a => a -> a -> C_Success
external_d_OP_eq_colon_eq = (=:=)

external_d_C_success :: C_Success
external_d_C_success = C_Success

external_d_OP_ampersand :: C_Success -> C_Success -> C_Success
external_d_OP_ampersand = (&)

external_d_C_return :: a -> C_IO a
external_d_C_return a = fromIO (return a)

external_d_C_prim_putChar :: C_Char -> C_IO OP_Unit
external_d_C_prim_putChar c = fromIO $ putChar (toChar c) >> return OP_Unit

external_d_C_getChar :: C_IO C_Char
external_d_C_getChar = fromIO $ getChar >>= return . fromChar

external_d_C_prim_readFile :: OP_List C_Char -> C_IO (OP_List C_Char)
external_d_C_prim_readFile s = fromIO $ readFile (toString s) >>= return . fromString

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_writeFile :: OP_List C_Char -> OP_List C_Char -> C_IO OP_Unit
external_d_C_prim_writeFile f s = fromIO $ writeFile f' s' >> return OP_Unit
  where f' = toString f
        s' = toString s

-- TODO: Problem: s is not evaluated to enable lazy IO and therefore could
-- be non-deterministic
external_d_C_prim_appendFile :: OP_List C_Char -> OP_List C_Char -> C_IO OP_Unit
external_d_C_prim_appendFile f s = fromIO $ appendFile f' s' >> return OP_Unit
  where f' = toString f
        s' = toString s

external_d_C_catchFail :: C_IO a -> C_IO a -> C_IO a
external_d_C_catchFail act err = fromIO $ catch (toIO act) handle
  where handle ioErr = print ioErr >> (toIO err)

external_d_C_prim_show :: Show a => a -> OP_List C_Char
external_d_C_prim_show a = fromString (show a)

external_d_C_cond :: Curry a => C_Success -> a -> a
external_d_C_cond succ a = const a `dho_OP_dollar_bang` succ

-- External ND
-- -----------

external_nd_OP_qmark :: NonDet a => a -> a -> IDSupply -> a
external_nd_OP_qmark x y ids = let i = thisID ids in i `seq` choiceCons i x y

-- External HO
-- -----------

external_dho_OP_dollar_bang :: (NonDet a, NonDet b) => (a -> b) -> a -> b
external_dho_OP_dollar_bang f x = hnf (try x)
  where
   hnf (Val v) = f v
   hnf Fail    = failCons
   hnf (Choice id a b) = choiceCons id (hnf (try a)) (hnf (try b))
   -- TODO give reasonable implementation (see $$!!)
   hnf (Free id a b) = error "external_dho_OP_dollar_bang with free variable"
   hnf (Guard c e) = guardCons c (hnf (try e))

external_ndho_OP_dollar_bang :: (NonDet a, NonDet b) => (Func a b) -> a -> IDSupply -> b
external_ndho_OP_dollar_bang f x s = hnf (try x)
  where
   hnf (Val v) = external_ndho_C_apply f v s
   hnf Fail    = failCons
   -- TODO Do we have to use leftSupply and rightSupply?
   hnf (Choice id a b) = choiceCons id (hnf (try a)) (hnf (try b))
   -- TODO give reasonable implementation (see $$!!)
   hnf (Free id a b) = error "external_dho_OP_dollar_bang with free variable"
   hnf (Guard c e) = guardCons c (hnf (try e))

external_dho_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => (a -> b) -> a -> b
external_dho_OP_dollar_bang_bang = ($!!)

external_ndho_OP_dollar_bang_bang :: (NormalForm a, NonDet b) => Func a b -> a -> IDSupply -> b
external_ndho_OP_dollar_bang_bang f x s = (\y -> external_ndho_C_apply f y s) $!! x

external_dho_C_apply :: (a -> b) -> a -> b
external_dho_C_apply f a = f a

-- TODO: Support non-deterministic Funcs
external_ndho_C_apply :: Func a b -> a -> IDSupply -> b
external_ndho_C_apply (Func f) a s = f a s

external_dho_C_catch :: C_IO a -> (C_IOError -> C_IO a) -> C_IO a
external_dho_C_catch act cont = fromIO $ catch (toIO act) handle where
  handle = toIO . cont . C_IOError . fromString . ioe_description

external_ndho_C_catch :: C_IO a -> Func C_IOError (C_IO a) -> IDSupply -> C_IO a
external_ndho_C_catch act cont s = C_IO $ catch (toIO act) handle where
  handle e = toIO (external_ndho_C_apply cont (C_IOError (fromString (ioe_description e))) s)

-- TODO: Support non-deterministic IO ?
external_dho_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> (t0 -> C_IO t1) -> C_IO t1
external_dho_OP_gt_gt_eq m f = fromIO $ (toIO m) >>= toIO . f

-- TODO: Support non-deterministic IO ?
external_ndho_OP_gt_gt_eq :: (Curry t0, Curry t1) => C_IO t0 -> Func t0 (C_IO t1) -> IDSupply -> C_IO t1
external_ndho_OP_gt_gt_eq m f s = fromIO $ (toIO m) >>= \x -> toIO (external_ndho_C_apply f x s)

-- Encapsulated search
-- -------------------

-- external_dho_C_try :: (a -> Success) -> [a -> Success]
external_dho_C_try = error "external_dho_C_try"

-- external_ndho_C_try :: Func a Success -> [Func a Success]
external_ndho_C_try = error "external_ndho_C_try"