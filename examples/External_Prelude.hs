{-# LANGUAGE MagicHash #-}

import GHC.Prim
import GHC.Types

-- ---------------------------------------------------------------------------
-- Externals
-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- Int
-- ---------------------------------------------------------------------------
data C_Int
  = Choice_C_Int ID C_Int C_Int
  | Fail_C_Int
  | Guard_C_Int Constraint C_Int
  | C_Int Int#
  | C_Integer Integer

instance NonDet C_Int where
  choiceCons = Choice_C_Int
  failCons = Fail_C_Int
  guardCons = Guard_C_Int
  try (Choice_C_Int i x y) = tryChoice i x y
  try Fail_C_Int = Fail
  try (Guard_C_Int c e) = Guard c e
  try x = Val x

instance Generable C_Int where
  generate i = error "No constructors for C_Int"

instance Show C_Int where
  showsPrec d (Choice_C_Int i x y) = showsChoice d i x y
  showsPrec d (Guard_C_Int c e) = showsGuard d c e
  showsPrec d Fail_C_Int = showChar '!'
  showsPrec d (C_Int i) = shows (I# i)
  showsPrec d (C_Integer i) = shows i

instance Unifiable C_Int where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_Int j@(FreeID _) _ _) = [(i :=: (BindTo j))]

instance NormalForm C_Int where
  ($!!) cont x = cont $$!! x

instance Eq C_Int where
  (==) _ _ = False

instance Ord C_Int where
  (<=) _ _ = False

-- ---------------------------------------------------------------------------
-- Float
-- ---------------------------------------------------------------------------
data C_Float
     = Choice_C_Float ID C_Float C_Float
     | Fail_C_Float
     | Guard_C_Float Constraint C_Float
     | C_Float Float#

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

instance Show C_Float where
  showsPrec d (Choice_C_Float i x y) = showsChoice d i x y
  showsPrec d (Guard_C_Float c e) = showsGuard d c e
  showsPrec d Fail_C_Float = showChar '!'
  showsPrec d (C_Float f) = shows (F# f)

instance Unifiable C_Float where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_Float j@(FreeID _) _ _) = [(i :=: (BindTo j))]

instance NormalForm C_Float where
  ($!!) cont x = cont $$!! x

instance Eq C_Float where
  (==) _ _ = False

instance Ord C_Float where
  (<=) _ _ = False

-- ---------------------------------------------------------------------------
-- Char
-- ---------------------------------------------------------------------------
data C_Char
     = Choice_C_Char ID C_Char C_Char
     | Fail_C_Char
     | Guard_C_Char Constraint C_Char
     | C_Char Char#

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

instance Show C_Char where
  showsPrec d (Choice_C_Char i x y) = showsChoice d i x y
  showsPrec d (Guard_C_Char c e) = showsGuard d c e
  showsPrec d Fail_C_Char = showChar '!'
  showsPrec d (C_Char c) = showChar (C# c)

instance Unifiable C_Char where
  (=.=) _ _ = Fail_C_Success
  bind i (Choice_C_Char j@(FreeID _) _ _) = [(i :=: (BindTo j))]

instance NormalForm C_Char where
  ($!!) cont x = cont $$!! x

instance Eq C_Char where
  (==) _ _ = False

instance Ord C_Char where
  (<=) _ _ = False

-- ---------------------------------------------------------------------------
-- IO
-- ---------------------------------------------------------------------------
data C_IO a
     = Choice_C_IO ID (C_IO a) (C_IO a)
     | Fail_C_IO
     | Guard_C_IO Constraint (C_IO a)
     | C_IO (IO a)

instance NonDet (C_IO a) where
  choiceCons = Choice_C_IO
  failCons = Fail_C_IO
  guardCons = Guard_C_IO
  try (Choice_C_IO i x y) = tryChoice i x y
  try Fail_C_IO = Fail
  try (Guard_C_IO c e) = Guard c e
  try x = Val x

instance Generable a => Generable (C_IO a) where
  generate i = error "C_IO: generate"

-- ---------------------------------------------------------------------------
-- Primitive operations
-- ---------------------------------------------------------------------------

-- External DFO
-- -------------

-- external_c_C_seq :: a
external_d_C_seq = error "external_d_C_seq"

-- external_d_C_ensureNotFree :: a
external_d_C_ensureNotFree = error "external_d_C_ensureNotFree"

-- external_d_C_prim_error :: a
external_d_C_prim_error = error "external_d_C_prim_error"

-- external_d_C_failed :: a
external_d_C_failed = error "external_d_C_failed"

-- external_d_OP_eq_eq :: a -> a -> C_Bool
external_d_OP_eq_eq = error "external_d_OP_eq_eq"

-- external_d_C_compare :: a -> a -> C_Ordering
external_d_C_compare = error "external_d_C_compare"

-- external_d_C_prim_ord :: C_Char -> C_Int
external_d_C_prim_ord = error "external_d_C_prim_ord"

-- external_d_C_prim_chr :: C_Int -> C_Char
external_d_C_prim_chr = error "external_d_C_prim_chr"

external_d_C_prim_Int_plus :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_plus (C_Int x) (C_Int y) = C_Int (x +# y)

-- external_d_C_prim_Int_minus :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_minus = error "external_d_C_prim_Int_minus"

-- external_d_C_prim_Int_times :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_times = error "external_d_C_prim_Int_times"

-- external_d_C_prim_Int_div :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_div = error "external_d_C_prim_Int_div"

-- external_d_C_prim_Int_mod :: C_Int -> C_Int -> C_Int
external_d_C_prim_Int_mod = error "external_d_C_prim_Int_mod"

-- external_d_C_prim_negateFloat :: C_Float -> C_Float
external_d_C_prim_negateFloat = error "external_d_C_prim_negateFloat"

-- external_d_OP_eq_colon_eq :: a -> a -> C_Success
external_d_OP_eq_colon_eq = error "external_d_OP_eq_colon_eq"

external_d_C_success :: C_Success
external_d_C_success = C_Success

-- external_d_OP_ampersand :: C_Success -> C_Success -> C_Success
external_d_OP_ampersand = error "external_d_OP_ampersand"

-- external_d_C_return :: a -> C_IO a
external_d_C_return = error "external_d_C_return"

-- external_d_C_prim_putChar :: C_Char -> C_IO OP_Unit
external_d_C_prim_putChar = error "external_d_C_prim_putChar"

-- external_d_C_getChar :: C_IO C_Char
external_d_C_getChar = error "external_d_C_getChar"

-- external_d_C_prim_readFile :: OP_List C_Char -> C_IO (OP_List C_Char)
external_d_C_prim_readFile = error "external_d_C_prim_readFile"

-- external_d_C_prim_readFileContents :: OP_List C_Char -> OP_List C_Char
external_d_C_prim_readFileContents = error "external_d_C_prim_readFileContents"

-- external_d_C_prim_writeFile :: OP_List C_Char -> OP_List C_Char -> C_IO OP_Unit
external_d_C_prim_writeFile = error "external_d_C_prim_writeFile"

-- external_d_C_prim_appendFile :: OP_List C_Char -> OP_List C_Char -> C_IO OP_Unit
external_d_C_prim_appendFile = error "external_d_C_prim_appendFile"

-- external_d_C_catchFail :: C_IO a -> C_IO a -> C_IO a
external_d_C_catchFail = error "external_d_C_catchFail"

-- external_d_C_prim_show    :: a -> OP_List C_Char
external_d_C_prim_show = error "external_d_C_prim_show"

-- external_d_C_unknown :: a
external_d_C_unknown = error "external_d_C_unknown"

-- external_d_C_cond :: C_Success -> a -> a
external_d_C_cond = error "external_d_C_cond"

-- external_d_C_letrec :: a -> a -> C_Success
external_d_C_letrec = error "external_d_C_letrec"

-- external_d_OP_eq_colon_lt_eq :: a -> a -> C_Success
external_d_OP_eq_colon_lt_eq = error "external_d_OP_eq_colon_lt_eq"

-- external_d_OP_eq_colon_lt_lt_eq :: a -> a -> C_Success
external_d_OP_eq_colon_lt_lt_eq = error "external_d_OP_eq_colon_lt_lt_eq"

-- external_d_C_ifVar :: a -> b -> b -> b
external_d_C_ifVar = error "external_d_C_ifVar"

-- external_d_C_failure :: a -> b -> c
external_d_C_failure = error "external_d_C_failure"

-- External ND
-- -----------

-- external_nd_OP_qmark :: NonDet a => a -> a -> IDSupply -> a
external_nd_OP_qmark x y ids = let i = thisID ids in i `seq` choiceCons i x y

-- External HO
-- -----------

external_dho_C_apply :: (a -> b) -> a -> b
external_dho_C_apply f a = f a

external_ndho_C_apply :: Func a b -> a -> IDSupply -> b
external_ndho_C_apply (Func f) a s = f a s

-- external_dho_C_catch :: C_IO a -> (C_IOError -> C_IO a) -> C_IO a
external_dho_C_catch = error "external_dho_C_catch"

-- external_ndho_C_catch :: C_IO a -> (C_IOError -> C_IO a) -> C_IO a
external_ndho_C_catch = error "external_ndho_C_catch"

-- external_dho_OP_gt_gt_eq :: C_IO a -> (a -> C_IO b) -> C_IO b
external_dho_OP_gt_gt_eq = error "external_dho_OP_gt_gt_eq"

-- external_ndho_OP_gt_gt_eq :: C_IO a -> (a -> C_IO b) -> C_IO b
external_ndho_OP_gt_gt_eq = error "external_ndho_OP_gt_gt_eq"

-- external_dho_C_try :: (a -> Success) -> [a -> Success]
external_dho_C_try = error "external_dho_C_try"

-- external_ndho_C_try :: Func a Success -> [Func a Success]
external_ndho_C_try = error "external_ndho_C_try"