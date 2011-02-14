-- Externals

data C_Int
  = Int_Choice ID C_Int C_Int
  | Int_Fail
  | Int_Guard Constraint C_Int
  | C_Int Int#
  | C_Integer Integer
  deriving (Eq, Show)

data C_Float
  = Float_Choice ID C_Float C_Float
  | Float_Fail
  | Float_Guard Constraint C_Float
  | C_Float Float#
  deriving (Eq, Show)

data C_Char
  = Choice_C_Char C_Char C_Char
  | Fail_C_Char
  | Guard_C_Char Constraint C_Char
  | C_Char Char#
  deriving (Eq, Show)

{-
data C_Success
  = Choice_C_Success ID C_Success C_Success
  | Fail_C_Success
  | Guard_C_Success Constraint C_Success
  | C_Success
  deriving (Show,Eq)
-}

data C_IO a
  = Choice_C_IO ID (C_IO a) (C_IO a)
  | Fail_C_IO
  | Guard_C_IO Constraint (C_IO a)
  | C_IO (IO a)
  deriving (Show,Eq)

external_c_C_seq :: a
external_c_C_seq = error "external_c_C_seq"

external_c_C_ensureNotFree :: a
external_c_C_ensureNotFree = error "external_c_C_ensureNotFree"

external_c_C_prim_error :: a
external_c_C_prim_error = error "external_c_C_prim_error"

external_c_C_failed :: a
external_c_C_failed = error "external_c_C_failed"

external_c_OP_eq_eq :: a -> a -> C_Bool
external_c_OP_eq_eq = error "external_c_OP_eq_eq"

external_c_C_compare :: a -> a -> C_Ordering
external_c_C_compare = error "external_c_C_compare"

external_c_C_prim_ord :: C_Char -> C_Int
external_c_C_prim_ord = error "external_c_C_prim_ord"

external_c_C_prim_chr :: C_Int -> C_Char
external_c_C_prim_chr = error "external_c_C_prim_chr"

external_c_C_prim_Int_plus :: C_Int -> C_Int -> C_Int
external_c_C_prim_Int_plus = error "external_c_C_prim_Int_plus"

external_c_C_prim_Int_minus :: C_Int -> C_Int -> C_Int
external_c_C_prim_Int_minus = error "external_c_C_prim_Int_minus"

external_c_C_prim_Int_times :: C_Int -> C_Int -> C_Int
external_c_C_prim_Int_times = error "external_c_C_prim_Int_times"

external_c_C_prim_Int_div :: C_Int -> C_Int -> C_Int
external_c_C_prim_Int_div = error "external_c_C_prim_Int_div"

external_c_C_prim_Int_mod :: C_Int -> C_Int -> C_Int
external_c_C_prim_Int_mod = error "external_c_C_prim_Int_mod"

external_c_C_prim_negateFloat :: C_Float -> C_Float
external_c_C_prim_negateFloat = error "external_c_C_prim_negateFloat"

external_c_OP_eq_colon_eq :: a -> a -> C_Success
external_c_OP_eq_colon_eq = error "external_c_OP_eq_colon_eq"

external_c_C_success :: C_Success
external_c_C_success = C_Success

external_c_OP_ampersand :: C_Success -> C_Success -> C_Success
external_c_OP_ampersand = error "external_c_OP_ampersand"

external_c_OP_gt_gt_eq :: C_IO a -> (Func a (C_IO b)) -> C_IO b
external_c_OP_gt_gt_eq = error "external_c_OP_gt_gt_eq"

external_c_C_return :: a -> C_IO a
external_c_C_return = error "external_c_C_return"

external_c_C_prim_putChar :: C_Char -> C_IO C_Unit
external_c_C_prim_putChar = error "external_c_C_prim_putChar"

external_c_C_getChar :: C_IO C_Char
external_c_C_getChar = error "external_c_C_getChar"

external_c_C_prim_readFile :: C_String -> C_IO C_String
external_c_C_prim_readFile = error "external_c_C_prim_readFile"

external_c_C_prim_writeFile :: C_String -> C_String -> C_IO ()
external_c_C_prim_writeFile = error "external_c_C_prim_writeFile"

external_c_C_prim_appendFile :: C_String -> C_String -> C_IO ()
external_c_C_prim_appendFile = error "external_c_C_prim_appendFile"

external_c_C_catch :: C_IO a -> Func C_IOError (C_IO a) -> C_IO a
external_c_C_catch = error "external_c_C_catch"

external_c_C_catchFail :: C_IO a -> C_IO a -> C_IO a
external_c_C_catchFail = error "external_c_C_catchFail"

external_c_C_prim_show    :: a -> C_String
external_c_C_prim_show = error "external_c_C_prim_show"

external_c_OP_qmark :: NonDet a => a -> a -> IDSupply -> a
external_c_OP_qmark x y ids = let i = thisID ids in i `seq` choiceCons i x y

external_c_C_unknown :: a
external_c_C_unknown = error "external_c_C_unknown"

external_c_C_try :: a
external_c_C_try = error "external_c_C_try"

external_c_C_apply :: Func a b -> a -> b
external_c_C_apply = error "external_c_C_apply"

external_c_C_cond :: C_Success -> a -> a
external_c_C_cond = error "external_c_C_cond"

external_c_C_letrec :: a -> a -> C_Success
external_c_C_letrec = error "external_c_C_letrec"

external_c_OP_eq_colon_lt_eq :: a -> a -> C_Success
external_c_OP_eq_colon_lt_eq = error "external_c_OP_eq_colon_lt_eq"

external_c_OP_eq_colon_lt_lt_eq :: a -> a -> C_Success
external_c_OP_eq_colon_lt_lt_eq = error "external_c_OP_eq_colon_lt_lt_eq"

external_c_C_ifVar :: a -> b -> b -> b
external_c_C_ifVar = error "external_c_C_ifVar"