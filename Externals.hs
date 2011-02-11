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
  = Char_Choice ID C_Char C_Char
  | Char_Fail
  | Char_Guard Constraint C_Char
  | C_Char Char#
  deriving (Eq, Show)

data C_List a
  = List_Choice ID (C_List a) (C_List a)
  | List_Fail
  | List_Guard Constraint (C_List a)
  | C_Nil
  | C_Cons a (C_List a)

data C_Success 
  = Success_Choice ID C_Success C_Success
  | Success_Fail
  | Success_Guard Constraint C_Success
  | C_Success
  deriving (Show,Eq)

-- TODO what about IO?
data C_IO a 
  = IO_Choice ID (C_IO a) (C_IO a)
  | IO_Fail
  | IO_Guard Constraint (C_IO a)
  | C_IO (IO a)
  deriving (Show,Eq)

c_seq :: a
c_seq = undefined

c_ensureNotFree :: a
c_ensureNotFree = undefined

c_prim_error :: a
c_prim_error = undefined

c_failed :: a
c_failed = undefined

c_op_eq_eq :: a -> a -> C_Bool
c_op_eq_eq = undefined

c_compare :: a -> a -> C_Ordering
c_compare = undefined

c_prim_ord :: C_Char -> C_Int
c_prim_ord = undefined

c_prim_chr :: C_Int -> C_Char
c_prim_chr = undefined

c_prim_Int_plus :: C_Int -> C_Int -> C_Int
c_prim_Int_plus = undefined

c_prim_Int_minus :: C_Int -> C_Int -> C_Int
c_prim_Int_minus = undefined

c_prim_Int_times :: C_Int -> C_Int -> C_Int
c_prim_Int_times = undefined

c_prim_Int_div :: C_Int -> C_Int -> C_Int
c_prim_Int_div = undefined

c_prim_Int_mod :: C_Int -> C_Int -> C_Int
c_prim_Int_mod = undefined

c_prim_negateFloat :: C_Float -> C_Float
c_prim_negateFloat = undefined

c_op_eq_colon_eq :: a -> a -> C_Success
c_op_eq_colon_eq = undefined

success :: C_Success
success = C_Success

c_op_ampersand :: C_Success -> C_Success -> C_Success
c_op_ampersand = undefined

c_op_gt_gt_eq :: C_IO a -> (Func a (C_IO b)) -> C_IO b
c_op_gt_gt_eq = undefined

c_return :: a -> C_IO a
c_return = undefined

c_prim_putChar :: C_Char -> C_IO C_Unit
c_prim_putChar = undefined

c_getChar :: C_IO C_Char
c_getChar = undefined

c_prim_readFile :: C_String -> C_IO C_String
c_prim_readFile = undefined

c_prim_writeFile :: C_String -> C_String -> C_IO ()
c_prim_writeFile = undefined

c_prim_appendFile :: C_String -> C_String -> C_IO ()
c_prim_appendFile = undefined

c_catch :: C_IO a -> Func C_IOError (C_IO a) -> C_IO a
c_catch = undefined

c_catchFail :: C_IO a -> C_IO a -> C_IO a
c_catchFail = undefined

c_prim_show    :: a -> C_String
c_prim_show = undefined

c_op_qmark :: NonDet a => a -> a -> IDSupply -> a
c_op_qmark = undefined

c_unknown :: a
c_unknown = undefined

c_try :: a
c_try = undefined

c_apply :: Func a b -> a -> b
c_apply = undefined

c_cond :: C_Success -> a -> a
c_cond = undefined

c_letrec :: a -> a -> C_Success
c_letrec = undefined

c_op_eq_colon_lt_eq :: a -> a -> C_Success
c_op_eq_colon_lt_eq = undefined

c_op_eq_colon_lt_lt_eq :: a -> a -> C_Success
c_op_eq_colon_lt_lt_eq = undefined

c_ifVar :: a -> b -> b -> b
c_ifVat = undefined