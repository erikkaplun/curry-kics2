import Data.IORef
import System.IO
import System.Directory(doesFileExist)
import System.IO.Unsafe
import qualified Curry_Prelude as CP

-- Implementation of Globals in Curry. We use Haskell's IORefs for temporary
-- globals where Curry values are stored in the IORefs
data C_Global a
     = Choice_C_Global ID (C_Global a) (C_Global a)
     | Fail_C_Global
     | Guard_C_Global [Constraint] (C_Global a)
     | C_Global_Temp (IORef a)  -- a temporary global
     | C_Global_Pers String     -- a persistent global with a given (file) name

instance Show (C_Global a) where
  show = error "ERROR: no show for Global"

instance Read (C_Global a) where
  readsPrec = error "ERROR: no read for Global"

instance NonDet (C_Global a) where
  choiceCons = Choice_C_Global
  failCons = Fail_C_Global
  guardCons = Guard_C_Global
  try (Choice_C_Global i x y) = tryChoice i x y
  try Fail_C_Global = Fail
  try (Guard_C_Global c e) = Guard c e
  try x = Val x

instance Generable (C_Global a) where
  generate _ = error "ERROR: no generator for Global"

instance NormalForm (C_Global a) where
  cont $!! io@(C_Global_Temp _) = cont io
  cont $!! io@(C_Global_Pers _) = cont io
  cont $!! Choice_C_Global i io1 io2 = nfChoice cont i io1 io2
  cont $!! Guard_C_Global c io = guardCons c (cont $!! io)
  _    $!! Fail_C_Global = failCons

instance Unifiable (C_Global a) where
  (=.=) _ _ = error "(=.=) for C_Global"
  bind i (Choice_C_Global j@(FreeID _ _) _ _) = [i :=: (BindTo j)]

instance CP.Curry a => CP.Curry (C_Global a) where
  (=?=) = error "(=?=) is undefined for Globals"
  (<?=) = error "(<?=) is undefined for Globals"


external_d_C_global :: CP.Curry a => a -> C_GlobalSpec -> C_Global a
external_d_C_global val C_Temporary = ref `seq` (C_Global_Temp ref)
  where ref = unsafePerformIO (newIORef val)
external_d_C_global val (C_Persistent cname) =
  let name = fromCurry cname :: String
   in unsafePerformIO (initGlobalFile name >> return (C_Global_Pers name))
 where initGlobalFile name = do
         ex <- doesFileExist name
         if ex then return ()
               else writeFile name (show val++"\n")

external_d_C_prim_readGlobal :: CP.Curry a => C_Global a -> CP.C_IO a
external_d_C_prim_readGlobal (C_Global_Temp ref) = fromIO (readIORef ref)
external_d_C_prim_readGlobal (C_Global_Pers name) = fromIO $
  do h <- openFile name ReadMode
     s <- hGetLine h
     hClose h
     return (read s)

external_d_C_prim_writeGlobal :: CP.Curry a => C_Global a -> a
                                            -> CP.C_IO CP.OP_Unit
external_d_C_prim_writeGlobal (C_Global_Temp ref) val =
  fromIO (writeIORef ref val >> return CP.OP_Unit)
external_d_C_prim_writeGlobal (C_Global_Pers name) val =
  fromIO (writeFile name (show val++"\n") >> return CP.OP_Unit)
