import Data.IORef
import System.IO.Unsafe(unsafePerformIO) -- for global associations
import System.Process(runInteractiveCommand)
import Control.Concurrent(forkIO)
import System.IO
import qualified Curry_Prelude as CP

external_d_C_prim_execCmd :: CP.C_String
 -> CP.C_IO (CP.OP_Tuple3 Curry_IO.C_Handle Curry_IO.C_Handle Curry_IO.C_Handle)
external_d_C_prim_execCmd = fromHaskellIO1
  (\s -> do (h1,h2,h3,_) <- runInteractiveCommand s
            return (OneHandle h1, OneHandle h2, OneHandle h3))

external_d_C_prim_connectToCmd :: CP.C_String -> CP.C_IO Curry_IO.C_Handle
external_d_C_prim_connectToCmd = fromHaskellIO1
  (\s -> do (hin,hout,herr,_) <- runInteractiveCommand s
            forkIO (forwardError herr)
            return (InOutHandle hout hin))

forwardError :: Handle -> IO ()
forwardError h = do
   eof <- hIsEOF h 
   if eof then return ()
          else hGetLine h >>= hPutStrLn stderr >> forwardError h


-----------------------------------------------------------------------
-- Implementation of global associations as simple association lists
-- (could be later improved by a more efficient implementation, e.g., maps)

type Assocs = [(String,String)]

assocs :: IORef Assocs
assocs = unsafePerformIO (newIORef [])

external_d_C_prim_setAssoc :: CP.C_String -> CP.C_String -> CP.C_IO CP.OP_Unit
external_d_C_prim_setAssoc = fromHaskellIO2 
  (\key val -> do as <- readIORef assocs
                  writeIORef assocs ((key,val):as))

external_d_C_prim_getAssoc :: CP.C_String -> CP.C_IO (CP.C_Maybe (CP.C_String))
external_d_C_prim_getAssoc = fromHaskellIO1
  (\key -> do as <- readIORef assocs
              return (lookup key as))

-----------------------------------------------------------------------
-- Implementation of IORefs in Curry. Note that we store Curry values
-- (and not the corresponding Haskell values) in the Haskell IORefs
data C_IORef a
     = Choice_C_IORef ID (C_IORef a) (C_IORef a)
     | Fail_C_IORef
     | Guard_C_IORef [Constraint] (C_IORef a)
     | C_IORef (IORef a)

instance Show (C_IORef a) where
  show = error "ERROR: no show for IORef"

instance Read (C_IORef a) where
  readsPrec = error "ERROR: no read for IORef"

instance NonDet (C_IORef a) where
  choiceCons = Choice_C_IORef
  failCons = Fail_C_IORef
  guardCons = Guard_C_IORef
  try (Choice_C_IORef i x y) = tryChoice i x y
  try Fail_C_IORef = Fail
  try (Guard_C_IORef c e) = Guard c e
  try x = Val x

instance Generable (C_IORef a) where
  generate _ = error "ERROR: no generator for IORef"

instance NormalForm (C_IORef a) where
  cont $!! io@(C_IORef _) = cont io
  cont $!! Choice_C_IORef i io1 io2 = nfChoice cont i io1 io2
  cont $!! Guard_C_IORef c io = guardCons c (cont $!! io)
  _    $!! Fail_C_IORef = failCons

instance Unifiable (C_IORef a) where
  (=.=) _ _ = error "(=.=) for C_IORef"
  bind i (Choice_C_IORef j@(FreeID _) _ _) = [i :=: (BindTo j)]

instance CP.Curry a => CP.Curry (C_IORef a) where
  (=?=) = error "(=?=) is undefined for IORefs"
  (<?=) = error "(<?=) is undefined for IORefs"

instance ConvertCurryHaskell (C_IORef a) (IORef a) where
  fromCurry (C_IORef r) = r
  fromCurry _           = error "IORef with no ground term occurred"

  toCurry r = C_IORef r

external_d_C_newIORef :: CP.Curry a => a -> CP.C_IO (C_IORef a)
external_d_C_newIORef cv = fromIO (newIORef cv >>= return . toCurry)

external_d_C_prim_readIORef :: CP.Curry a => C_IORef a -> CP.C_IO a
external_d_C_prim_readIORef ref = fromIO (readIORef (fromCurry ref))

external_d_C_prim_writeIORef :: CP.Curry a => C_IORef a -> a
                                           -> CP.C_IO CP.OP_Unit
external_d_C_prim_writeIORef ref cv =
 fromIO (writeIORef (fromCurry ref) cv >>= return . toCurry)

-----------------------------------------------------------------------