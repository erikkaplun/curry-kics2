{-# LANGUAGE MultiParamTypeClasses #-}
import Data.IORef
import System.IO.Unsafe   (unsafePerformIO) -- for global associations
import System.Process     (readProcessWithExitCode, runInteractiveCommand)
import Control.Concurrent (forkIO)
import System.IO
import qualified Curry_Prelude as CP

external_d_C_prim_execCmd :: CP.C_String
 -> ConstStore -> CP.C_IO (CP.OP_Tuple3 Curry_IO.C_Handle Curry_IO.C_Handle Curry_IO.C_Handle)
external_d_C_prim_execCmd str _ = toCurry
  (\s -> do (h1,h2,h3,_) <- runInteractiveCommand s
            return (OneHandle h1, OneHandle h2, OneHandle h3)) str

external_d_C_prim_evalCmd :: CP.C_String -> CP.OP_List CP.C_String -> CP.C_String
  -> ConstStore -> CP.C_IO (CP.OP_Tuple3 CP.C_Int CP.C_String CP.C_String)
external_d_C_prim_evalCmd cmd args input _
  = toCurry readProcessWithExitCode cmd args input

external_d_C_prim_connectToCmd :: CP.C_String -> ConstStore -> CP.C_IO Curry_IO.C_Handle
external_d_C_prim_connectToCmd str _ = toCurry
  (\s -> do (hin,hout,herr,_) <- runInteractiveCommand s
            forkIO (forwardError herr)
            return (InOutHandle hout hin)) str

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

external_d_C_prim_setAssoc :: CP.C_String -> CP.C_String -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_setAssoc str1 str2 _ = toCurry
  (\key val -> do as <- readIORef assocs
                  writeIORef assocs ((key,val):as)) str1 str2

external_d_C_prim_getAssoc :: CP.C_String -> ConstStore -> CP.C_IO (CP.C_Maybe (CP.C_String))
external_d_C_prim_getAssoc str _ = toCurry
  (\key -> do as <- readIORef assocs
              return (lookup key as)) str

-----------------------------------------------------------------------
-- Implementation of IORefs in Curry. Note that we store Curry values
-- (and not the corresponding Haskell values) in the Haskell IORefs
data C_IORef a
    = Choice_C_IORef Cover ID (C_IORef a) (C_IORef a)
    | Choices_C_IORef Cover ID ([C_IORef a])
    | Fail_C_IORef Cover FailInfo
    | Guard_C_IORef Cover  Constraints (C_IORef a)
    | C_IORef (IORef a)

instance Show (C_IORef a) where
  show = error "ERROR: no show for IORef"

instance Read (C_IORef a) where
  readsPrec = error "ERROR: no read for IORef"

instance NonDet (C_IORef a) where
  choiceCons = Choice_C_IORef
  choicesCons = Choices_C_IORef
  failCons = Fail_C_IORef
  guardCons = Guard_C_IORef
  try (Choice_C_IORef cd i x y) = tryChoice cd i x y
  try (Choices_C_IORef cd s xs) = tryChoices cd s xs
  try (Fail_C_IORef cd info) = Fail cd info
  try (Guard_C_IORef cd c e) = Guard cd c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_IORef  cd i x y)                 = f cd i x y
  match _ f _ _ _ _ (Choices_C_IORef cd i@(NarrowedID _ _) xs) = f cd i xs
  match _ _ f _ _ _ (Choices_C_IORef cd i@(FreeID _ _)     xs) = f cd i xs
  match _ _ _ _ _ _ (Choices_C_IORef _ i _)      =
    error ("IOExts.IORef.match: Choices with ChoiceID " ++ show i)
  match _ _ _ f _ _ (Fail_C_IORef cd info)                     = f cd info
  match _ _ _ _ f _ (Guard_C_IORef cd cs e)                    = f cd cs e
  match _ _ _ _ _ f x                                          = f x

instance Generable (C_IORef a) where
  generate _ = error "ERROR: no generator for IORef"

instance NormalForm (C_IORef a) where
  ($!!) cont ioref@(C_IORef _)          cs = cont ioref cs
  ($!!) cont (Choice_C_IORef cd i io1 io2) cs = nfChoice cont cd i io1 io2 cs
  ($!!) cont (Choices_C_IORef cd i ios)    cs = nfChoices cont cd  i ios cs
  ($!!) cont (Guard_C_IORef cd c io)       cs = guardCons cd c ((cont $!! io) (addCs c cs))
  ($!!) _    (Fail_C_IORef cd info)     cs = failCons cd info
  ($##) cont io@(C_IORef _)             cs = cont io cs
  ($##) cont (Choice_C_IORef cd i io1 io2) cs = gnfChoice cont cd i io1 io2 cs
  ($##) cont (Choices_C_IORef cd i ios)    cs = gnfChoices cont cd i ios cs
  ($##) cont (Guard_C_IORef cd c io)       cs = guardCons cd c ((cont $## io) (addCs c cs))
  ($##) _    (Fail_C_IORef cd info)        cs = failCons cd info
  ($!<) cont (Choice_C_IORef cd i x y)     = nfChoiceIO cont cd i x y
  ($!<) cont (Choices_C_IORef cd i xs)     = nfChoicesIO cont cd i xs
  ($!<) cont x                             = cont x
  searchNF _ cont ioref@(C_IORef _)        = cont ioref

instance Unifiable (C_IORef a) where
  (=.=) _ _ = error "(=.=) for C_IORef"
  (=.<=) _ _ = error "(=.<=) for C_IORef"
  bind i (Choice_C_IORef cd j l r) = [(ConstraintChoice cd j (bind i l) (bind i r))]
  bind i (Choices_C_IORef cd j@(FreeID _ _) xs) = bindOrNarrow i cd j xs
  bind i (Choices_C_IORef cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (bind i) xs))]
  bind _ (Fail_C_IORef cd info) = [Unsolvable info]
  bind i (Guard_C_IORef _ cs e) = (getConstrList cs) ++ (bind i e)
  lazyBind i (Choice_C_IORef cd j l r) = [(ConstraintChoice cd j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_IORef cd j@(FreeID _ _) xs) = lazyBindOrNarrow i cd j xs
  lazyBind i (Choices_C_IORef cd j@(NarrowedID _ _) xs) = [(ConstraintChoices cd j (map (lazyBind i) xs))]
  lazyBind _ (Fail_C_IORef cd info) = [Unsolvable info]
  lazyBind i (Guard_C_IORef _ cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind i e)))]

instance CP.Curry a => CP.Curry (C_IORef a) where
  (=?=) = error "(==) is undefined for IORefs"
  (<?=) = error "(<=) is undefined for IORefs"

instance Coverable (C_IORef a) where
  cover (Choice_C_IORef cd i x y) = Choice_C_IORef (incCover cd) i (cover x) (cover y)
  cover (Choices_C_IORef cd i xs) = Choices_C_IORef (incCover cd) i (map cover xs)
  cover (Fail_C_IORef cd info)    = Fail_C_IORef (incCover cd) info
  cover (Guard_C_IORef cd cs x)   = Guard_C_IORef (incCover cd) cs (cover x)
  cover r@(C_IORef _)             = r

instance ConvertCurryHaskell (C_IORef a) (IORef a) where
  fromCurry (C_IORef r) = r
  fromCurry _           = error "IORef with no ground term occurred"
  toCurry r             = C_IORef r

external_d_C_newIORef :: CP.Curry a => a -> ConstStore -> CP.C_IO (C_IORef a)
external_d_C_newIORef cv _ = toCurry (newIORef cv)

external_d_C_prim_readIORef :: CP.Curry a => C_IORef a -> ConstStore -> CP.C_IO a
external_d_C_prim_readIORef ref _ = fromIO (readIORef (fromCurry ref))

external_d_C_prim_writeIORef :: CP.Curry a => C_IORef a -> a
                                           -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_writeIORef ref cv _ = toCurry (writeIORef (fromCurry ref) cv)
