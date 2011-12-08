{-# LANGUAGE MultiParamTypeClasses #-}
import Data.IORef
import System.IO.Unsafe(unsafePerformIO) -- for global associations
import System.Process(runInteractiveCommand)
import Control.Concurrent(forkIO)
import System.IO
import qualified Curry_Prelude as CP

external_d_C_prim_execCmd :: CP.C_String
 -> ConstStore -> CP.C_IO (CP.OP_Tuple3 Curry_IO.C_Handle Curry_IO.C_Handle Curry_IO.C_Handle)
external_d_C_prim_execCmd str _ = fromHaskellIO1
  (\s -> do (h1,h2,h3,_) <- runInteractiveCommand s
            return (OneHandle h1, OneHandle h2, OneHandle h3)) str

external_d_C_prim_connectToCmd :: CP.C_String -> ConstStore -> CP.C_IO Curry_IO.C_Handle
external_d_C_prim_connectToCmd str _ = fromHaskellIO1
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
external_d_C_prim_setAssoc str1 str2 _ = fromHaskellIO2
  (\key val -> do as <- readIORef assocs
                  writeIORef assocs ((key,val):as)) str1 str2

external_d_C_prim_getAssoc :: CP.C_String -> ConstStore -> CP.C_IO (CP.C_Maybe (CP.C_String))
external_d_C_prim_getAssoc str _ = fromHaskellIO1
  (\key -> do as <- readIORef assocs
              return (lookup key as)) str

-----------------------------------------------------------------------
-- Implementation of IORefs in Curry. Note that we store Curry values
-- (and not the corresponding Haskell values) in the Haskell IORefs
data C_IORef a
     = Choice_C_IORef ID (C_IORef a) (C_IORef a)
     | Choices_C_IORef ID ([C_IORef a])
     | Fail_C_IORef
     | Guard_C_IORef Constraints (C_IORef a)
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
  try (Choice_C_IORef i x y) = tryChoice i x y
  try (Choices_C_IORef s xs) = tryChoices s xs
  try Fail_C_IORef = Fail
  try (Guard_C_IORef c e) = Guard c e
  try x = Val x
  match f _ _ _ _ _ (Choice_C_IORef  i x y)                 = f i x y
  match _ f _ _ _ _ (Choices_C_IORef i@(NarrowedID _ _) xs) = f i xs
  match _ _ f _ _ _ (Choices_C_IORef i@(FreeID _ _)     xs) = f i xs
  match _ _ _ _ _ _ (Choices_C_IORef i@(ChoiceID _) _)      =
    error ("IOExts.IORef.match: Choices with ChoiceID " ++ show i)
  match _ _ _ f _ _ Fail_C_IORef                            = f
  match _ _ _ _ f _ (Guard_C_IORef cs e)                    = f cs e
  match _ _ _ _ _ f x                                       = f x

instance Generable (C_IORef a) where
  generate _ = error "ERROR: no generator for IORef"

instance NormalForm (C_IORef a) where
  ($!!) cont ioref@(C_IORef _)          cs = cont ioref cs
  ($!!) cont (Choice_C_IORef i io1 io2) cs = nfChoice cont i io1 io2 cs
  ($!!) cont (Choices_C_IORef i ios)    cs = nfChoices cont i ios cs
  ($!!) cont (Guard_C_IORef c io)       cs = guardCons c ((cont $!! io) (addCs c cs)) 
  ($!!) _    Fail_C_IORef               cs = failCons
  ($##) cont io@(C_IORef _)             cs = cont io cs
  ($##) cont (Choice_C_IORef i io1 io2) cs = gnfChoice cont i io1 io2 cs
  ($##) cont (Choices_C_IORef i ios)    cs = gnfChoices cont i ios cs
  ($##) cont (Guard_C_IORef c io)       cs = guardCons c ((cont $## io) (addCs c cs)) 
  ($##) _    Fail_C_IORef               cs = failCons
  ($!<) cont (Choice_C_IORef i x y)     = nfChoiceIO cont i x y
  ($!<) cont (Choices_C_IORef i xs)     = nfChoicesIO cont i xs
  ($!<) cont x                          = cont x
  searchNF _ cont ioref@(C_IORef _)     = cont ioref

instance Unifiable (C_IORef a) where
  (=.=) _ _ = error "(=.=) for C_IORef"
  (=.<=) _ _ = error "(=.<=) for C_IORef"
  bind i (Choice_C_IORef j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_IORef j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_IORef j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_IORef = [Unsolvable]
  bind i (Guard_C_IORef cs e) = (getConstrList cs) ++ (bind i e)
  lazyBind i (Choice_C_IORef j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_IORef j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_IORef j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_IORef = [Unsolvable]
  lazyBind i (Guard_C_IORef cs e) = (getConstrList cs) ++ [(i :=: (LazyBind (lazyBind i e)))]

instance CP.Curry a => CP.Curry (C_IORef a) where
  (=?=) = error "(==) is undefined for IORefs"
  (<?=) = error "(<=) is undefined for IORefs"

instance ConvertCurryHaskell (C_IORef a) (IORef a) where
  fromCurry (C_IORef r) = r
  fromCurry _           = error "IORef with no ground term occurred"

  toCurry r = C_IORef r

external_d_C_newIORef :: CP.Curry a => a -> ConstStore -> CP.C_IO (C_IORef a)
external_d_C_newIORef cv _ = fromIO (newIORef cv >>= return . toCurry)

external_d_C_prim_readIORef :: CP.Curry a => C_IORef a -> ConstStore -> CP.C_IO a
external_d_C_prim_readIORef ref _ = fromIO (readIORef (fromCurry ref))

external_d_C_prim_writeIORef :: CP.Curry a => C_IORef a -> a
                                           -> ConstStore -> CP.C_IO CP.OP_Unit
external_d_C_prim_writeIORef ref cv _ =
 fromIO (writeIORef (fromCurry ref) cv >>= return . toCurry)

-----------------------------------------------------------------------