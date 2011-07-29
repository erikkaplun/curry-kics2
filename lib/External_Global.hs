import Data.IORef
import System.IO
import System.Directory(doesFileExist)
import System.IO.Unsafe
import qualified Curry_Prelude as CP

-- Implementation of Globals in Curry. We use Haskell's IORefs for temporary
-- globals where Curry values are stored in the IORefs
data C_Global a
     = Choice_C_Global ID (C_Global a) (C_Global a)
     | Choices_C_Global ID ([C_Global a])
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
  choicesCons = Choices_C_Global
  failCons = Fail_C_Global
  guardCons = Guard_C_Global
  try (Choice_C_Global i x y) = tryChoice i x y
  try (Choices_C_Global i xs) = tryChoices i xs
  try Fail_C_Global = Fail
  try (Guard_C_Global c e) = Guard c e
  try x = Val x
  match choiceF _ _ _ _ _ (Choice_C_Global i x y) = choiceF i x y
  match _ narrF _ _ _ _   (Choices_C_Global i@(NarrowedID _ _) xs) = narrF i xs
  match _ _ freeF _ _ _   (Choices_C_Global i@(FreeID _ _) xs)     = freeF i xs
  match _ _ _ failV _ _   Fail_C_Global = failV
  match _ _ _ _ guardF _  (Guard_C_Global c e) = guardF c e
  match _ _ _ _ _ valF    x                    = valF x 

instance Generable (C_Global a) where
  generate _ = error "ERROR: no generator for Global"

instance NormalForm (C_Global a) where
  cont $!! g@(C_Global_Temp _) = cont g
  cont $!! g@(C_Global_Pers _) = cont g
  cont $!! Choice_C_Global i g1 g2 = nfChoice cont i g1 g2
  cont $!! Choices_C_Global i gs = nfChoices cont i gs
  cont $!! Guard_C_Global c g = guardCons c (cont $!! g)
  _    $!! Fail_C_Global = failCons
  cont $## g@(C_Global_Temp _) = cont g
  cont $## g@(C_Global_Pers _) = cont g
  cont $## Choice_C_Global i g1 g2 = gnfChoice cont i g1 g2
  cont $## Choices_C_Global i gs = gnfChoices cont i gs
  cont $## Guard_C_Global c g = guardCons c (cont $## g)
  _    $## Fail_C_Global = failCons
  cont $!< Choice_C_Global i g1 g2 = nfChoiceIO cont i g1 g2
  cont $!< Choices_C_Global i gs   = nfChoicesIO cont i gs
  cont $!< x = cont x
  searchNF _ cont g@(C_Global_Temp _) = cont g
  searchNF _ cont g@(C_Global_Pers _) = cont g

instance Unifiable (C_Global a) where
  (=.=) (C_Global_Temp ref1) (C_Global_Temp ref2)
    | ref1 == ref2 = C_Success
    | otherwise    = Fail_C_Success
  (=.=) (C_Global_Pers f1) (C_Global_Pers f2)
    | f1 == f2  = C_Success
    | otherwise = Fail_C_Success
  (=.=) _ _ = Fail_C_Success
  (=.<=) = (=.=)
  bind i (Choice_C_Global j l r) = [(ConstraintChoice j (bind i l) (bind i r))]
  bind i (Choices_C_Global j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  bind i (Choices_C_Global j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (bind i) xs))]
  bind _ Fail_C_Global = [Unsolvable]
  bind i (Guard_C_Global cs e) = cs ++ (bind i e)
  lazyBind i (Choice_C_Global j l r) = [(ConstraintChoice j (lazyBind i l) (lazyBind i r))]
  lazyBind i (Choices_C_Global j@(FreeID _ _) xs) = [(i :=: (BindTo j))]
  lazyBind i (Choices_C_Global j@(NarrowedID _ _) xs) = [(ConstraintChoices j (map (lazyBind i) xs))]
  lazyBind _ Fail_C_Global = [Unsolvable]
  lazyBind i (Guard_C_Global cs e) = cs ++ [(i :=: (LazyBind (lazyBind i e)))]

instance CP.Curry a => CP.Curry (C_Global a) where
  (=?=) = error "(==) is undefined for Globals"
  (<?=) = error "(<=) is undefined for Globals"


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
