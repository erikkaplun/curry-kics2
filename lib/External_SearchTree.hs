
import System.IO
import Control.Monad
import Control.Parallel.TreeSearch
import MonadSearch
import GHC.Exts (Int (I#), (<#))

instance Monad C_SearchTree where
  return = C_Value

  C_Fail d  >>= _ = C_Fail d
  C_Value x >>= f = f x
  C_Or x y  >>= f = C_Or (x >>= f) (y >>= f)

  Choice_C_SearchTree cd i x y >>= f = Choice_C_SearchTree cd i (x >>= f) (y >>= f)
  Choices_C_SearchTree cd i xs >>= f = Choices_C_SearchTree cd i (map (>>= f) xs)
  Guard_C_SearchTree cd cs x   >>= f = Guard_C_SearchTree cd cs (x >>= f)
  Fail_C_SearchTree cd info >>= _ = Fail_C_SearchTree cd info   
  

instance MonadPlus C_SearchTree where
  mzero = C_Fail (Curry_Prelude.C_Int -1#)
  mplus = C_Or

instance MonadSearch C_SearchTree where
  splus            = Choice_C_SearchTree
  ssum             = Choices_C_SearchTree
  szero (I# d) _   = C_Fail (Curry_Prelude.C_Int d)
  constrainMSearch = Guard_C_SearchTree

external_d_C_someSearchTree :: NormalForm a => a -> ConstStore -> C_SearchTree a
external_d_C_someSearchTree = encapsulatedSearch

external_d_OP_bar_plus_plus_bar ::  Curry_Prelude.Curry a =>
 C_ValueSequence a -> C_ValueSequence a -> ConstStore -> C_ValueSequence a
external_d_OP_bar_plus_plus_bar l1 l2 _ = l1 |++| l2

data C_ValueSequence a = EmptyVS | Values (Curry_Prelude.OP_List a) 
                       | FailVS (Curry_Prelude.C_Int)
                       | Choice_VS Cover ID (C_ValueSequence a) (C_ValueSequence a)
                       | Choices_VS Cover ID [C_ValueSequence a]
                       | Guard_VS Cover Constraints (C_ValueSequence a)

instance Curry_Prelude.Curry (C_ValueSequence a) where

instance Show (C_ValueSequence a) where
  showsPrec = error "SearchTree: ValueSequence: showsPrec"

instance Read (C_ValueSequence a) where
  readsPrec = error "SearchTree: ValueSequence: readsPrec"

instance Unifiable (C_ValueSequence a) where
  (=.=)      = error "SearchTree: ValueSequence: (=.=)"
  (=.<=)     = error "SearchTree: ValueSequence: (=.<=)"
  bind       = error "SearchTree: ValueSequence: bind"
  lazyBind   = error "SearchTree: ValueSequence: lazyBind"

instance NonDet (C_ValueSequence a) where
  choiceCons  = Choice_VS
  choicesCons = Choices_VS
  guardCons   = Guard_VS
  failCons    = error "SearchTree: ValueSequence: failCons"
  try        = error "SearchTree: ValueSequence: try" 
  match      = error "SearchTree: ValueSequence: match" 

instance Generable (C_ValueSequence a) where
  generate = error "SearchTree: ValueSequence: generate"

instance Coverable (C_ValueSequence a) where
  cover   = error "SearchTree: ValueSequence: cover"

instance NormalForm (C_ValueSequence a) where
 ($!!)    = error "SearchTree: ValueSequence: ($!!)"
 ($##)    = error "SearchTree: ValueSequence: ($##)"
 ($!<)    = error "SearchTree: ValueSequence: ($!<)"
 searchNF = error "SearchTree: ValueSequence: searchNF"

external_d_C_emptyVS :: ConstStore -> C_ValueSequence a
external_d_C_emptyVS _ = EmptyVS

external_d_C_addVS :: a -> C_ValueSequence a -> ConstStore -> C_ValueSequence a
external_d_C_addVS x vs _ = Values (Curry_Prelude.OP_Cons x (getValues vs))

external_d_C_failVS :: Curry_Prelude.C_Int -> ConstStore -> C_ValueSequence a 
external_d_C_failVS d@(Curry_Prelude.C_Int d') _
  | d' <# 0#   = Values (Curry_Prelude.OP_List)
  | otherwise  = FailVS d 

external_d_C_vsToList :: C_ValueSequence a -> ConstStore -> Curry_Prelude.OP_List a
external_d_C_vsToList (Values xs)   _ = xs
external_d_C_vsToList (FailVS (Curry_Prelude.C_Int d))    _ = failCons (I# d) defFailInfo
external_d_C_vsToList (Choice_VS cd i x y) cs = choiceCons cd i (external_d_C_vsToList x cs)
                                                                (external_d_C_vsToList y cs)
external_d_C_vsToList (Choices_VS cd i xs) cs = 
  choicesCons cd i (map (flip external_d_C_vsToList cs) xs )
external_d_C_vsToList (Guard_VS cd c x) cs =
  guardCons cd c (external_d_C_vsToList x cs)

(|++|) :: Curry_Prelude.Curry a => C_ValueSequence a -> C_ValueSequence a -> C_ValueSequence a 
EmptyVS            |++| vs = vs
Values xs          |++| vs = Values (Curry_Prelude.d_OP_plus_plus xs (getValues vs) emptyCs)
FailVS d           |++| vs = failSmallest d vs
Choice_VS cd i x y |++| vs = choiceCons cd i (x |++| vs) (y |++| vs) 
Choices_VS cd i xs |++| vs = choicesCons cd i (map (|++| vs) xs)
Guard_VS cd cs xs  |++| vs = guardCons cd cs (xs |++| vs)

getValues EmptyVS              = Curry_Prelude.OP_List
getValues (FailVS _)           = Curry_Prelude.OP_List
getValues (Values xs)          = xs
getValues (Choice_VS cd i x y) = choiceCons cd i (getValues x) (getValues y)
getValues (Choices_VS cd i xs) = choicesCons cd i (map getValues xs)
getValues (Guard_VS cd cs x)   = guardCons cd cs (getValues x)

failSmallest d EmptyVS              = FailVS d
failSmallest d (FailVS d2)          = FailVS (Curry_Prelude.d_C_min d d2 emptyCs)
failSmallest _ vs@(Values xs)       = vs
failSmallest d (Choice_VS cd i x y) = choiceCons cd i (failSmallest d x) (failSmallest d y)
failSmallest d (Choices_VS cd i xs) = choicesCons cd i (map (failSmallest d) xs)
failSmallest d (Guard_VS cd cs x)   = guardCons cd cs (failSmallest d x)

