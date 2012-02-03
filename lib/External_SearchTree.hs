
import System.IO
import Control.Monad
import Control.Parallel.TreeSearch
import MonadSearch

instance Monad C_SearchTree where
  return = C_Value

  C_Fail    >>= _ = C_Fail
  C_Value x >>= f = f x
  C_Or x y  >>= f = C_Or (x >>= f) (y >>= f)

  Choice_C_SearchTree i x y >>= f = Choice_C_SearchTree i (x >>= f) (y >>= f)
  Choices_C_SearchTree i xs >>= f = Choices_C_SearchTree i (map (>>= f) xs)
  

instance MonadPlus C_SearchTree where
  mzero = C_Fail
  mplus = C_Or

instance MonadSearch C_SearchTree where
  splus = Choice_C_SearchTree
  ssum  = Choices_C_SearchTree

external_d_C_someSearchTree :: NormalForm a => a -> ConstStore -> C_SearchTree a
external_d_C_someSearchTree = encapsulatedSearch

