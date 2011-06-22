
import System.IO
import Control.Monad
import Control.Parallel.TreeSearch

instance Monad C_SearchTree where
  return = C_Value

  C_Fail    >>= _ = C_Fail
  C_Value x >>= f = f x
  C_Or x y  >>= f = C_Or (x >>= f) (y >>= f)

instance MonadPlus C_SearchTree where
  mzero = C_Fail
  mplus = C_Or

external_d_C_someSearchTree :: (NormalForm a,NonDet a) => a -> C_SearchTree a 
external_d_C_someSearchTree = searchMPlus

