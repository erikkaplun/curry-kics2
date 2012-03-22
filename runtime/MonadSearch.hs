module MonadSearch where

import ID
import Control.Monad
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict

class MonadPlus m => MonadSearch m where
  szero :: Int -> FailInfo -> m a
  splus :: ID -> m a -> m a -> m a
  ssum  :: ID -> [m a] -> m a
  constrainMSearch :: Constraints -> m a -> m a

  szero _ _ = mzero
  splus _   = mplus
  ssum  _   = msum

instance MonadSearch m => MonadSearch (Lazy.StateT a m) where
  splus i mx my = Lazy.StateT (\s -> splus i (Lazy.runStateT mx s) (Lazy.runStateT my s))
  ssum  i mxs   = Lazy.StateT (\s -> ssum i (map (flip Lazy.runStateT s) mxs))
  szero cd info = Lazy.StateT (\_ -> szero cd info)
  constrainMSearch cs mx = Lazy.StateT (\s -> constrainMSearch cs (Lazy.runStateT mx s))

instance MonadSearch m => MonadSearch (Strict.StateT a m) where
  splus i mx my = Strict.StateT (\s -> splus i (Strict.runStateT mx s) (Strict.runStateT my s))
  ssum  i mxs   = Strict.StateT (\s -> ssum i (map (flip Strict.runStateT s) mxs))
  szero cd info = Strict.StateT (\_ -> szero cd info)
  constrainMSearch cs mx = Strict.StateT (\s -> constrainMSearch cs (Strict.runStateT mx s))
