module Debug (trace) where

import Control.Monad (when)

inDebugMode :: Bool
inDebugMode = False

{-# INLINE trace #-}
trace :: String -> IO ()
trace msg = when inDebugMode $ putStrLn msg