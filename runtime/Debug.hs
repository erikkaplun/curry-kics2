{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module Debug (internalError, nondetError, trace) where

import Control.Exception (throw)

trace :: String -> IO ()
#ifdef DEBUG
trace msg = putStrLn msg
#else
trace _   = return ()
#endif

internalError :: String -> a
internalError = error . ("Internal error: " ++)

nondetError :: String -> a
nondetError = throw . userError
