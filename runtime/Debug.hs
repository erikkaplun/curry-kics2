{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module Debug  where

trace :: String -> IO ()
#ifdef DEBUG
trace msg = putStrLn msg
#else
trace _   = return ()
#endif

internalError :: String -> a
internalError = error . ("Internal error: " ++)
