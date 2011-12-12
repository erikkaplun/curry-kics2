{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fforce-recomp #-}
module Debug (trace) where

trace :: String -> IO ()
#ifdef DEBUG
trace msg = putStrLn msg
#else
trace _   = return ()
#endif
