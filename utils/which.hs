-- ---------------------------------------------------------------------------
-- Utility to retrieve the path to an executable w.r.t. different operating
-- systems.
-- ---------------------------------------------------------------------------
module Main where

import System.Directory   (findExecutable)
import System.Environment (getArgs, getProgName)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [exe] -> do path <- findExecutable exe
                case path of
                  Just p  -> putStrLn $ map forwardifySlashes p
                  Nothing -> exitFailure
    _     -> do prog <- getProgName
                hPutStrLn stderr $ "Bad args: " ++ show args
                hPutStrLn stderr $ "Usage: " ++ prog ++ " <prog>"
                exitFailure

forwardifySlashes :: Char -> Char
forwardifySlashes '\\' = '/'
forwardifySlashes c    = c
