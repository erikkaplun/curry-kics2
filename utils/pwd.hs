-- ---------------------------------------------------------------------------
-- Utility to retrieve the current directory w.r.t. different operating
-- system. For example:
-- 
-- Unix:    /path/to/current/dir
-- Windows: C:/path/to/current/dir
-- ---------------------------------------------------------------------------
module Main where

import System.Directory   (getCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do d <- getCurrentDirectory
             putStrLn $ map forwardifySlashes d
    _  -> do prog <- getProgName
             hPutStrLn stderr $ "Bad args: " ++ show args
             hPutStrLn stderr $ "Usage: " ++ prog
             exitFailure

forwardifySlashes :: Char -> Char
forwardifySlashes '\\' = '/'
forwardifySlashes c    = c
