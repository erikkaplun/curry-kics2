-- ---------------------------------------------------------------------------
-- Utility to retrieve the cabal installation directory.
-- For example:
--
-- Unix:    ~/.cabal
-- Windows: C:\Users\<username>\AppData\Roaming\cabal
-- ---------------------------------------------------------------------------
module Main where

import System.Directory   (getAppUserDataDirectory)
import System.Environment (getArgs, getProgName)
import System.Exit        (exitFailure)
import System.IO          (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> do d <- getAppUserDataDirectory "cabal"
             putStrLn $ map forwardifySlashes d
    _  -> do prog <- getProgName
             hPutStrLn stderr $ "Bad args: " ++ show args
             hPutStrLn stderr $ "Usage: " ++ prog
             exitFailure

forwardifySlashes :: Char -> Char
forwardifySlashes '\\' = '/'
forwardifySlashes c    = c
