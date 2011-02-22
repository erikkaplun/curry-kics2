module Main where

import System.Environment (getArgs)
import System (system)
import System.FilePath

main :: IO ()
main = do
  args <- getArgs
  mapM_ test args
  rm "*.hi *.o"
  rm "Curry_*.hs"

test :: String -> IO ()
test prog = do
  ecOpt <- system callOpt
  ecNoOpt <- system callNoOpt
  putStrLn $ file ++ " +O: " ++ show ecOpt
  putStrLn $ file ++ " -O: " ++ show ecNoOpt
  where
  callOpt = unwords [idcCall True file, "&&", ghcCall file, "2>/dev/null"]
  callNoOpt = unwords [idcCall False file, "&&", ghcCall file, "2>/dev/null"]
  file = takeBaseName prog

rm :: String -> IO ()
rm pattern = system ("rm -f " ++ pattern) >> return ()

idcCall :: Bool -> String -> String
idcCall True prog  = unwords [idc, prog]
idcCall False prog = unwords [idc, "--no-opt", prog]

idc :: String
idc = "../idc -q --x-no-implicit-prelude"

ghcCall :: String -> String
ghcCall prog = "ghc -i.. --make " ++ hsFile prog

hsFile :: String -> String
hsFile = ("Curry_" ++)
