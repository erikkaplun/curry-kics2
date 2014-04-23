{- |
    Module      :  $Header$
    Description :  cleancurry binary
    Copyright   :  2012 - 2013 Björn Peemöller
    License     :  OtherLicense

    Maintainer  :  bjp@informatik.uni-kiel.de
    Stability   :  stable
    Portability :  portable

    Command line tool for cleaning up Curry directories.
-}
module Main where

import Control.Exception as E (catch, throwIO)
import Control.Monad          (filterM, liftM, when)
import System.Console.GetOpt
import System.Directory
import System.FilePath
import System.IO              (hPutStrLn, stderr)
import System.IO.Error        (isDoesNotExistError, isPermissionError)
import System.Environment     (getArgs, getProgName)
import System.Exit            (exitFailure, exitSuccess)

version :: String
version = "0.2"

-- |cleancurry options
data Options = Options
  { optHelp      :: Bool -- show usage and exit
  , optVersion   :: Bool -- show version and exit
  , optRecursive :: Bool -- clean all subdirectories recursively
  }

defaultOptions :: Options
defaultOptions = Options
  { optHelp      = False
  , optVersion   = False
  , optRecursive = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h', '?'] ["help"]
      (NoArg (\opts -> opts { optHelp      = True }))
      "show usage information"
  , Option ['V'] ["version"]
      (NoArg (\opts -> opts { optVersion   = True }))
      "show version number"
  , Option ['r'] ["recursive"]
      (NoArg (\opts -> opts { optRecursive = True }))
      "clean recursively"
  ]

parseOpts :: [String] -> (Options, [String], [String])
parseOpts args = (foldl (flip ($)) defaultOptions opts, files, errs)
  where (opts, files, errs) = getOpt Permute options args

printVersion :: String -> IO a
printVersion prog = do
  putStrLn $ prog ++ ' ' : version
  exitSuccess

printUsage :: String -> IO a
printUsage prog = do
  putStrLn $ usageInfo header options
  exitSuccess
    where header = "usage: " ++ prog ++ " [OPTION] ... MODULE ..."

badUsage :: String -> [String] -> IO a
badUsage prog errs = do
  mapM_ (hPutStrLn stderr) errs
  hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information"
  exitFailure

processOpts :: String -> (Options, [String], [String])
            -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | optHelp    opts = printUsage prog
  | optVersion opts = printVersion prog
  | not (null errs) = badUsage prog errs
  | otherwise       = return (opts, files)

getOpts :: IO (Options, [String])
getOpts = do
  args <- getArgs
  prog <- getProgName
  processOpts prog $ parseOpts args

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  (opts, files) <- getOpts
  dir <- getCurrentDirectory
  cleancurry (optRecursive opts) (map dropExtension files) dir

cleancurry :: Bool     -- clean recursively?
           -> [String] -- Modules to clean for ([] means all modules)
           -> FilePath -- directory to clean
           -> IO ()
cleancurry rec []   dir = do
  mapM_ (cleanModule dir) =<< allModules dir
  when rec $ getSubDirs dir >>= mapM_ (cleancurry rec [])
cleancurry rec mdls dir = do
  mapM_ (cleanModule dir) mdls
  let filtered = filter hasNoPath mdls
  when (rec && not (null filtered)) $
    getSubDirs dir >>= mapM_ (cleancurry rec filtered)

-- Retrieve all Curry modules in the given directory
allModules :: String -> IO [String]
allModules dir = map dropExtension `liftM` findCurryModules dir

cleanModule :: String -> String -> IO ()
cleanModule dir mdl = do
  let base   = if hasNoPath mdl then dir else takeDirectory mdl
  let cydir  = base  </> ".curry"
      subdir = cydir </> "kics2"
  sdExists <- doesDirectoryExist subdir
  when sdExists $ do
    removeFiles $ map (subdir </>) mainFiles
    removeFiles $ map ((subdir </> "Curry_" ++ mdl) <.>) kics2Exts
    removeFiles $ map ((subdir </> "Curry_Trace_" ++ mdl) <.>) kics2Exts
    rmdirIfEmpty subdir
  cyExists <- doesDirectoryExist cydir
  when cyExists $ do
    removeFiles $ map ((cydir </> mdl) <.>) cyExts
    rmdirIfEmpty cydir

mainFiles :: [String]
mainFiles = ["Main", "Main.hs", "Main.hi", "Main.o"]

kics2Exts :: [String]
kics2Exts = ["hs", "hi", "o", "nda", "info"]

srcExts :: [String]
srcExts = [".curry", ".lcurry"]

cyExts :: [String]
cyExts = ["fcy", "fint", "acy", "uacy", "icurry"]

hasNoPath :: String -> Bool
hasNoPath = (== ".") . takeDirectory

removeFiles :: [String] -> IO ()
removeFiles files = filterM doesFileExist files >>= mapM_ removeFile

rmdirIfEmpty :: String -> IO ()
rmdirIfEmpty dir = do
  exists <- doesDirectoryExist dir
  when exists $ do
    cnts <- getUsefulContents dir
    when (null cnts) $ removeDirectory dir

getSubDirs :: String -> IO [String]
getSubDirs curdir = do
  cnts <- getUsefulContents curdir
  filterM isDirectory $ map (curdir </>) cnts

findCurryModules :: String -> IO [String]
findCurryModules dir = filter ((`elem` srcExts) . takeExtension)
                       `liftM` getUsefulContents dir

isDirectory :: FilePath -> IO Bool
isDirectory f = E.catch (searchable `liftM` getPermissions f) handler
  where
  handler :: IOError -> IO Bool
  handler e | isDoesNotExistError e = return False
            | isPermissionError   e = return False
            | otherwise             = E.throwIO e

getUsefulContents :: FilePath -> IO [String]
getUsefulContents dir = filter (`notElem` [".", ".."])
                        `liftM` getDirectoryContents dir
