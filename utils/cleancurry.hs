module Main where

import Control.Monad
import Data.List (intercalate)
import System.Console.GetOpt
import System.Directory
import System.FilePath
import System.IO     (hPutStrLn, stderr)
import System.Environment (getArgs, getProgName)
import System.Exit

version :: String
version = "0.1"

debug :: Bool
debug = False

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
badUsage prog []         = do
  hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information"
  exitFailure
badUsage prog (err:errs) = hPutStrLn stderr err >> badUsage prog errs

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

cleancurry :: Bool -> [String] -> String -> IO ()
cleancurry False []   dir = cleanAll dir
cleancurry False mdls dir = mapM_ (cleanModule dir) mdls
cleancurry True  []   dir = do
  cleanAll dir
  getSubDirs dir >>= mapM_ (cleancurry True [])
cleancurry True  mdls dir = do
  mapM_ (cleanModule dir) mdls
  let filtered = filter hasNoPath mdls
  unless (null filtered) $ getSubDirs dir >>= mapM_ (cleancurry True filtered)

cleanAll :: String -> IO ()
cleanAll dir = do
  cyModules <- findCurryModules dir
  mapM_ (cleanModule dir) (map dropExtension cyModules)

cleanModule :: String -> String -> IO ()
cleanModule dir mdl = do
  let base   = if hasNoPath mdl then dir else takeDirectory mdl
  let cydir  = base  </> ".curry"
      subdir = cydir </> "kics2"
  sdExists <- doesDirectoryExist subdir
  when sdExists $ do
    removeFiles $ map (subdir </>) mainFiles
    removeFiles $ map ((subdir </> "Curry_" ++ mdl) <.>) kics2Exts
    rmdirIfEmpty subdir
  cyExists <- doesDirectoryExist cydir
  when cyExists $ do
    removeFiles $ map ((cydir </> mdl) <.>) cyExts
    rmdirIfEmpty cydir

mainFiles :: [String]
mainFiles = ["Main", "Main.hs", "Main.hi", "Main.o"]

kics2Exts :: [String]
kics2Exts = ["hs", "hi", "o", "nda", "info"]

cyExts :: [String]
cyExts = ["fcy", "fint", "acy", "uacy"]

hasNoPath :: String -> Bool
hasNoPath = (== ".") . takeDirectory

removeFiles :: [String] -> IO ()
removeFiles files | debug     = putStrLn $ "Would remove files " ++ intercalate " " files
                  | otherwise = mapM_ tryRemove files
  where
  tryRemove f = do
    exists <- doesFileExist f
    when exists $ removeFile f

rmdirIfEmpty :: String -> IO ()
rmdirIfEmpty dir = do
  exists <- doesDirectoryExist dir
  when exists $ do
    cnts <- getUsefulContents dir
    when (null cnts) $ if debug
      then putStrLn $ "Would remove directory " ++ dir
      else removeDirectory dir

getSubDirs :: String -> IO [String]
getSubDirs curdir = do
  cnts <- getUsefulContents curdir
  filterM isDirectory $ map (curdir </>) cnts

findCurryModules :: String -> IO [String]
findCurryModules dir = do
  cnts <- getUsefulContents dir
  return $ filter ((`elem` [".curry", ".lcurry"]) . takeExtension) cnts

isDirectory :: FilePath -> IO Bool
isDirectory = liftM searchable . getPermissions

getUsefulContents :: FilePath -> IO [String]
getUsefulContents path = do
    names <- getDirectoryContents path
    return (filter (`notElem` [".", ".."]) names)
