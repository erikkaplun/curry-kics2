module CompilerOpts (compilerOpts, Options (..)) where

import GetOpt
import SearchMode
import Maybe (fromMaybe)
import System (exitWith, getArgs, getProgName)

type Options =
  { quiet      :: Bool       -- quiet mode
  , version    :: Bool       -- show version
  , help       :: Bool       -- show usage
  , searchMode :: SearchMode -- search mode
  , hoOpt      :: Bool       -- higher order optimization
  }

defaultOptions :: Options
defaultOptions =
  { quiet = False
  , version = False
  , help = False
  , searchMode = NoSearch
  , hoOpt = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['q'] ["quiet"]
      (NoArg (\opts -> { quiet   := True | opts }))
      "run in quiet mode"
  , Option ['v'] ["version"]
      (NoArg (\opts -> { version := True | opts }))
      "show version number"
  , Option ['h'] ["help"]
      (NoArg (\opts -> { help    := True | opts }))
      "show usage information"
  , Option ['s'] ["search-mode"]
      (ReqArg (\arg opts -> { searchMode := fromMaybe (opts -> searchMode)
                              (lookup arg searchModes) | opts } )
      "SEARCHMODE")
      "set search mode, one of [DFS, BFS, IterDFS, PAR]"
  , Option [] ["HO"]
      (NoArg (\opts -> { hoOpt := True | opts } ))
      "enable higher-order optimizaton"
  ]

searchModes :: [(String, SearchMode)]
searchModes =
  [ ("DFS", DFS)
  , ("BFS", BFS)
  , ("IterDFS", IterDFS)
  , ("PAR", PAR)
  ]

versionString :: String
versionString = "ID-based Curry -> Haskell Compiler (Version of 08/02/11)"

parseOpts :: [String] -> (Options, [String], [String])
parseOpts args = (foldl (flip ($)) defaultOptions opts, files, errs)
  where (opts, files, errs) = getOpt Permute options args

checkOpts :: Options -> [String] -> [String]
checkOpts _ []    = ["no files"]
checkOpts _ (_:_) = []

printVersion :: IO a
printVersion = do
  putStrLn versionString
  exitWith 0

printUsage :: String -> IO a
printUsage prog = do
  putStrLn $ usageInfo header options
  exitWith 0
    where header = "usage: " ++ prog ++ " [OPTION] ... MODULE ..."

badUsage :: String -> [String] -> IO a
badUsage prog [] = do
  putStrLn $ "Try '" ++ prog ++ " --help' for more information"
  exitWith 1
badUsage prog (err:errs) = putStrLn err >> badUsage prog errs

compilerOpts :: IO (Options, [String])
compilerOpts = do
  args <- getArgs
  prog <- getProgName
  processOpts prog $ parseOpts args

processOpts :: String -> (Options,[String],[String]) -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | opts -> help    = printUsage prog
  | opts -> version = printVersion
  | otherwise = do
                  let errs' = errs ++ checkOpts opts files
                  if null errs'
                    then return (opts, files)
                    else badUsage prog errs'
