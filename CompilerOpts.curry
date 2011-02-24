------------------------------------------------------------------------------
--- Compiler options for the ID-based curry compiler
---
--- @author Fabian Reck, Björn Peemöller
--- @version February 2011
------------------------------------------------------------------------------
module CompilerOpts
  ( Options (..), SearchMode (..), Dump (..), defaultOptions, compilerOpts
  ) where

import IO (hPutStrLn, stderr)
import List (nub)
import Maybe (fromMaybe)
import System (exitWith, getArgs, getProgName)

import GetOpt

type Options =
  { optQuiet           :: Bool       -- quiet mode
  , optVersion         :: Bool       -- show version
  , optHelp            :: Bool       -- show usage
  , optSearchMode      :: SearchMode -- search mode
  , optDetOptimization :: Bool       -- optimization for deterministic functions
  , optDump            :: [Dump]     -- dump intermediate results
  , optXNoImplicitPrelude :: Bool
  }

data SearchMode
  = NoSearch -- no search
  | DFS      -- depth first search
  | BFS      -- bredth first search
  | IterDFS  -- iterative depth first search
  | PAR      -- parallel search

data Dump
  = DumpFlat        -- dump flat curry
  | DumpLifted      -- dump flat curry after case lifting
  | DumpRenamed     -- dump renamed flat curry
  | DumpFunDecls    -- dump transformed function declarations
  | DumpTypeDecls   -- dump transformed type declarations
  | DumpAbstractHs  -- dump abstract Haskell

defaultOptions :: Options
defaultOptions =
  { optQuiet           = False
  , optVersion         = False
  , optHelp            = False
  , optSearchMode      = NoSearch
  , optDetOptimization = True
  , optDump            = []
  , optXNoImplicitPrelude = False
  }

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['q'] ["quiet"]
      (NoArg (\opts -> { optQuiet   := True | opts }))
      "run in quiet mode"
  , Option ['v'] ["version"]
      (NoArg (\opts -> { optVersion := True | opts }))
      "show version number"
  , Option "h?"  ["help"]
      (NoArg (\opts -> { optHelp    := True | opts }))
      "show usage information"
  , Option ['s'] ["search-mode"]
      (ReqArg (\arg opts -> { optSearchMode := fromMaybe
        (opts -> optSearchMode) (lookup arg searchModes) | opts } )
      "SEARCHMODE")
      "set search mode, one of [DFS, BFS, IterDFS, PAR]"
  , Option [] ["no-opt"]
      (NoArg (\opts -> { optDetOptimization := False | opts } ))
      "disable optimization for deterministic functions"
  , Option [] ["dump-flat"]
      (NoArg (\opts -> { optDump := nub (DumpFlat : opts -> optDump) | opts }))
      "dump flat curry representation"
  , Option [] ["dump-lifted"]
      (NoArg (\opts -> { optDump := nub (DumpLifted : opts -> optDump) | opts }))
      "dump flat curry after case lifting"
  , Option [] ["dump-abstract-hs"]
      (NoArg (\opts -> { optDump := nub (DumpAbstractHs : opts -> optDump) | opts }))
      "dump abstract Haskell representation"
  , Option [] ["dump-fun-decls"]
      (NoArg (\opts -> { optDump := nub (DumpFunDecls : opts -> optDump) | opts }))
      "dump transformed function declarations"
  , Option [] ["dump-type-decls"]
      (NoArg (\opts -> { optDump := nub (DumpTypeDecls : opts -> optDump) | opts }))
      "dump transformed type declarations"
  , Option [] ["dump-renamed"]
      (NoArg (\opts -> { optDump := nub (DumpRenamed : opts -> optDump) | opts }))
      "dump renamed abstract Haskell representation"
  , Option [] ["dump-all"]
      (NoArg (\opts -> { optDump := [DumpFlat, DumpLifted, DumpRenamed, DumpFunDecls, DumpTypeDecls, DumpAbstractHs] | opts }))
      "dump all intermediate results"
  , Option ['x'] ["x-no-implicit-prelude"]
      (NoArg (\opts -> { optXNoImplicitPrelude := True | opts }))
      "do not implicitly import Prelude"
  ]

searchModes :: [(String, SearchMode)]
searchModes =
  [ ("DFS", DFS)
  , ("BFS", BFS)
  , ("IterDFS", IterDFS)
  , ("PAR", PAR)
  ]

versionString :: String
versionString = "ID-based Curry -> Haskell Compiler (Version of 23/02/2011)"

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
  hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information"
  exitWith 1
badUsage prog (err:errs) = hPutStrLn stderr err >> badUsage prog errs

compilerOpts :: IO (Options, [String])
compilerOpts = do
  args <- getArgs
  prog <- getProgName
  processOpts prog $ parseOpts args

processOpts :: String -> (Options, [String], [String]) -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | opts -> optHelp    = printUsage prog
  | opts -> optVersion = printVersion
  | not (null errs')   = badUsage prog errs'
  | otherwise          = return (opts, files)
    where errs' = errs ++ checkOpts opts files
