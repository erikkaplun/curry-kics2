------------------------------------------------------------------------------
--- Compiler options for the ID-based curry compiler
---
--- @author Fabian Reck, Bjoern Peemoeller
--- @version June 2011
------------------------------------------------------------------------------
module CompilerOpts
  ( Options (..), Verbosity (..), OptimLevel (..), DumpFormat (..)
  , Extension (..), defaultOptions, debugOptions, compilerOpts
  ) where

import Char (toLower)
import FileGoodies (splitPath)
import IO (hPutStrLn, stderr)
import List (nub)
import Maybe (fromMaybe)
import System (exitWith, getArgs, getProgName)

import GetOpt
import Installation (compilerName, majorVersion, minorVersion, compilerDate)

-- |Compiler options
type Options =
  { optHelp               :: Bool         -- show usage and exit
  , optVersion            :: Bool         -- show version and exit
  , optVerbosity          :: Verbosity    -- verbosity level
  , optForce              :: Bool         -- force recompilation
  , optImportPaths        :: [String]     -- directories searched for imports
  , optOutputSubdir       :: String       -- subdirectory for compiled modules
  , optOptimization       :: OptimLevel   -- level of optimization
  , optDump               :: [DumpFormat] -- dump intermediate results
  , optExtensions         :: [Extension]  -- language extensions
  }

-- |Verbosity levels of the compiler
data Verbosity
  = VerbQuiet    -- be quiet
  | VerbStatus   -- show compilation status
  | VerbFrontend -- additionally show frontend infos
  | VerbAnalysis -- additionally show analysis infos
  | VerbDetails  -- additionally show details

-- |Dump formats of the compiler
data DumpFormat
  = DumpFlat        -- dump flat curry
  | DumpTypedFlat   -- dump typed flat curry
  | DumpLifted      -- dump flat curry after case lifting
  | DumpEliminated  -- dump flat curry after cond elimination
  | DumpDefaulted   -- dump flat curry after defaulting
  | DumpRenamed     -- dump renamed flat curry
  | DumpFunDecls    -- dump transformed function declarations
  | DumpTypeDecls   -- dump transformed type declarations
  | DumpAbstractHs  -- dump abstract Haskell

-- |Levels of optimization
data OptimLevel
  = OptimNone         -- no optimization
  | OptimHigherOrder  -- higher-order optimization
  | OptimStrictSupply -- strict evaluation of supplies

-- Known language extensions
data Extension
  = ExtNoImplicitPrelude
  | ExtUnknown String

allDumps :: [DumpFormat]
allDumps = [ DumpFlat, DumpTypedFlat, DumpLifted, DumpEliminated, DumpDefaulted
           , DumpRenamed, DumpFunDecls, DumpTypeDecls, DumpAbstractHs]

defaultOptions :: Options
defaultOptions =
  { optHelp         := False
  , optVersion      := False
  , optVerbosity    := VerbStatus
  , optForce        := False
  , optImportPaths  := []
  , optOutputSubdir := ".curry/kics2/"
  , optOptimization := OptimStrictSupply
  , optDump         := []
  , optExtensions   := []
  }

debugOptions = { optVerbosity := VerbDetails
               , optForce := True | defaultOptions }

parseVerbosity :: String -> Verbosity -> Verbosity
parseVerbosity s v = case s of
  "0" -> VerbQuiet
  "1" -> VerbStatus
  "2" -> VerbFrontend
  "3" -> VerbAnalysis
  "4" -> VerbDetails
  _   -> v

parseOptimization :: String -> OptimLevel -> OptimLevel
parseOptimization s o = case s of
  "0" -> OptimNone
  "1" -> OptimHigherOrder
  "2" -> OptimStrictSupply
  _   -> o

parseExtension :: String -> Extension
parseExtension s = case map toLower s of
  "noimplicitprelude" -> ExtNoImplicitPrelude
  _                   -> ExtUnknown s

options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h', '?'] ["help"]
      (NoArg (\opts -> { optHelp      := True | opts }))
      "show usage information"
  , Option ['V'] ["version"]
      (NoArg (\opts -> { optVersion   := True | opts }))
      "show version number"
  , Option ['v'] ["verbosity"]
      (ReqArg (\arg opts -> { optVerbosity :=
        parseVerbosity arg (opts :> optVerbosity) | opts }) "<n>")
      ("set verbosity (0 = quiet, 1 = + status, 2 = + frontend" ++
       ", 3 = + nd-analysis, 4 = + dump-all)")
  , Option ['q'] ["quiet"]
      (NoArg (\opts -> { optVerbosity := VerbQuiet | opts }))
      "run in quiet mode"
  , Option ['f'] ["force"]
      (NoArg (\opts -> { optForce     := True | opts }))
      "force recompilation"
  , Option ['i'] ["import-dir"]
      (ReqArg (\arg opts -> { optImportPaths :=
        nub (opts :> optImportPaths ++ splitPath arg) | opts }) "DIR")
      "search for imports in DIR"
  , Option ['o'] ["output-subdir"]
      (ReqArg (\arg opts -> { optOutputSubdir := arg | opts }) "SUBDIR")
      "output compiled modules to SUBDIR"
  , Option ['O'] ["optimization"]
      (ReqArg (\arg opts -> { optOptimization :=
        parseOptimization arg (opts :> optOptimization) | opts }) "<n>")
      "set optimization level (0 = none, 1 = higher order, 2 = strict supply evaluation (default))"
  , Option [] ["no-opt"]
      (NoArg (\opts -> { optOptimization := OptimNone | opts } ))
      "disable optimization"
  , Option [] ["dump-flat"]
      (NoArg (\opts -> { optDump :=
        nub (DumpFlat : opts :> optDump) | opts }))
      "dump flat curry representation"
  , Option [] ["dump-typed-flat"]
      (NoArg (\opts -> { optDump :=
        nub (DumpTypedFlat : opts :> optDump) | opts }))
      "dump flat curry representation after type inference"
  , Option [] ["dump-lifted"]
      (NoArg (\opts -> { optDump :=
        nub (DumpLifted : opts :> optDump) | opts }))
      "dump flat curry after case lifting"
  , Option [] ["dump-elim"]
      (NoArg (\opts -> { optDump :=
        nub (DumpEliminated : opts :> optDump) | opts}))
      "dump flat curry after cond elimination"
  , Option [] ["dump-defaulted"]
      (NoArg (\opts -> { optDump :=
        nub (DumpDefaulted : opts :> optDump) | opts}))
      "dump flat curry after defaulting locally polymorphic sub-expressions"
  , Option [] ["dump-abstract-hs"]
      (NoArg (\opts -> { optDump :=
        nub (DumpAbstractHs : opts :> optDump) | opts }))
      "dump abstract Haskell representation"
  , Option [] ["dump-fun-decls"]
      (NoArg (\opts -> { optDump :=
        nub (DumpFunDecls : opts :> optDump) | opts }))
      "dump transformed function declarations"
  , Option [] ["dump-type-decls"]
      (NoArg (\opts -> { optDump :=
        nub (DumpTypeDecls : opts :> optDump) | opts }))
      "dump transformed type declarations"
  , Option [] ["dump-renamed"]
      (NoArg (\opts -> { optDump :=
        nub (DumpRenamed : opts :> optDump) | opts }))
      "dump renamed abstract Haskell representation"
  , Option [] ["dump-all"]
      (NoArg (\opts -> { optDump := allDumps | opts }))
      "dump all intermediate results"
  , Option ['X'] []
      (ReqArg (\arg opts -> { optExtensions :=
        nub (parseExtension arg : opts :> optExtensions) | opts }) "EXT")
      "enable language extension EXT"
  ]

versionString :: String
versionString = concat
  [ compilerName
  , " (Version " ++ show majorVersion ++ '.' : show minorVersion
  , " of " ++ compilerDate ++ ")"
  ]

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

processOpts :: String -> (Options, [String], [String])
            -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | opts :> optHelp    = printUsage prog
  | opts :> optVersion = printVersion
  | not (null errs')   = badUsage prog errs'
  | otherwise          = return (opts, files)
    where errs' = errs ++ checkOpts opts files
