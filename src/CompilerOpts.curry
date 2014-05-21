------------------------------------------------------------------------------
--- Compiler options for the ID-based curry compiler
---
--- @author Fabian Reck, Bjoern Peemoeller
--- @version April 2014
------------------------------------------------------------------------------
{-# LANGUAGE Records #-}
module CompilerOpts
  ( Options (..), Verbosity (..), OptimLevel (..), DumpFormat (..)
  , Extension (..), defaultOptions, getCompilerOpts
  ) where

import List         (intercalate, maximum, nub)
import FilePath     ((</>), splitSearchPath)
import GetOpt
import IO           (hPutStrLn, stderr)
import System       (exitWith, getArgs, getProgName)

import Installation (compilerName, majorVersion, minorVersion, compilerDate)

--- Version string
version :: String
version = concat
  [ compilerName
  , " (Version " ++ show majorVersion ++ '.' : show minorVersion
  , " of " ++ compilerDate ++ ")"
  ]

--- Compiler options
type Options =
  { optHelp               :: Bool         -- show usage and exit
  , optVersion            :: Bool         -- show version and exit
  , optVerbosity          :: Verbosity    -- verbosity level
  , optForce              :: Bool         -- force recompilation
  , optImportPaths        :: [String]     -- directories searched for imports
  , optOutputSubdir       :: String       -- subdirectory for compiled modules
  , optOptimization       :: OptimLevel   -- level of optimization
  , optExtensions         :: [Extension]  -- language extensions
  , optDump               :: [DumpFormat] -- dump intermediate results
  , optParser             :: String       -- additional front-end options
  , optTraceFailure       :: Bool         -- trace failures
  }

--- Default compiler options
defaultOptions :: Options
defaultOptions =
  { optHelp         := False
  , optVersion      := False
  , optVerbosity    := VerbStatus
  , optForce        := False
  , optImportPaths  := []
  , optOutputSubdir := ".curry" </> "kics2"
  , optOptimization := OptimStrictSupply
  , optExtensions   := []
  , optDump         := []
  , optParser       := ""
  , optTraceFailure := False
  }

--- Verbosity levels of the compiler
data Verbosity
  = VerbQuiet    -- be quiet
  | VerbFrontend -- show frontend status
  | VerbStatus   -- show own compilation status
  | VerbAnalysis -- additionally show analysis infos
  | VerbDetails  -- additionally show current transformation for each module

--- Description and flag of verbosities
verbosities :: [(Verbosity, String, String)]
verbosities = [ (VerbQuiet   , "0", "quiet"           )
              , (VerbFrontend, "1", "frontend status" )
              , (VerbStatus  , "2", "own status"      )
              , (VerbAnalysis, "3", "analysis"        )
              , (VerbDetails , "4", "details"         )
              ]

--- Levels of optimization
data OptimLevel
  = OptimNone         -- no optimization
  | OptimHigherOrder  -- higher-order optimization
  | OptimStrictSupply -- strict evaluation of supplies

--- Description and flag of optimization levels
optimizations :: [(OptimLevel, String, String)]
optimizations = [ (OptimNone        , "0", "no optimization"                   )
                , (OptimHigherOrder , "1", "higher order"                      )
                , (OptimStrictSupply, "2", "strict supply evaluation (default)")
                ]

--- Known language extensions
data Extension
  = AnonFreeVars       -- anonymous free variables
  | FunctionalPatterns -- functional patterns
  | NoImplicitPrelude  -- no implicit import of the prelude
  | Records            -- record syntax

--- Description and flag of language extensions
extensions :: [(Extension, String, String)]
extensions =
  [ ( AnonFreeVars      , "AnonFreeVars"
    , "enable anonymous free variables"     )
  , ( FunctionalPatterns, "FunctionalPatterns"
    , "enable functional patterns"          )
  , ( NoImplicitPrelude , "NoImplicitPrelude"
    , "do not implicitly import the Prelude")
  , ( Records           , "Records"
    , "enable record syntax"                )
  ]

--- Dump formats of the compiler
data DumpFormat
  = DumpFlat        -- dump flat curry
  | DumpTypedFlat   -- dump typed flat curry
  | DumpExtImports  -- dump typed flat curry with extended import list
  | DumpLifted      -- dump flat curry after case lifting
  | DumpEliminated  -- dump flat curry after cond elimination
  | DumpDefaulted   -- dump flat curry after defaulting
  | DumpRenamed     -- dump renamed flat curry
  | DumpFunDecls    -- dump transformed function declarations
  | DumpTypeDecls   -- dump transformed type declarations
  | DumpTranslated  -- dump abstract Haskell

--- Description and flag of dump levels
dumpLevel :: [(DumpFormat, String, String)]
dumpLevel =
  [ (DumpFlat      , "dump-flat"      , "FlatCurry"                     )
  , (DumpTypedFlat , "dump-typed"     , "result of type inference"      )
  , (DumpExtImports, "dump-imports"   , "result of completing imports"  )
  , (DumpLifted    , "dump-lifted"    , "result of case lifting"        )
  , (DumpEliminated, "dump-condelim"  , "result of cond elimination"    )
  , (DumpDefaulted , "dump-defaulted" , "result of type defaulting"     )
  , (DumpRenamed   , "dump-renamed"   , "result of renaming"            )
  , (DumpFunDecls  , "dump-fun-decls" , "result of function translation")
  , (DumpTypeDecls , "dump-type-decls", "result of type translation"    )
  , (DumpTranslated, "dump-trans"     , "result of entire translation"  )
  ]

-- -----------------------------------------------------------------------------
-- Helper functions.
--
-- Because some flags require additional arguments, the structure is slightly
-- more complicated to enable malformed arguments to be reported.
-- -----------------------------------------------------------------------------

type OptErr = (Options, [String])

type OptErrTable = [(String, String, Options -> Options)]

onOpts :: (Options -> Options) -> OptErr -> OptErr
onOpts f (opts, errs) = (f opts, errs)

onOptsArg :: (String -> Options -> Options) -> String -> OptErr -> OptErr
onOptsArg f arg (opts, errs) = (f arg opts, errs)

addErr :: String -> OptErr -> OptErr
addErr err (opts, errs) = (opts, errs ++ [err])

mkOptErrOption :: String -> [String] -> String -> String -> OptErrTable
               -> OptDescr (OptErr -> OptErr)
mkOptErrOption flags longFlags arg what tbl = Option flags longFlags
  (ReqArg (parseOptErr what tbl) arg)
  ("set " ++ what ++ " `" ++ arg ++ "', where `" ++ arg ++ "' is one of\n"
    ++ renderOptErrTable tbl)

parseOptErr :: String -> OptErrTable -> String -> OptErr -> OptErr
parseOptErr what table opt = case lookup3 opt table of
  Just f  -> onOpts f
  Nothing -> addErr $ "unrecognized " ++ what ++ '`' : opt ++ "'\n"
 where
  lookup3 _ []                  = Nothing
  lookup3 k ((k', _, v2) : kvs)
    | k == k'                   = Just v2
    | otherwise                 = lookup3 k kvs

renderOptErrTable :: OptErrTable -> String
renderOptErrTable ds
  = intercalate "\n" $ map (\(k, d, _) -> rpad maxLen k ++ ": " ++ d) ds
  where
  maxLen = maximum $ map (\(k, _, _) -> length k) ds
  rpad n x = x ++ replicate (n - length x) ' '

-- -----------------------------------------------------------------------------
-- Specification of the command line options
-- -----------------------------------------------------------------------------

--- Command line options
options :: [OptDescr (OptErr -> OptErr)]
options =
  [ Option ['h', '?'] ["help"]
      (NoArg (onOpts $ \opts -> { optHelp      := True | opts }))
      "display this help and exit"
  , Option ['V'] ["version"]
      (NoArg (onOpts $ \opts -> { optVersion   := True | opts }))
      "show the version number and exit"
  , mkOptErrOption "v" ["verbosity"] "n" "verbosity level" verbDescriptions
  , Option ['f'] ["force"]
      (NoArg (onOpts $ \opts -> { optForce     := True | opts }))
      "force recompilation of target files"
  , Option ['i'] ["import-dir"]
      (ReqArg (onOptsArg $ \arg opts -> { optImportPaths := nub
        (opts :> optImportPaths ++ splitSearchPath arg) | opts }) "dir[:dir]")
      "search for imports in `dir[:dir]'"
  , Option ['o'] ["output-subdir"]
      (ReqArg (onOptsArg $ \arg opts ->
        { optOutputSubdir := arg | opts }) "dir")
      "output compiled modules to `dir'"
  , mkOptErrOption ['X'] [] "ext" "language extension" extDescriptions
  , mkOptErrOption ['O'] [] "n"   "optimization level" optimDescriptions
  , mkOptErrOption ['d'] [] "opt" "debug option"       dumpDescriptions
  , Option [] ["parse-options"]
      (ReqArg (onOptsArg $ \arg opts -> { optParser := arg | opts }) "options")
      "additional options for the parser"
  , Option [] ["trace-failure"]
      (NoArg (onOpts $ \opts -> { optTraceFailure := True | opts }))
      "Trace failures in deterministic program"
  ]

--- Verbosity descriptions
verbDescriptions :: OptErrTable
verbDescriptions = map toDescr verbosities
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set f opts = { optVerbosity := f | opts }

--- Optimization descriptions
optimDescriptions :: OptErrTable
optimDescriptions = map toDescr optimizations
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set f opts = { optOptimization := f | opts }

--- Extension descriptions
extDescriptions :: OptErrTable
extDescriptions = map toDescr extensions
  where
  toDescr (flag, name, desc) = (name, desc, set flag)
  set f opts = { optExtensions := addFlag f (opts :> optExtensions) | opts }

--- Dump descriptions
dumpDescriptions :: OptErrTable
dumpDescriptions =
  [ ( "dump-all", "dump everything"
    , \ opts -> { optDump := map (\ (l, _, _) -> l) dumpLevel | opts })
  , ( "dump-none", "dump nothing"
    , \ opts -> { optDump := []                               | opts })
  ] ++ map toDescr dumpLevel
  where
  toDescr (flag, name, desc) = (name , "dump " ++ desc, set flag)
  set f opts = { optDump := addFlag f (opts :> optDump) | opts }

addFlag :: a -> [a] -> [a]
addFlag o opts = nub $ o : opts

removeFlag :: a -> [a] -> [a]
removeFlag o opts = filter (/= o) opts

-- -----------------------------------------------------------------------------
-- Parsing of the command line options
-- -----------------------------------------------------------------------------

--- Retrieve the options specified at the command line and the input file names.
--- This operation directly handles the following cases and exits the program
--- (exit code in parentheses) if one of them occurs
---  * The usage should be printed (0)
---  * The version should be printed (0)
---  * There were errors in the specified options (1)
---  * no files for compilation are given (1)
getCompilerOpts :: IO (Options, [String])
getCompilerOpts = do
  args <- getArgs
  prog <- getProgName
  processOpts prog $ parseOpts args

--- Parse the command line arguments.
parseOpts :: [String] -> (Options, [String], [String])
parseOpts args = (opts, files, errs ++ errs2)
  where
  (opts, errs2)          = foldl (flip ($)) (defaultOptions, []) optErrs
  (optErrs, files, errs) = getOpt Permute options args

--- Process the parsed command line arguments.
processOpts :: String -> (Options, [String], [String])
            -> IO (Options, [String])
processOpts prog (opts, files, errs)
  | opts :> optHelp    = printUsage prog
  | not (null errs')   = badUsage prog errs'
  | opts :> optVersion = printVersion
  | otherwise          = return (opts, files)
    where errs' = errs ++ checkOpts opts files

--- Check the parsed command line arguments for errors.
checkOpts :: Options -> [String] -> [String]
checkOpts _ []    = ["no files"]
checkOpts _ (_:_) = []

--- Print the usage information.
printUsage :: String -> IO a
printUsage prog = do
  putStrLn $ usageInfo header options
  exitWith 0
 where header = "usage: " ++ prog ++ " [OPTION] ... MODULE ..."

--- Complain about a bad usage.
badUsage :: String -> [String] -> IO a
badUsage prog errs = do
  mapIO_ (hPutStrLn stderr) errs
  hPutStrLn stderr $ "Try '" ++ prog ++ " --help' for more information"
  exitWith 1

--- Print the usage information.
printVersion :: IO a
printVersion = do
  putStrLn version
  exitWith 0
