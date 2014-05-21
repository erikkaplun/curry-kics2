--- --------------------------------------------------------------------------
--- This module implements the "linker" of KiCS2.
--- It provides operations to write the main goal as a Haskell file
--- and compiling this main file together with all compiled Curry modules.
---
--- @author Michael Hanus, Bjoern Peemoeller
--- @version January 2014
--- --------------------------------------------------------------------------
{-# LANGUAGE Records #-}
module Linker
  ( ReplState (..), NonDetMode (..), MainCompile (..), loadPaths
  , setExitStatus
  , writeVerboseInfo, mainGoalFile, initReplState, createAndCompileMain
  ) where

import AbstractCurry
import Directory
import FilePath      ((</>), dropExtension)
import IO            (Handle, hFlush, stdout)
import List          (intercalate)
import PropertyFile
import ReadShowTerm  (readQTermFile)
import System

import qualified Installation as Inst
import GhciComm
import Names         (funcInfoFile)
import RCFile
import Utils         (notNull, strip)

type ReplState =
  { kics2Home    :: String     -- installation directory of the system
  , rcvars       :: [(String, String)] -- content of rc file
  , idSupply     :: String     -- IDSupply implementation (ioref, integer or ghc)
  , verbose      :: Int        -- verbosity level:
                               -- 0 = errors and warnings
                               -- 1 = show frontend compilation status
                               -- 2 = show also kics2c compilation status
                               -- 3 = show also ghc compilation status
                               -- 4 = show analysis information
  , importPaths  :: [String]   -- additional directories to search for imports
  , libPaths     :: [String]   -- directories containg the standard libraries
  , preludeName  :: String     -- the name of the standard prelude
  , outputSubdir :: String
  , mainMod      :: String     -- name of main module
  , addMods      :: [String]   -- names of additionally added modules
  , prompt       :: String     -- repl prompt shown in front of user input
  , optim        :: Bool       -- compile with optimization
  , ndMode       :: NonDetMode -- mode for non-deterministic main goal
  , firstSol     :: Bool       -- print only first solution to nd main goal?
  , interactive  :: Bool       -- interactive execution of goal?
  , showBindings :: Bool       -- show free variables in main goal in output?
  , showTime     :: Bool       -- show execution of main goal?
  , traceFailure :: Bool       -- trace failure in deterministic expression
  , useGhci      :: Bool       -- use ghci to evaluate main goal
  , safeExec     :: Bool       -- safe execution mode without I/O actions
  , parseOpts    :: String     -- additional options for the front end
  , cmpOpts      :: String     -- additional options for calling kics2 compiler
  , ghcOpts      :: String     -- additional options for ghc compilation
  , rtsOpts      :: String     -- run-time options for ghc
  , rtsArgs      :: String     -- run-time arguments passed to main application
  , quit         :: Bool       -- terminate the REPL?
  , exitStatus   :: Int        -- exit status (set in case of REPL errors)
  , sourceguis   :: [(String,(String,Handle))] -- handles to SourceProgGUIs
  , ghcicomm     :: Maybe GhciComm -- possible ghci comm. info
  }

--- Initial state of REPL
initReplState :: ReplState
initReplState =
  { kics2Home    := ""
  , rcvars       := []
  , idSupply     := "ioref"
  , verbose      := 1
  , importPaths  := []
  , libPaths     := map (Inst.installDir </>) ["lib", "lib" </> "meta"]
  , preludeName  := "Prelude"
  , outputSubdir := ".curry" </> "kics2"
  , mainMod      := "Prelude"
  , addMods      := []
  , prompt       := "%s> "
  , optim        := True
  , ndMode       := BFS
  , firstSol     := False
  , interactive  := False
  , showBindings := True
  , showTime     := False
  , traceFailure := False
  , useGhci      := False
  , safeExec     := False
  , parseOpts    := ""
  , cmpOpts      := ""
  , ghcOpts      := ""
  , rtsOpts      := ""
  , rtsArgs      := ""
  , quit         := False
  , exitStatus   := 0
  , sourceguis   := []
  , ghcicomm     := Nothing
  }

loadPaths :: ReplState -> [String]
loadPaths rst = "." : rst :> importPaths ++ rst :> libPaths

--- Sets the exit status in the REPL state.
setExitStatus :: Int -> ReplState -> ReplState
setExitStatus s rst = { exitStatus := s | rst }

-- ---------------------------------------------------------------------------

--- File name of created Main file
mainGoalFile :: String
mainGoalFile = "Curry_Main_Goal.curry"

--- Show an info message for a given verbosity level
writeVerboseInfo :: ReplState -> Int -> String -> IO ()
writeVerboseInfo rst lvl msg =
  unless (rst :> verbose < lvl) (putStrLn msg >> hFlush stdout)

-- Reads the determinism infomation for the main goal file
readInfoFile :: ReplState -> IO [((String,String),Bool)]
readInfoFile rst = do
  readQTermFile (funcInfoFile (rst :> outputSubdir) mainGoalFile)

getGoalInfo :: ReplState -> IO (Bool, Bool)
getGoalInfo rst = do
  infos <- readInfoFile rst
  --print infos
  let isdet = notNull (filter (\i -> (snd (fst i)) == "d_C_kics2MainGoal")
                                infos)
      isio  = snd (head (filter (\i -> snd (fst i) ==
                           (if isdet then "d" else "nd") ++ "_C_kics2MainGoal")
                        infos))
  writeVerboseInfo rst 2 $ "Initial goal is " ++
                (if isdet then "" else "non-") ++ "deterministic and " ++
                (if isio  then "" else "not ") ++ "of IO type..."
  return (isdet, isio)

-- Checks whether user-defined ghc options have been changed.
updateGhcOptions :: ReplState -> IO (ReplState,Bool)
updateGhcOptions rst =
  if oldOpts == newOpts
    then return (rst,False)
    else do
      setRCProperty key newOpts
      rcDefs <- readRC
      return ({ rcvars := rcDefs | rst }, True)
 where
   key = "ghc_options"
   oldOpts = rcValue (rst :> rcvars) key
   newOpts = rst :> ghcOpts

--- Result of compiling main program
data MainCompile = MainError | MainDet | MainNonDet

-- Create and compile the main module containing the main goal
createAndCompileMain :: ReplState -> Bool -> String -> Maybe Int
                     -> IO (ReplState, MainCompile)
createAndCompileMain rst createExecutable mainExp bindings = do
  (isdet, isio) <- getGoalInfo rst
  (rst',wasUpdated) <- updateGhcOptions rst
  writeFile mainFile $ mainModule rst' isdet isio (rst :> traceFailure) bindings

  let ghcCompile = ghcCall rst' useGhci wasUpdated mainFile
  writeVerboseInfo rst' 3 $ "Compiling " ++ mainFile ++ " with: " ++ ghcCompile
  (rst'', status) <- if useGhci
                      then compileWithGhci rst' ghcCompile mainExp
                      else system ghcCompile >>= \stat -> return (rst', stat)
  return (if status > 0
          then (setExitStatus 1 rst'', MainError)
          else (setExitStatus 0 rst'',
                if isdet || isio then MainDet else MainNonDet))
 where
  mainFile = "." </> rst :> outputSubdir </> "Main.hs"
  -- option parsing
  useGhci = rst :> useGhci && not createExecutable && not (rst :> interactive)

compileWithGhci :: ReplState -> String -> String -> IO (ReplState, Int)
compileWithGhci rst ghcCompile mainExp = do
  comm <- refresh ghcCompile (rst :> verbose > 2)
  writeVerboseInfo rst 1 $ "Evaluating expression: " ++ strip mainExp
  evalMainCmd comm (rst :> showTime)
  return ({ ghcicomm := Just comm | rst }, 0)
 where refresh = case rst :> ghcicomm of
                    Nothing  -> initGhciComm
                    Just old -> restartGhciComm old

ghcCall :: ReplState -> Bool -> Bool -> String -> String
ghcCall rst useGhci recompile mainFile = unwords . filter notNull $
  [ Inst.ghcExec
  , Inst.ghcOptions
  , if rst :> optim && not useGhci then "-O2"            else ""
  , if useGhci                     then "--interactive"  else "--make"
  , if rst :> verbose < 2          then "-v0"            else "-v1"
  , if withGhcSupply               then "-package ghc"   else ""
  , if isParSearch                 then "-threaded"      else ""
  , if withRtsOpts                 then "-rtsopts"       else ""
  , if recompile                   then "-fforce-recomp" else ""
      -- XRelaxedPolyRec due to problem in FlatCurryShow
  , "-XMultiParamTypeClasses", "-XFlexibleInstances", "-XRelaxedPolyRec"
--   , "-cpp" -- use the C pre processor -- TODO WHY?
  , rst :> ghcOpts
  , "-i" ++ (intercalate ":" ghcImports)
  , mainFile
  ]
 where
  withGhcSupply = (rst :> idSupply) `elem` ["ghc", "ioref"]
  withRtsOpts   = notNull (rst :> rtsOpts) || isParSearch
  isParSearch   = case rst :> ndMode of
    Par _ -> True
    _     -> False
  ghcImports
    | Inst.installGlobal
    = map (</> rst :> outputSubdir) ("." : rst :> importPaths)
    | otherwise
    = [ rst :> kics2Home </> "runtime"
      , rst :> kics2Home </> "runtime" </> "idsupply" ++ rst :> idSupply
      ] ++ map (</> rst :> outputSubdir) (loadPaths rst)

--- Mode of non-deterministic evaluation of main goal
data NonDetMode  = DFS | BFS | IDS Int | Par Int | PrDFS | PrtChoices Int

data EvalMode    = All | One | Interactive MoreDefault -- | Count

data MoreDefault = MoreYes | MoreNo | MoreAll

-- Create the Main.hs program containing the call to the initial expression:
mainModule :: ReplState -> Bool -> Bool -> Bool -> Maybe Int -> String
mainModule rst isdet isio isTF mbBindings = unlines
  [ "module Main where"
  , if rst :> interactive then "import MonadList" else ""
  , "import Basics"
  , "import SafeExec"
  , if mbBindings==Nothing
    then ""
    else ("import Curry_"++ rst :> preludeName)
  , if (rst :> traceFailure)
      then "import Curry_Trace_" ++ dropExtension mainGoalFile
      else "import Curry_" ++ dropExtension mainGoalFile
  , ""
  , "main :: IO ()"
  , mainExpr "kics2MainGoal" isdet isio isTF (rst :> ndMode) evalMode mbBindings
  ]
 where
  evalMode
    | rst :> interactive = Interactive moreDefault
    | rst :> firstSol    = One
    | otherwise          = All
  moreDefault = case rcValue (rst :> rcvars) "moresolutions" of
    "yes" -> MoreYes
    "no"  -> MoreNo
    "all" -> MoreAll
    _     -> MoreYes

mainExpr :: String -> Bool -> Bool -> Bool -> NonDetMode -> EvalMode -> Maybe Int -> String
mainExpr goal isdet isio isTF ndMode evalMode mbBindings
  = "main = " ++ mainOperation ++ ' ' : detPrefix ++ goal
 where
  detPrefix = if isdet then "d_C_" else "nd_C_"
  mainOperation
    | isio && isdet && isTF = "failtraceDIO"
    | isio && isdet         = "evalDIO"
    | isio                  = "evalIO"
    | isdet && isTF         = "failtraceD"
    | isdet                 = "evalD"
    | otherwise             = case ndMode of
      PrDFS        -> searchExpr $ "prdfs"
      DFS          -> searchExpr $ "printDFS" ++ searchSuffix
      BFS          -> searchExpr $ "printBFS" ++ searchSuffix
      IDS        d -> searchExpr $ "printIDS" ++ searchSuffix ++ ' ' : show d
      Par        _ -> searchExpr $ "printPar" ++ searchSuffix
      PrtChoices d -> "prtChoiceTree" ++ ' ' : show d
  searchExpr strat = strat ++ ' ' : printOperation
  searchSuffix = case evalMode of
    All            -> ""
    One            -> "1"
    Interactive md -> "i " ++ show md
  printOperation = maybe "print" printWithBindings mbBindings

  -- Create the following Haskell expression for printing goals with bindings:
  -- (\ (OP_Tuple<n+2> result names v1 ... v<n>) ->
  --  printWithBindings (zip (fromCurry names) [show v1,..., show v<n>]) result)
  printWithBindings n =
    "(\\ (OP_Tuple" ++ show (n + 2) ++ " result names" ++
    concatMap ((" v" ++) . show) [1 .. n] ++
    " ) ->" ++
    " printWithBindings (zip (fromCurry names) [" ++
    intercalate "," (map (("show v" ++) . show) [1..n]) ++
    "]) result)"
