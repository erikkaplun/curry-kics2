module Linker where

import AbstractCurry
import PropertyFile
import ReadShowTerm  (readQTermFile)
import IO
import Directory
import IOExts
import Time
import System
import FileGoodies


import qualified Installation as Inst
import Files
import Names         (funcInfoFile)
import RCFile
import Utils


type ReplState =
  { kics2Home    :: String     -- installation directory of the system
  , rcvars       :: [(String, String)] -- content of rc file
  , idSupply     :: String     -- IDSupply implementation (ioref, integer or ghc)
  , verbose      :: Int        -- verbosity level: 0 = quiet,
                               -- 1 = show frontend (module) compile/load
                               -- 2 = show backend (Haskell) compile/load
                               -- 3 = show intermediate messages, commands
                               -- 4 = show intermediate results
  , importPaths  :: [String]   -- additional directories to search for imports
  , libPaths     :: [String]   -- direcoties containg the standard libraries
  , outputSubdir :: String
  , mainMod      :: String     -- name of main module
  , addMods      :: [String]   -- names of additionally added modules
  , optim        :: Bool       -- compile with optimization
  , ndMode       :: NonDetMode -- mode for non-deterministic main goal
  , firstSol     :: Bool       -- print only first solution to nd main goal?
  , interactive  :: Bool       -- interactive execution of goal?
  , showBindings :: Bool       -- show free variables in main goal in output?
  , showTime     :: Bool       -- show execution of main goal?
  , useGhci      :: Bool       -- use ghci to evaluate main goal
  , cmpOpts      :: String     -- additional options for calling kics2 compiler
  , ghcOpts      :: String     -- additional options for ghc compilation
  , rtsOpts      :: String     -- run-time options for ghc
  , rtsArgs      :: String     -- run-time arguments passed to main application
  , quit         :: Bool       -- terminate the REPL?
  , sourceguis   :: [(String,(String,Handle))] -- handles to SourceProgGUIs
  , ghcicomm     :: Maybe GhciCommunication -- possible ghci comm. info
  }

--- Initial state of REPL
initReplState :: ReplState
initReplState =
  { kics2Home    = ""
  , rcvars       = []
  , idSupply     = "ioref"
  , verbose      = 1
  , importPaths  = []
  , libPaths     = map (Inst.installDir </>) ["lib", "lib" </> "meta"]
  , outputSubdir = ".curry" </> "kics2"
  , mainMod      = "Prelude"
  , addMods      = []
  , optim        = True
  , ndMode       = DFS
  , firstSol     = False
  , interactive  = False
  , showBindings = True
  , showTime     = False
  , useGhci      = False
  , cmpOpts      = ""
  , ghcOpts      = ""
  , rtsOpts      = ""
  , rtsArgs      = ""
  , quit         = False
  , sourceguis   = []
  , ghcicomm     = Nothing
  }

--- Mode of non-deterministic evaluation of main goal
data NonDetMode = DFS | BFS | IDS Int | Par Int | PrDFS | PrtChoices Int


--- --------------------------------------------------------------------------
--- Information for communication with ghci
--- (triple of command, handle, and KiCS2 verbosity of this command)
data GhciCommunication = GhciComm String Handle Int

--- start ghci communication (if necessary)
startGhciComm :: ReplState -> String -> IO ReplState
startGhciComm rst cmd = let v = rst->verbose in
  maybe (do hdl <- connectToCommand cmd
            let rst' = { ghcicomm := Just (GhciComm cmd hdl v) | rst }
            showGhciOutput rst' "putStrLn \"\""
            return rst'
        )
        (\ (GhciComm cmd0 hdl0 v0) ->
          if cmd==cmd0
          then hPutStrLnGhci rst hdl0 ":reload" >> return rst
          else do hPutStrLnGhci rst hdl0 ":quit"
                  unless (v0 < 2) (hGetLine hdl0 >>= putStrLn)
                  hClose hdl0
                  hdl <- connectToCommand cmd
                  return { ghcicomm := Just (GhciComm cmd hdl v) | rst }
        )
        (rst->ghcicomm)

--- terminate ghci communication
stopGhciComm :: ReplState -> IO ReplState
stopGhciComm rst =
  maybe (return rst)
        (\ (GhciComm _ hdl _) -> hPutStrLnGhci rst hdl ":quit" >>
                                 hClose hdl >>
                                 return { ghcicomm := Nothing | rst })
        (rst->ghcicomm)

--- send "main" to ghci and print results
mainGhciComm :: ReplState -> IO ()
mainGhciComm rst = do
  let (Just (GhciComm _ hdl _)) = rst->ghcicomm
  hPutStrLnGhci rst hdl $ ':':(if rst->showTime then "" else "un")++"set +s"
  showGhciOutput rst "safeExec main"
  unless (not (rst->showTime)) (hGetLine hdl >>= putStrLn)

--- send an IO expression to ghci and print the stdout data from ghci
showGhciOutput :: ReplState -> String -> IO ()
showGhciOutput rst cmd = do
  ctime <- getLocalTime
  let (Just (GhciComm _ hdl _)) = rst->ghcicomm
      stopstring = "???" ++ reverse (calendarTimeToString ctime) ++ "==="
  hPutStrLnGhci rst hdl (cmd++" >> putStrLn \""++stopstring++"\"")
  hPrintLinesBefore hdl stopstring
 where
   hPrintLinesBefore h stop = do
     line <- hGetLine h
     --putStrLn $ "FROM GHCI> "++line
     if line == stop
       then done
       else putStrLn line >> hPrintLinesBefore h stop

--- Send and show (if demanded) a string to ghci handle:
hPutStrLnGhci :: ReplState -> Handle -> String -> IO ()
hPutStrLnGhci rst h s = do
  writeVerboseInfo rst 2 $ "SEND TO GHCI> "++s
  hPutStrLn h s
  hFlush h

-- ---------------------------------------------------------------------------


--- Result of compiling main goal
data MainGoalCompile
  = GoalError                             -- error occurred
  | GoalWithoutBindings CurryProg         -- goal does not contain free vars
  | GoalWithBindings CurryProg Int String -- number of vars / new goal

--- Result of compiling main program
data MainCompile = MainError | MainDet | MainNonDet

-- ---------------------------------------------------------------------------


--- Location of the system configuration file.
scFileName :: String
scFileName = Inst.installDir </> ".kics2sc"


--- File name of created Main file
mainGoalFile :: String
mainGoalFile = "Curry_Main_Goal.curry"

--- Show an info message for a given verbosity level
writeVerboseInfo :: ReplState -> Int -> String -> IO ()
writeVerboseInfo rst lvl msg = unless (rst -> verbose < lvl) (putStrLn msg)



-- Reads the determinism infomation for the main goal file
readInfoFile :: ReplState -> IO [((String,String),Bool)]
readInfoFile rst = do
  readQTermFile (funcInfoFile (rst -> outputSubdir) mainGoalFile)

-- Create and compile the main module containing the main goal
createAndCompileMain :: ReplState -> Bool -> String -> MainGoalCompile
                     -> IO (ReplState,MainCompile)
createAndCompileMain rst createexecutable mainexp goalstate = do
  infos <- readInfoFile rst
  --print infos
  let isdet = notNull (filter (\i -> (snd (fst i)) == "d_C_kics2MainGoal")
                                infos)
      isio  = snd
               (head
                (filter (\i -> snd (fst i) ==
                           (if isdet then "d" else "nd") ++ "_C_kics2MainGoal")
                        infos))
  writeVerboseInfo rst 3 $ "Initial goal is " ++
                  (if isdet then "" else "non-") ++ "deterministic and " ++
                  (if isio  then "" else "not ") ++ "of IO type..."
  createHaskellMain rst goalstate isdet isio
  oldGhcOptions <- readPropertyFile scFileName >>=
                   return . flip rcValue "GHC_OPTIONS"
  if oldGhcOptions == rst -> ghcOpts then done
   else updatePropertyFile scFileName "GHC_OPTIONS" (rst -> ghcOpts)
  let useghci    = rst->useGhci && not createexecutable && not (rst->interactive)
      parSearch  = case rst -> ndMode of
                     Par _ -> True
                     _     -> False
      ghcImports = (if Inst.installGlobal
                    then [] 
                    else [ rst -> kics2Home ++ "/runtime"
                         , rst -> kics2Home ++ "/runtime/idsupply" ++ rst -> idSupply])
                   ++ ["." </> rst -> outputSubdir]
                   ++ map (</> rst -> outputSubdir) 
                          ((if Inst.installGlobal
                            then []
                            else (rst -> libPaths))
                           ++ (rst -> importPaths))
      ghcCompile = unwords . filter notNull $
        [ Inst.ghcExec
        , if rst -> optim && not useghci then "-O2" else ""
        , if useghci then "--interactive" else "--make"
        , if rst -> verbose < 2 then "-v0" else "-v1"
        , "-XMultiParamTypeClasses"
        , "-XFlexibleInstances"
        , "-XRelaxedPolyRec" --due to problem in FlatCurryShow
        , if (rst -> idSupply) `elem` ["ghc","ioref"] then "-package ghc" else ""
        , if parSearch then "-threaded" else ""
        , "-cpp" -- use the C pre processor -- TODO WHY?
        , rst -> ghcOpts
        , if oldGhcOptions == rst -> ghcOpts then "" else "-fforce-recomp"
        , if notNull (rst -> rtsOpts) || parSearch then "-rtsopts" else ""
        , "-i" ++ (intercalate ":" ghcImports)
        , "." </> rst -> outputSubdir </> "Main.hs"
        ]
                     -- also: -fforce-recomp -funbox-strict-fields ?
  writeVerboseInfo rst 2 $ "Compiling Main.hs with: "++ghcCompile
  rst' <- if useghci then startGhciComm rst ghcCompile
                          else return rst
  status <- if useghci
            then do writeVerboseInfo rst' 1
                                  ("Evaluating expression: " ++ strip mainexp)
                    mainGhciComm rst'
                    return 0
            else system ghcCompile
  return (rst',if status>0 then MainError else
               if isdet || isio then MainDet else MainNonDet)

-- Create the Main.hs program containing the call to the initial expression:
createHaskellMain :: ReplState -> MainGoalCompile -> Bool -> Bool -> IO ()
createHaskellMain rst goalstate isdet isio
  = writeFile ("." </> rst -> outputSubdir </> "Main.hs") $ unlines
      [ "module Main where"
      , "import MonadList"
      , "import Basics"
      , "import SafeExec"
      , if printOperation == "print" then "" else "import Curry_Prelude"
      , "import Curry_" ++ stripSuffix mainGoalFile
      , ""
      , "main = " ++ mainOperation ++ ' ' : mainPrefix ++ "kics2MainGoal"
      ]
  where
  printOperation = case goalstate of
    GoalWithBindings _ n _ -> printWithBindings n
    _                      -> "print"
  mainPrefix     = if isdet then "d_C_" else "nd_C_"
  mainOperation
    | isio && isdet                  = "evalDIO"
    | isio                           = "evalIO"
    | isdet                          = "evalD"
    | otherwise                      = case rst -> ndMode of
      PrDFS        -> searchExpr $ "prdfs"
      DFS          -> searchExpr $ "printDFS" ++ searchSuffix
      BFS          -> searchExpr $ "printBFS" ++ searchSuffix
      IDS        d -> searchExpr $ "printIDS" ++ searchSuffix ++ ' ' : show d
      Par        _ -> searchExpr $ "printPar" ++ searchSuffix
      PrtChoices d -> "prtChoiceTree" ++ ' ' : show d
  searchExpr search = search ++ ' ' : printOperation
  searchSuffix
    | rst -> interactive = "i " ++ moreDefault
    | rst -> firstSol    = "1"
    | otherwise          = ""
  moreDefault = case rcValue (rst -> rcvars) "moresolutions" of
    "yes" -> "MoreYes"
    "no"  -> "MoreNo"
    "all" -> "MoreAll"
    _     -> "MoreYes"
  -- Create the following Haskell expression for printing goals with bindings:
  -- (\ (OP_Tuple<n+2> result names v1 ... v<n>) ->
  --  printWithBindings (zip (fromCurry names) [show v1,..., show v<n>]) result)
  printWithBindings n =
    "(\\ (OP_Tuple"++show (n+2)++" result names"++
    concatMap (\i->' ':'v':show i) [1..n]++
    " ) ->"++
    " printWithBindings (zip (fromCurry names) ["++
    (intercalate "," (map (\i->"show v"++show i) [1..n])) ++
    "]) result)"