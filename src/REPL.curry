--- --------------------------------------------------------------------------
--- Read-Eval-Print loop for KiCS2
---
--- @author Michael Hanus, Björn Peemöller
--- @version June 2012
--- --------------------------------------------------------------------------
module REPL where

import AbstractCurry
import Char          (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Directory
import Distribution
import FileGoodies
import FlatCurry     (flatCurryFileName)
import IO
import IOExts
import List          (isPrefixOf, isInfixOf, intersperse, nub)
import PropertyFile
import ReadNumeric   (readNat)
import ReadShowTerm  (readQTermFile)
import Sort          (mergeSort)
import System        (system, getArgs, getEnviron, setEnviron, getPID)
import Time

import AbstractCurryGoodies
import Files
import qualified Installation as Inst
import Names         (funcInfoFile)
import RCFile
import Utils

--- Location of the system configuration file.
scFileName :: String
scFileName = installDir </> ".kics2sc"

--- File name of created Main file
mainGoalFile :: String
mainGoalFile = "Curry_Main_Goal.curry"

--- Internal state of the REPL
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

--- Mode of non-deterministic evaluation of main goal
data NonDetMode = DFS | BFS | IDS Int | Par Int | PrDFS | PrtChoices Int

--- Result of compiling main goal
data MainGoalCompile
  = GoalError                             -- error occurred
  | GoalWithoutBindings CurryProg         -- goal does not contain free vars
  | GoalWithBindings CurryProg Int String -- number of vars / new goal

--- Result of compiling main program
data MainCompile = MainError | MainDet | MainNonDet

--- Show an info message for a given verbosity level
writeVerboseInfo :: ReplState -> Int -> String -> IO ()
writeVerboseInfo rst lvl msg = unless (rst -> verbose < lvl) (putStrLn msg)

--- Show an error message
writeErrorMsg :: String -> IO ()
writeErrorMsg msg = putStrLn ("ERROR: " ++ msg)

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rcdefs <- readRC
  args   <- getArgs
  let rst = { kics2Home := Inst.installDir
            , rcvars    := rcdefs
            | initReplState
            }
  ipath  <- defaultImportPaths rst
  processArgsAndStart { importPaths := ipath | rst }
    (map strip (words (rcValue (rst->rcvars) "defaultparams")) ++ args)

--- The default import paths of KiCS2.
--- It consists of the path defined by the environment variable CURRYPATH,
--- and the "libraries" property defined in ~/.kics2rc
defaultImportPaths :: ReplState -> IO [String]
defaultImportPaths rst = do
  currypath <- getEnviron "CURRYPATH"
  let rclibs = rcValue (rst -> rcvars) "libraries"
  return $ (if null currypath then [] else splitPath currypath) ++
           (if null rclibs    then [] else splitPath rclibs)

defaultImportPathsWith :: ReplState -> String -> IO [String]
defaultImportPathsWith rst dirs =
  defaultImportPaths rst >>= return . (splitPath dirs ++)

loadPath :: ReplState -> [String]
loadPath rst = "." : rst -> importPaths ++ rst -> libPaths

processArgsAndStart :: ReplState -> [String] -> IO ()
processArgsAndStart rst []
  | rst -> quit = cleanUpRepl rst
  | otherwise   = do getBanner >>= writeVerboseInfo rst 1
                     writeVerboseInfo rst 1 "Type \":h\" for help"
                     repl rst
processArgsAndStart rst (arg:args) =
  if head arg /= ':'
  then writeErrorMsg ("unknown command: " ++ unwords (arg:args)) >> printHelp
  else do let (cmdargs,more) = break (\a -> head a == ':') args
          mbrst <- processCommand rst (tail (unwords (arg:cmdargs)))
          maybe printHelp (\rst' -> processArgsAndStart rst' more) mbrst
 where
  printHelp = putStrLn "Usage: kics2 <list of commands>\n" >> printHelpOnCommands

--- Retrieve the KiCS2 banner
getBanner :: IO String
getBanner = do
  logo <- readFile (Inst.installDir ++ "/data/logo.txt")
  return (logo ++ version)
 where version = "Version "
              ++ show Inst.majorVersion ++ "." ++ show Inst.minorVersion
              ++ " of " ++ Inst.compilerDate
              ++ " (installed at " ++ Inst.installDate ++ ")"

-- ---------------------------------------------------------------------------

-- The main read-eval-print loop:
repl :: ReplState -> IO ()
repl rst = do
  putStr prompt >> hFlush stdout
  eof <- isEOF
  if eof
    then cleanUpRepl rst
    else do getLine >>= processInput rst . strip
 where prompt = unwords (rst -> addMods ++ [rst-> mainMod]) ++ "> "

-- Clean resources of REPL before terminating it.
cleanUpRepl :: ReplState -> IO ()
cleanUpRepl rst = terminateSourceProgGUIs rst >> done

processInput :: ReplState -> String -> IO ()
processInput rst g
  | null g      = repl rst
  | isCommand g = do mbrst <- processCommand rst (strip (tail g))
                     maybe (repl rst)
                           (\rst' -> if (rst'->quit) then cleanUpRepl rst'
                                                     else repl rst')
                           mbrst
  | otherwise   = evalExpression rst g >>= repl
 where isCommand str = notNull str && head str == ':'

--- Evaluate an expression w.r.t. currently loaded modules
evalExpression :: ReplState -> String -> IO ReplState
evalExpression rst expr = do
  (rst', status) <- compileProgramWithGoal rst (not (rst -> useGhci)) expr
  unless (status == MainError || (rst' -> useGhci && not (rst' -> interactive)))
         (execMain rst' status expr >> done)
  cleanMainGoalFile rst'
  return rst'

-- Remove mainGoalFile and auxiliaries
cleanMainGoalFile :: ReplState -> IO ()
cleanMainGoalFile rst = unless keepfiles $ do
  system $ Inst.installDir ++ "/bin/cleancurry " ++ mainGoalFile
  removeFileIfExists mainGoalFile
 where keepfiles = rcValue (rst -> rcvars) "keepfiles" == "yes"

-- Generate, read, and delete .acy file of main goal file.
-- Return Nothing if some error occurred during parsing.
getAcyOfMainGoal :: ReplState -> IO (Maybe CurryProg)
getAcyOfMainGoal rst = do
  let mainGoalProg    = stripSuffix mainGoalFile
      acyMainGoalFile = inCurrySubdir $ mainGoalProg <.> "acy"
      frontendParams  = setQuiet    (rst -> verbose < 2)
                      $ setFullPath (loadPath rst)
                        defaultParams
  callFrontendWithParams ACY frontendParams mainGoalProg
  acyExists <- doesFileExist acyMainGoalFile
  if not acyExists
    then return Nothing
    else do
      acySize <- fileSize acyMainGoalFile
      if acySize == 0
        then return Nothing
        else do
          prog <- tryReadACYFile acyMainGoalFile
          removeFile acyMainGoalFile
          return prog

-- Show the type of goal w.r.t. main program:
showTypeOfGoal :: ReplState -> String -> IO Bool
showTypeOfGoal rst goal = do
  writeMainGoalFile rst [] Nothing goal
  mbprog <- getAcyOfMainGoal rst
  removeFile mainGoalFile
  maybe (return False)
        (\ (CurryProg _ _ _ [mfunc] _) -> do
           let (CFunc _ _ _ maintype _) = mfunc
           putStrLn $ goal ++ " :: " ++ showMonoTypeExpr False False maintype
           return True)
        mbprog

-- Get the module of a function visible in the main program:
getModuleOfFunction :: ReplState -> String -> IO String
getModuleOfFunction rst funname = do
  writeMainGoalFile rst [] Nothing
    (if isAlpha (head funname) then funname else '(':funname++")")
  mbprog <- getAcyOfMainGoal rst
  removeFile mainGoalFile
  maybe (return "")
        (\ (CurryProg _ _ _ [mfunc] _) -> do
           let (CFunc _ _ _ _ mainrules) = mfunc
           return (modOfMain mainrules))
        mbprog
 where
  modOfMain r = case r of
    CRules _ [CRule [] [(_,CSymbol (mod,_))] []] -> mod
    _ -> ""

-- Compile main program with goal:
compileProgramWithGoal :: ReplState -> Bool -> String
                       -> IO (ReplState, MainCompile)
compileProgramWithGoal rst createexecutable goal = do
  let infoFile = funcInfoFile (rst -> outputSubdir) mainGoalFile
  removeFileIfExists infoFile
  removeFileIfExists $ flatCurryFileName mainGoalFile
  writeMainGoalFile rst [] Nothing goal
  goalstate <- insertFreeVarsInMainGoal rst goal
  if goalstate == GoalError then return (rst, MainError) else do
    let (newprog,newgoal) = case goalstate of
                              GoalWithBindings p _ g -> (p,g)
                              GoalWithoutBindings p  -> (p,goal)
    typeok <- makeMainGoalMonomorphic rst newprog newgoal
    if typeok
     then do
      status <- compileCurryProgram rst mainGoalFile True
      exinfo <- doesFileExist infoFile
      if status == 0 && exinfo
       then createAndCompileMain rst createexecutable goal goalstate
       else return (rst, MainError)
     else return (rst, MainError)

-- write the file with the main goal where necessary imports
-- and possibly a type string is provided:
writeMainGoalFile :: ReplState -> [String] -> Maybe String -> String -> IO ()
writeMainGoalFile rst imports mtype goal = writeFile mainGoalFile $
  unlines $ map ("import " ++) allImports
         ++ maybe [] (\ts -> ["kics2MainGoal :: " ++ ts]) mtype
         ++ ["kics2MainGoal = " ++ goal]
  where allImports = nub $ rst -> mainMod : rst -> addMods ++ imports

--- If the main goal is polymorphic, make it monomorphic by adding a type
--- declaration where type variables are replaced by type "()".
--- If the main goal has type "IO t" where t is monomorphic, t/=(),
--- and t is not a function, then ">>= print" is added to the goal.
--- The result is False if the main goal contains some error.
makeMainGoalMonomorphic :: ReplState -> CurryProg -> String -> IO Bool
makeMainGoalMonomorphic rst (CurryProg _ _ _ [mfunc] _) goal
  | isFunctionalType maintype
  = writeErrorMsg "expression is of functional type" >> return False
  | isPolyType maintype = do
    writeMainGoalFile rst (modsOfType maintype)
                          (Just (showMonoTypeExpr True False maintype))
                          goal
    writeVerboseInfo rst 2 $
      "Type of main expression \"" ++ showMonoTypeExpr False False maintype
      ++ "\" made monomorphic"
    writeVerboseInfo rst 1
      "Type variables of main expression replaced by \"()\""
    return True
  | otherwise = do
    unless (newgoal == goal) $ writeMainGoalFile rst [] Nothing newgoal
    return True
 where
  (CFunc _ _ _ maintype _) = mfunc
  newgoal = if isIOReturnType maintype
              then '(' : goal++") >>= print"
              else goal

-- Insert free variables occurring in the main goal as components
-- of the main goal so that their bindings are shown
-- The status of the main goal is returned.
insertFreeVarsInMainGoal :: ReplState -> String -> IO MainGoalCompile
insertFreeVarsInMainGoal rst goal = getAcyOfMainGoal rst >>=
  maybe (return GoalError)
   (\ prog@(CurryProg _ _ _ [mfunc] _) -> do
    let (CFunc _ _ _ maintype _) = mfunc
        freevars = freeVarsInFuncRule mfunc
    if null freevars || not (rst -> showBindings)
       || isPrtChoices (rst->ndMode)
       || isIOType maintype
       || length freevars > 10 -- due to limited size of tuples used
                               -- in PrintBindings
     then return (GoalWithoutBindings prog)
     else let (exp,whereclause) = break (=="where") (words goal)
           in if null whereclause then return (GoalWithoutBindings prog) else do
              let newgoal = unwords $
                        ["("] ++
                        exp ++ [",["] ++
                        intersperse "," (map (\v->"\""++v++"\"") freevars) ++
                        ["]"] ++
                        map (\v->',':v) freevars ++
                        ")":whereclause
              writeMainGoalFile rst [] Nothing newgoal
              writeVerboseInfo rst 2
                ("Adding printing of bindings for free variables: "++
                 (intercalate "," freevars))
              mbprog <- getAcyOfMainGoal rst
              return (maybe GoalError
                            (\p -> GoalWithBindings p (length freevars) newgoal)
                            mbprog)
   )
 where
  isPrtChoices c = case c of
    PrtChoices _ -> True
    _            -> False
  freeVarsInFuncRule (CFunc _ _ _ _ (CRules _ [CRule _ _ ldecls])) =
    concatMap lvarName ldecls

  lvarName ldecl = case ldecl of CLocalVar (_,v) -> [v]
                                 _               -> []

-- Compile a Curry program with IDC compiler:
compileCurryProgram :: ReplState -> String -> Bool -> IO Int
compileCurryProgram rst curryprog ismain = do
  let compileProg = rst -> kics2Home </> "bin" </> ".local" </> "kics2c"
      kics2options  = --(if rst->verbose < 2 then "-q " else "") ++
                    "-v " ++ show (verbREPL2IDC (rst->verbose)) ++ " " ++
                    (concatMap (\i -> " -i "++i) (rst->importPaths ++ rst->libPaths))
      compileCmd  = unwords [compileProg,kics2options,rst->cmpOpts,curryprog]
  writeVerboseInfo rst 3 $ "Executing: "++compileCmd
  system compileCmd
 where
  verbREPL2IDC v | v==1 && not ismain = 2 -- to get frontend messages
                 | otherwise          = v

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

-- Execute main program and show run time:
execMain :: ReplState -> MainCompile -> String -> IO Int
execMain rst cmpstatus mainexp = do
  timecmd <- getTimeCmd
  let paropts = case rst -> ndMode of
                  Par n -> "-N" ++ (if n == 0 then "" else show n)
                  _     -> ""
      maincmd = ("." </> rst -> outputSubdir </> "Main") ++
                (if null (rst->rtsOpts) && null paropts
                 then " "
                 else " +RTS "++rst->rtsOpts++" "++paropts++" -RTS ") ++
                rst->rtsArgs
      tcmd    = timecmd ++ maincmd
      icmd    = if rst->interactive && cmpstatus==MainNonDet
                then execInteractive rst tcmd
                else tcmd
  writeVerboseInfo rst 1 $ "Evaluating expression: " ++ strip mainexp
  writeVerboseInfo rst 3 $ "Executing: " ++ icmd
  system icmd
 where
  getTimeCmd = if rst->showTime
               then getDistribution >>= return . getTimeCmdForDist
               else return ""

  -- Interactive execution of a main search command: current, a new
  -- terminal is opened due to problematic interaction with readline
  execInteractive rst' cmd =
    rcValue (rst'->rcvars) "interactivecommand" ++ ' ':cmd

  -- Time command for specific distributions. It might be necessary
  -- to adapt this command.
  getTimeCmdForDist dist
    | "Ubuntu" `isInfixOf` dist
     = "time --format=\"Execution time: %Us / elapsed: %E\" "
    | "Debian" `isInfixOf` dist
      = "export TIMEFORMAT=\"Execution time: %2Us / elapsed: %2Es\" && time "
    | otherwise = "time "

  getDistribution = do
    (hin,hout,herr) <- execCmd "lsb_release -i"
    dist <- hGetContents hout
    hClose hin
    hClose hout
    hClose herr
    return dist

-- ---------------------------------------------------------------------------
-- Processing of REPL commands
-- ---------------------------------------------------------------------------

-- Process a command of the REPL.
-- The result is either just a new ReplState or Nothing if an error occurred.
processCommand :: ReplState -> String -> IO (Maybe ReplState)
processCommand rst cmds
  | null cmds        = skipCommand "unknown command"
  | head cmds == '!' = processSysCall rst (strip $ tail cmds)
  | otherwise        = case matchedCmds of
      []            -> skipCommand $ "unknown command: ':" ++ cmds ++ "'"
      [(_, act)]    -> act rst (strip args)
      (_:_:_)       -> skipCommand $ "ambiguous command: ':" ++ cmds ++ "'"
 where (cmd, args) = break (==' ') cmds
       matchedCmds = filter (isPrefixOf (map toLower cmd) . fst) replCommands

-- all available REPL commands
replCommands :: [(String, ReplState -> String -> IO (Maybe ReplState))]
replCommands =
  [ ("quit"       , processQuit     )
  , ("help"       , processHelp     )
  , ("?"          , processHelp     )
  , ("load"       , processLoad     )
  , ("reload"     , processReload   )
  , ("add"        , processAdd      )
  , ("eval"       , processEval     )
  , ("browse"     , processBrowse   )
  , ("cd"         , processCd       )
  , ("fork"       , processFork     )
  , ("programs"   , processPrograms )
  , ("edit"       , processEdit     )
  , ("interface"  , processInterface)
  , ("source"     , processSource   )
  , ("show"       , processShow     )
  , ("set"        , processSetOption)
  , ("save"       , processSave     )
  , ("type"       , processType     )
  , ("usedimports", processUsedImps )
  ]

skipCommand :: String -> IO (Maybe ReplState)
skipCommand msg = writeErrorMsg msg >> return Nothing

processSysCall :: ReplState -> String -> IO (Maybe ReplState)
processSysCall rst cmd
  | null cmd  = skipCommand "missing system command"
  | otherwise = system cmd >> return (Just rst)

processQuit :: ReplState -> String -> IO (Maybe ReplState)
processQuit rst _ = return (Just { quit := True | rst })

processHelp :: ReplState -> String -> IO (Maybe ReplState)
processHelp rst _ = do
  printHelpOnCommands
  putStrLn "... or type any <expression> to evaluate\n"
  return (Just rst)

processLoad :: ReplState -> String -> IO (Maybe ReplState)
processLoad rst args = do
  rst' <- terminateSourceProgGUIs rst
  let dirmodname = stripSuffix args
  if null dirmodname
    then skipCommand "missing module name"
    else do
    let (dirname,modname) = splitDirectoryBaseName dirmodname
    if dirname=="." then done else setCurrentDirectory dirname
    mbf <- lookupFileInPath modname [".curry", ".lcurry"] ["."]
    maybe (skipCommand ("source file of module " ++ dirmodname ++ " not found"))
          (\fn ->
              readAndProcessSourceFileOptions rst' fn >>=
              maybe (return Nothing)
                (\rst'' -> compileCurryProgram rst'' modname False >>
                return (Just{mainMod := modname, addMods := [] | rst''}))
          )
          mbf

-- Terminate all open SourceProgGUIs
terminateSourceProgGUIs :: ReplState -> IO ReplState
terminateSourceProgGUIs rst
  | null sguis = return rst
  | otherwise  = do
      writeVerboseInfo rst 1 "Terminating source program GUIs..."
      catch (mapIO_ (\ (_,(_,h)) -> hPutStrLn h "q" >> hFlush h >> hClose h) sguis)
            (\_ -> done)
      return { sourceguis := [] | rst }
 where sguis = rst -> sourceguis

processReload :: ReplState -> String -> IO (Maybe ReplState)
processReload rst args
  | rst -> mainMod == "Prelude"
  = skipCommand "no program loaded!"
  | null (stripSuffix args)
  = compileCurryProgram rst (rst -> mainMod) False >> return (Just rst)
  | otherwise
  = skipCommand "superfluous argument"

processAdd :: ReplState -> String -> IO (Maybe ReplState)
processAdd rst args = do
  let modname = stripSuffix args
  if null modname
    then skipCommand "missing module name"
    else do
    mbf <- lookupFileInPath modname [".curry", ".lcurry"] (loadPath rst)
    maybe (skipCommand "source file of module not found")
          (\ _ -> return (Just { addMods := modname : rst -> addMods | rst}))
          mbf

processEval :: ReplState -> String -> IO (Maybe ReplState)
processEval rst args = evalExpression rst args >> return (Just rst)

processType :: ReplState -> String -> IO (Maybe ReplState)
processType rst args = do
  typeok <- showTypeOfGoal rst args
  return (if typeok then Just rst else Nothing)

processCd :: ReplState -> String -> IO (Maybe ReplState)
processCd rst args = do
  exists <- doesDirectoryExist args
  if exists then setCurrentDirectory args >> return (Just rst)
            else skipCommand "directory does not exist"

processPrograms :: ReplState -> String -> IO (Maybe ReplState)
processPrograms rst _ = printAllLoadPathPrograms rst >> return (Just rst)

processEdit :: ReplState -> String -> IO (Maybe ReplState)
processEdit rst args = do
  let modname = if null args then rst -> mainMod else stripSuffix args
  mbf <- lookupFileInPath modname [".curry", ".lcurry"]
                          ("." : rst -> importPaths)
  editenv <- getEnviron "EDITOR"
  let editcmd  = rcValue (rst -> rcvars) "editcommand"
      editprog = if null editcmd then editenv else editcmd
  if null editenv && null editcmd
    then skipCommand "no editor defined"
    else maybe (skipCommand "source file not found")
          (\fn -> system (editprog++" "++fn++"& ") >> return (Just rst))
          mbf

processSource :: ReplState -> String -> IO (Maybe ReplState)
processSource rst args
  | null args   = skipCommand "missing function name"
  | null dotfun = do m <- getModuleOfFunction rst args
                     if null m
                       then skipCommand "function not found"
                       else showFunctionInModule rst m args
  | otherwise   = showFunctionInModule rst mod (tail dotfun)
  where (mod, dotfun) = break (== '.') args

-- Showing source code of functions via SourcProgGUI tool.
-- If necessary, open a new connection and remember it in the repl state.
showFunctionInModule :: ReplState -> String -> String -> IO (Maybe ReplState)
showFunctionInModule rst mod fun = do
  let mbh      = lookup mod (rst->sourceguis)
      toolexec = "tools/SourceProgGUI"
      spgui    = rst->kics2Home </> toolexec
  spgexists <- doesFileExist spgui
  if not spgexists
   then errorMissingTool toolexec >> return Nothing
   else do
    (rst',h') <- maybe (do h <- connectToCommand (spgui++" "++mod)
                           return ({sourceguis := (mod,(fun,h))
                                              : rst->sourceguis | rst}, h))
                       (\ (f,h) -> do
                           hPutStrLn h ('-':f)
                           hFlush h
                           return ({sourceguis := updateFun (rst->sourceguis)
                                                          mod fun | rst }, h))
                       mbh
    hPutStrLn h' ('+':fun)
    hFlush h'
    return (Just rst')
 where
  updateFun []                _  _  = []
  updateFun ((m,(f,h)):sguis) md fn =
    if m==md then (m,(fn,h)):sguis
            else (m,(f,h)) : updateFun sguis md fn

processShow :: ReplState -> String -> IO (Maybe ReplState)
processShow rst args = do
  let modname = if null args then rst -> mainMod else stripSuffix args
  mbf <- lookupFileInPath modname [".curry", ".lcurry"]
                          (loadPath rst)
  maybe (skipCommand "source file not found")
        (\fn -> do  let showcmd = rcValue (rst->rcvars) "showcommand"
                    system $ (if null showcmd then "cat" else showcmd)
                            ++' ':fn
                    putStrLn ""
                    return (Just rst)
        )
        mbf

processInterface :: ReplState -> String -> IO (Maybe ReplState)
processInterface rst args = do
  let modname  = if null args then rst -> mainMod else stripSuffix args
      toolexec = "tools" </> "GenInt"
  callTool rst toolexec ("-int " ++ modname)

processBrowse :: ReplState -> String -> IO (Maybe ReplState)
processBrowse rst args
  | notNull $ stripSuffix args = skipCommand "superfluous argument"
  | otherwise                  = do
      let toolexec = "tools" </> "browser" </> "BrowserGUI"
      callTool rst toolexec (rst -> mainMod)

processUsedImps :: ReplState -> String -> IO (Maybe ReplState)
processUsedImps rst args = do
  let modname  = if null args then rst -> mainMod else stripSuffix args
      toolexec = "tools" </> "ImportCalls"
  callTool rst toolexec modname

callTool :: ReplState -> String -> String -> IO (Maybe ReplState)
callTool rst cmd args = do
  let path = rst -> kics2Home </> cmd
  exists <- doesFileExist path
  if exists
    then system (path ++ ' ' : args) >> return (Just rst)
    else errorMissingTool cmd

errorMissingTool :: String -> IO (Maybe ReplState)
errorMissingTool cmd = skipCommand $
     Inst.installDir ++ '/' : cmd ++ " not found\n"
  ++ "Possible solution: run \"cd " ++ Inst.installDir ++" && make install\""

processSave :: ReplState -> String -> IO (Maybe ReplState)
processSave rst args
  | rst -> mainMod == "Prelude" = skipCommand "no program loaded"
  | otherwise                   = do
    (rst', status) <- compileProgramWithGoal rst True
                                    (if null args then "main" else args)
    unless (status == MainError) $ do
      renameFile ("." </> rst' -> outputSubdir </> "Main") (rst' -> mainMod)
      writeVerboseInfo rst' 1 ("Executable saved in '" ++ rst' -> mainMod ++ "'")
    cleanMainGoalFile rst'
    return (Just rst')

processFork :: ReplState -> String -> IO (Maybe ReplState)
processFork rst args
  | rst -> mainMod == "Prelude" = skipCommand "no program loaded"
  | otherwise                   = do
    (rst', status) <- compileProgramWithGoal rst True
                      (if null args then "main" else args)
    unless (status == MainError) $ do
      pid <- getPID
      let execname = "/tmp/kics2fork" ++ show pid
      system ("mv ." </> rst' -> outputSubdir </> "Main " ++ execname)
      writeVerboseInfo rst' 3 ("Starting executable '" ++ execname ++ "'...")
      system ("( "++execname++" && rm -f "++execname++ ") "++
              "> /dev/null 2> /dev/null &") >> done
    cleanMainGoalFile rst'
    return (Just rst')

-- Process setting of an option
processSetOption :: ReplState -> String -> IO (Maybe ReplState)
processSetOption rst option
  | null (strip option) = printOptions rst >> return (Just rst)
  | otherwise           = case matched of
      []        -> skipCommand $ "unknown option: '" ++ option ++ "'"
      [(_,act)] -> act rst (strip args)
      _         -> skipCommand $ "ambiguous option: ':" ++ option ++ "'"
 where (opt,args)   = break (==' ') option
       matched      = filter (isPrefixOf (map toLower opt) . fst) availOptions

availOptions :: [(String, ReplState -> String -> IO (Maybe ReplState))]
availOptions = filter installOpts replOptions
  where installOpts (opt, _) = not Inst.installGlobal
                               || opt `notElem` ["supply","ghc"]

replOptions :: [(String, ReplState -> String -> IO (Maybe ReplState))]
replOptions =
  [ ("paths"        , setOptionPath                                      )
  , ("bfs"          , \r _ -> return (Just { ndMode       := BFS   | r }))
  , ("dfs"          , \r _ -> return (Just { ndMode       := DFS   | r }))
  , ("prdfs"        , \r _ -> return (Just { ndMode       := PrDFS | r }))
  , ("choices"      , setOptionNDMode PrtChoices 10                      )
  , ("ids"          , setOptionNDMode IDS        100                     )
  , ("par"          , setOptionNDMode Par        0                       )
  , ("supply"       , setOptionSupply                                    )
  , ("v0"           , \r _ -> return (Just { verbose      := 0     | r }))
  , ("v1"           , \r _ -> return (Just { verbose      := 1     | r }))
  , ("v2"           , \r _ -> return (Just { verbose      := 2     | r }))
  , ("v3"           , \r _ -> return (Just { verbose      := 3     | r }))
  , ("v4"           , \r _ -> return (Just { verbose      := 4     | r }))
  , ("+interactive" , \r _ -> return (Just { interactive  := True  | r }))
  , ("-interactive" , \r _ -> return (Just { interactive  := False | r }))
  , ("+first"       , \r _ -> return (Just { firstSol     := True  | r }))
  , ("-first"       , \r _ -> return (Just { firstSol     := False | r }))
  , ("+optimize"    , \r _ -> return (Just { optim        := True  | r }))
  , ("-optimize"    , \r _ -> return (Just { optim        := False | r }))
  , ("+bindings"    , \r _ -> return (Just { showBindings := True  | r }))
  , ("-bindings"    , \r _ -> return (Just { showBindings := False | r }))
  , ("+time"        , \r _ -> return (Just { showTime     := True  | r }))
  , ("-time"        , \r _ -> return (Just { showTime     := False | r }))
  , ("+ghci"        , \r _ -> return (Just { useGhci      := True  | r }))
  , ("-ghci"        , \r _ -> do r' <- stopGhciComm r
                                 return (Just { useGhci   := False | r' }))
  , ("cmp"          , \r a -> return (Just { cmpOpts      := a     | r }))
  , ("ghc"          , \r a -> return (Just { ghcOpts      := a     | r }))
  , ("rts"          , \r a -> return (Just { rtsOpts      := a     | r }))
  , ("args"         , \r a -> return (Just { rtsArgs      := a     | r }))
  ]

setOptionPath :: ReplState -> String -> IO (Maybe ReplState)
setOptionPath rst args = do
  ipath <- if null args then defaultImportPaths rst
                        else defaultImportPathsWith rst args
  return (Just { importPaths := ipath | rst })

setOptionNDMode :: (Int -> NonDetMode) -> Int
                -> ReplState -> String -> IO (Maybe ReplState)
setOptionNDMode mode defDepth rst args
  | null args = return (Just { ndMode := mode defDepth | rst })
  | otherwise = case readNat args of
      Nothing    -> skipCommand "illegal number"
      Just (n,s) -> if null (strip s)
                      then return (Just { ndMode := mode n | rst })
                      else skipCommand "illegal number"

setOptionSupply :: ReplState -> String -> IO (Maybe ReplState)
setOptionSupply rst args
  | args `elem` allSupplies = return (Just { idSupply := args | rst })
  | otherwise               = skipCommand "unknown identifier supply"
 where allSupplies = ["integer","ghc","ioref","pureio"]

printOptions :: ReplState -> IO ()
printOptions rst = putStrLn $
  "Options for ':set' command:\n"++
  "path <paths>   - set additional search paths for imported modules\n"++
  "prdfs          - set search mode to primitive depth-first search\n"++
  "dfs            - set search mode to depth-first search\n"++
  "bfs            - set search mode to breadth-first search\n"++
  "ids [<n>]      - set search mode to iterative deepening (initial depth <n>)\n"++
  "par [<n>]      - set search mode to parallel search with <n> threads\n"++
  "choices [<n>]  - set search mode to print the choice structure as a tree\n"++
  "                 (up to level <n>)\n"++
  ifLocal
    "supply <I>     - set idsupply implementation (ghc|integer|ioref|pureio)\n"++
  "v<n>           - verbosity level (0: quiet; 1: front end messages;\n"++
  "                 2: backend messages, 3: intermediate messages and commands;\n"++
  "                 4: all intermediate results)\n"++
  "+/-interactive - turn on/off interactive execution of main goal\n"++
  "+/-first       - turn on/off printing only first solution\n"++
  "+/-optimize    - turn on/off optimization\n"++
  "+/-bindings    - show bindings of free variables in initial goal\n"++
  "+/-time        - show execution time\n"++
  "+/-ghci        - use ghci instead of ghc to evaluate main expression\n"++
  "cmp <opts>     - additional options passed to KiCS2 compiler\n" ++
  ifLocal
   ("ghc <opts>     - additional options passed to GHC\n"++
    "                 (e.g., -DDISABLE_CS, -DSTRICT_VAL_BIND)\n") ++
  "rts <opts>     - run-time options for ghc (+RTS <opts> -RTS)\n" ++
  "args <args>    - run-time arguments passed to main program\n" ++
  showCurrentOptions rst

-- Hide string if the current installation is global:
ifLocal :: String -> String
ifLocal s = if Inst.installGlobal then "" else s

showCurrentOptions :: ReplState -> String
showCurrentOptions rst = "\nCurrent settings:\n"++
  "import paths      : " ++
     intercalate ":" ("." : rst -> importPaths) ++ "\n" ++
  "search mode       : " ++
      (case (rst -> ndMode) of
         PrDFS         -> "primitive non-monadic depth-first search"
         PrtChoices d  -> "show choice tree structure up to level " ++ show d
         DFS           -> "depth-first search"
         BFS           -> "breadth-first search"
         IDS d         -> "iterative deepening (initial depth: "++show d++")"
         Par s         -> "parallel search with "++show s++" threads"
      ) ++ "\n" ++
  "idsupply          : " ++ rst -> idSupply ++ "\n" ++
  "compiler options  : " ++ rst -> cmpOpts ++ "\n" ++
  ifLocal ("ghc options       : " ++ rst->ghcOpts ++ "\n") ++
  "run-time options  : " ++ rst -> rtsOpts ++ "\n" ++
  "run-time arguments: " ++ rst -> rtsArgs ++ "\n" ++
  "verbosity         : " ++ show (rst -> verbose) ++ "\n" ++
  showOnOff (rst -> interactive ) ++ "interactive " ++
  showOnOff (rst -> firstSol    ) ++ "first "       ++
  showOnOff (rst -> optim       ) ++ "optimize "    ++
  showOnOff (rst -> showBindings) ++ "bindings "    ++
  showOnOff (rst -> showTime    ) ++ "time "        ++
  showOnOff (rst -> useGhci     ) ++ "ghci "
 where
   showOnOff b = if b then "+" else "-"

printHelpOnCommands :: IO ()
printHelpOnCommands = putStrLn $ unlines
  [ "Commands (can be abbreviated to a prefix if unique)"
  , ":load <prog>     - load program \"<prog>.[l]curry\" as main module"
  , ":add  <prog>     - add module \"<prog>\" to currently loaded modules"
  , ":reload          - recompile currently loaded modules"
  , ":eval <expr>     - evaluate expression <expr>"
  , ":type <expr>     - show type of expression <expr>"
  , ":programs        - show names of all Curry programs available in load path"
  , ":cd <dir>        - change current directory to <dir>"
  , ":edit            - load source of currently loaded module into editor"
  , ":edit <mod>      - load source of module <m> into editor"
  , ":show            - show currently loaded source program"
  , ":show <mod>      - show source of module <m>"
  , ":source <f>      - show source of (visible!) function <f>"
  , ":source <m>.<f>  - show source of function <f> in module <m>"
  , ":browse          - browse program and its imported modules"
  , ":interface       - show currently loaded source program"
  , ":interface <mod> - show source of module <m>"
  , ":usedimports     - show all used imported functions/constructors"
  , ":set <option>    - set an option"
  , ":set             - see help on options and current options"
  , ":save            - save executable with main expression 'main'"
  , ":save <expr>     - save executable with main expression <expr>"
  , ":fork <expr>     - fork new process evaluating <expr>"
  , ":help            - show this message"
  , ":!<command>      - execute <command> in shell"
  , ":quit            - leave the system"
  ]

--- Print all Curry programs in current load path
printAllLoadPathPrograms :: ReplState -> IO ()
printAllLoadPathPrograms rst = mapIO_ printDirPrograms (loadPath rst)
  where printDirPrograms dir = do
          putStrLn $ "Curry programs in directory " ++ dir ++ ":"
          files <- getDirectoryContents dir
          putStrLn $ concat $ mergeSort (<=) $
            map (\f -> if take 6 (reverse f) == "yrruc." ||
                          take 7 (reverse f) == "yrrucl."
                        then let fb = stripSuffix f
                              in (if null fb then "" else f++" ")
                        else "") files
          putStrLn ""

-- ---------------------------------------------------------------------------
-- Read KiCS2 options in a Curry source file
-- Source file options are comments of the form
-- {-# KiCS2_OPTION <opt> #-}
-- occurring before the module header where <opt> is an option
-- of KiCS2 (i.e., ":set <opt>" is a valid KiCS2 command).
-- These options are read and processed when a module is loaded (not reloaded!).

readAndProcessSourceFileOptions :: ReplState -> String -> IO (Maybe ReplState)
readAndProcessSourceFileOptions rst fname = do
  opts <- readSourceFileOptions fname
  unless (null opts) $ writeVerboseInfo rst 1 $
    "Source file options: " ++ intercalate " | " (map unwords opts)
  processSourceFileOptions rst opts

processSourceFileOptions :: ReplState -> [[String]] -> IO (Maybe ReplState)
processSourceFileOptions rst []     = return (Just rst)
processSourceFileOptions rst (o:os) =
  processSetOption rst (unwords o) >>=
  maybe (return Nothing) (\rst' -> processSourceFileOptions rst' os)

readSourceFileOptions :: String -> IO [[String]]
readSourceFileOptions fname = do
  h <- openFile fname ReadMode
  headers <- readHeaderLines h
  hClose h
  return (filter notNull (map getOptions (filter isOptionComment headers)))
 where
  isOptionComment s = take 3 s == "{-#" -- #-}

  getOptions s =
   let optwords = words (extractOptionString (drop 3 s))
    in if null optwords || map toLower (head optwords) /= "kics2_option"
       then []
       else tail optwords

  extractOptionString []     = ""
  extractOptionString (c:cs) = case cs of
    []  -> ""
    [_] -> ""
    _   -> if (c : take 2 cs) == "#-}" then "" else c : extractOptionString cs

  readHeaderLines h = do
    eof <- hIsEOF h
    if eof then return []
          else do line <- hGetLine h
                  if isModuleStart line
                    then return []
                    else do lines <- readHeaderLines h
                            return (strip line : lines)
   where isModuleStart l = take 6 l `elem` ["module","import"] ||
                           (let (w,_) = break isSpace l
                             in notNull w && all isAlphaNum w)

removeFileIfExists :: String -> IO ()
removeFileIfExists file = do
  exists <- doesFileExist file
  when exists $ removeFile file
