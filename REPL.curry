--- --------------------------------------------------------------------------
--- Read-Eval-Print loop for IDC
---
--- @author Michael Hanus
--- @version April 2011
--- --------------------------------------------------------------------------

import System(system,getArgs,getEnviron)
import Char(isSpace,toLower)
import IO
import IOExts
import SetFunctions
import FileGoodies
import Directory (doesFileExist,removeFile,renameFile,getDirectoryContents)
import Names (funcInfoFile)
import ReadShowTerm (readQTermFile)
import ReadNumeric (readNat)
import List (isPrefixOf,intersperse)
import FlatCurry (flatCurryFileName)
import Sort (mergeSort)
import AbstractCurry
import Distribution
import qualified Installation as Inst
import Files

banner = unlines [bannerLine,bannerText,bannerDate,bannerLine]
 where
   bannerText = "KiCS2 Curry->Haskell Compiler (Version "++
                show Inst.majorVersion ++ "." ++ show Inst.minorVersion ++
                " of "++ Inst.compilerDate ++ ")"
   bannerDate = "(installed at "++Inst.installDate++")"
   bannerLine = take (length bannerText) (repeat '=')

mainGoalFile = "Curry_Main_Goal.curry"

-- Remove mainGoalFile and auxiliaries
cleanMainGoalFile = do
  system $ Inst.installDir++"/bin/cleancurry "++mainGoalFile
  removeFile mainGoalFile

-- REPL state:
type ReplState =
  { idcHome      :: String     -- installation directory of the system
  , idSupply     :: String     -- IDSupply implementation (ioref or integer)
  , quiet        :: Bool       -- be quiet?
  , importPaths  :: [String]   -- additional directories to search for imports
  , outputSubdir :: String
  , mainMod      :: String     -- name of main module
  , addMods      :: [String]   -- names of additionally added modules
  , optim        :: Bool       -- compile with optimization
  , ndMode       :: NonDetMode -- mode for non-deterministic main goal
  , firstSol     :: Bool       -- print only first solution to nd main goal?
  , interactive  :: Bool       -- interactive execution of goal?
  , showBindings :: Bool       -- show free variables in main goal in output?
  , showTime     :: Bool       -- show execution of main goal?
  , rtsOpts      :: String     -- run-time options for ghc
  , quit         :: Bool       -- terminate the REPL?
  }

-- Mode for non-deterministic evaluation of main goal
data NonDetMode = DFS | BFS | IDS Int | Par Int | PrDFS

-- Result of compiling main goal
data MainGoalCompile = GoalError | GoalWithoutBindings | GoalWithBindings

initReplState :: ReplState
initReplState =
  { idcHome      = ""
  , idSupply     = "integer"
  , quiet        = False
  , importPaths  = []
  , outputSubdir = "/.curry/kics2/"
  , mainMod      = "Prelude"
  , addMods      = []
  , optim        = True
  , ndMode       = DFS
  , firstSol     = False
  , interactive  = False
  , showBindings = True
  , showTime     = True
  , rtsOpts      = ""
  , quit         = False
  }

-- Show an info message if not quiet
writeInfo :: ReplState -> String -> IO ()
writeInfo rst msg = if rst -> quiet then done else putStrLn msg

main = do
  let rst = { idcHome := Inst.installDir
            , importPaths := map (Inst.installDir </>) ["/lib","/lib/meta"]
            | initReplState }
  args <- getArgs
  processArgsAndStart rst args

processArgsAndStart rst [] =
  if rst -> quit
  then done
  else do writeInfo rst banner
          writeInfo rst "Type \":h\" for help"
          repl rst
processArgsAndStart rst (arg:args) =
  if head arg /= ':'
  then putStrLn ("Unknown command: " ++ unwords (arg:args)) >> printHelp
  else do let (cmdargs,more) = break (\a -> head a == ':') args
          mbrst <- processCommand rst (tail (unwords (arg:cmdargs)))
          maybe printHelp (\rst' -> processArgsAndStart rst' more) mbrst
 where
  printHelp = putStrLn "Usage: idci <list of commands>\n" >> printHelpOnCommands

-- The main read-eval-print loop:
repl :: ReplState -> IO ()
repl rst = do
  putStr (unwords (rst->addMods ++ [rst-> mainMod]) ++ "> ")
  hFlush stdout
  eof <- isEOF
  if eof then done
   else do input <- getLine
           processInput rst (strip input)

processInput :: ReplState -> String -> IO ()
processInput rst g
  | null g = repl rst
  | head g == ':' = do mbrst <- processCommand rst (strip (tail g))
                       maybe (repl rst)
                             (\rst' -> if (rst'->quit) then done else repl rst')
                             mbrst
  | otherwise = do status <- compileProgramWithGoal rst g
                   unless (status>0) (execMain rst >> done)
                   cleanMainGoalFile
                   repl rst

-- Compile main program with goal:
compileProgramWithGoal :: ReplState -> String -> IO Int
compileProgramWithGoal rst goal = do
  let infoFile = funcInfoFile (rst -> outputSubdir) mainGoalFile
  oldmaincurryexists <- doesFileExist infoFile
  unless (not oldmaincurryexists) $ removeFile infoFile
  oldmainfcyexists <- doesFileExist (flatCurryFileName mainGoalFile)
  unless (not oldmainfcyexists) $ removeFile (flatCurryFileName mainGoalFile)
  writeMainGoalFile rst goal
  goalstate <- insertFreeVarsInMainGoal rst goal
  if goalstate==GoalError then return 1 else do
    status <- compileCurryProgram rst True mainGoalFile
    exinfo <- doesFileExist infoFile
    if status==0 && exinfo then createAndCompileMain rst goalstate
                           else return 1

-- write the file with the main goal:
writeMainGoalFile :: ReplState -> String -> IO ()
writeMainGoalFile rst goal =
  writeFile mainGoalFile
            (unlines $ ["import "++(rst -> mainMod)] ++
                       map ("import "++) (rst->addMods) ++
                       ["idcMainGoal = "++goal])

-- Insert free variables occurring in the main goal as components
-- of the main goal so that their bindings are shown
-- The result is True if the acy file is correct.
insertFreeVarsInMainGoal :: ReplState -> String -> IO MainGoalCompile
insertFreeVarsInMainGoal rst goal = do
  let mainGoalProg = stripSuffix mainGoalFile
      acyMainGoalFile = inCurrySubdir (mainGoalProg ++ ".acy")
  callFrontendWithParams ACY (setQuiet True defaultParams) mainGoalProg
  acyexists <- doesFileExist acyMainGoalFile
  if not acyexists then return GoalError else do
    (CurryProg _ _ _ [mfunc] _) <- readAbstractCurryFile acyMainGoalFile
    removeFile acyMainGoalFile
    let freevars = freeVarsInFuncRule mfunc
    if null freevars || not (rst -> showBindings) || length freevars > 5
     then return GoalWithoutBindings
     else let (exp,whereclause) = break (=="where") (words goal)
           in if null whereclause then return GoalWithoutBindings else do
               writeMainGoalFile
                { addMods := "ShowBindings" : (rst->addMods) | rst}
                (unwords (["ShowBindings.show"++show (length freevars)++" ("]++
                          exp ++ [",["] ++
                          intersperse "," (map (\v->"\""++v++"\"") freevars) ++
                          ["]"] ++
                          map (\v->',':v) freevars ++
                          ")":whereclause))
               return GoalWithBindings
 where
  freeVarsInFuncRule (CFunc _ _ _ _ (CRules _ [CRule _ _ ldecls])) =
    concatMap lvarName ldecls

  lvarName ldecl = case ldecl of CLocalVar (_,v) -> [v]
                                 _               -> []

-- Compile a Curry program with IDC compiler:
compileCurryProgram :: ReplState -> Bool -> String -> IO Int
compileCurryProgram rst quiet curryprog = do
  let compileProg = (rst->idcHome)++"/idc"
      idcoptions  = (if quiet then "-q " else "") ++
                    (concatMap (\i -> " -i "++i) (rst->importPaths))
      compileCmd  = unwords [compileProg,idcoptions,curryprog]
  writeInfo rst $ "Executing: "++compileCmd
  system compileCmd

-- Create and compile the main module containing the main goal
createAndCompileMain :: ReplState -> MainGoalCompile -> IO Int
createAndCompileMain rst goalstate = do
  infos <- readQTermFile (funcInfoFile (rst -> outputSubdir) mainGoalFile)
  --print infos
  let isdet = not (null (filter (\i -> (snd (fst i)) == "d_C_idcMainGoal")
                                infos))
      isio  = snd
               (head
                (filter (\i -> snd (fst i) ==
                           (if isdet then "d" else "nd") ++ "_C_idcMainGoal")
                        infos))
  writeInfo rst $ "Initial goal is " ++
                  (if isdet then "" else "non-") ++ "deterministic and " ++
                  (if isio then "" else "not ") ++ "of IO type..."
  createHaskellMain rst goalstate isdet isio
  let ghcImports = [ rst -> idcHome
                   , rst -> idcHome ++ "/idsupply" ++ rst -> idSupply
                   , "." </> rst -> outputSubdir
                   ]
                   ++ map (</> rst -> outputSubdir) (rst -> importPaths)
      ghcCompile = unwords ["ghc"
                           ,if rst->optim then "-O2" else ""
                           ,"--make"
                           ,if rst->quiet then "-v0" else ""
                           ,"-XMultiParamTypeClasses"
                           ,"-XFlexibleInstances"
                           ,case rst->ndMode of
                              Par _ -> "-threaded"
                              _     -> ""
                           ,"-i"++concat (intersperse ":" ghcImports)
                           ,"." </> rst -> outputSubdir </> "Main.hs"]
                     -- also: -fforce-recomp -funbox-strict-fields ?
  writeInfo rst $ "Compiling Main.hs with: "++ghcCompile
  system ghcCompile

-- Create the Main.hs program containing the call to the initial expression:
createHaskellMain rst goalstate isdet isio =
  let printOperation = if goalstate==GoalWithBindings then "printWithBindings"
                                                      else "print"
      mainPrefix = if isdet then "d_C_" else "nd_C_"
      mainOperation =
        if isio then (if isdet then "evalDIO" else "evalIO" ) else
        if isdet then "evalD" else
        if rst->ndMode == PrDFS then "prdfs "++printOperation
        else let searchSuffix = if rst->interactive then "i" else
                                if rst->firstSol    then "1" else ""
              in ("print" ++ case (rst->ndMode) of
                              DFS   -> "DFS"++searchSuffix
                              BFS   -> "BFS"++searchSuffix
                              IDS d -> "IDS"++searchSuffix++" "++show d
                              Par _ -> "Par"++searchSuffix )++' ':printOperation
   in writeFile ("." </> rst -> outputSubdir </> "Main.hs") $
       "module Main where\n"++
       "import Basics\n"++
       "import Curry_"++stripSuffix mainGoalFile++"\n"++
       "main = "++mainOperation++" "++mainPrefix++"idcMainGoal\n"


-- Execute main program and show run time:
execMain :: ReplState -> IO Int
execMain rst = do
  isubuntu <- isUbuntu
  let timecmd =
        if isubuntu
        then "time --format=\"Execution time: %Us / elapsed: %E\" "
        else -- for Debian-PCs:
          "export TIMEFORMAT=\"Execution time: %2Us / elapsed: %2Es\" && time "
      paropts = case rst->ndMode of
                  Par n -> "-N" ++ (if n==0 then "" else show n)
                  _     -> ""
      maincmd = ("." </> rst -> outputSubdir </> "Main") ++
                (if null (rst->rtsOpts) && null paropts
                 then " "
                 else " +RTS "++rst->rtsOpts++" "++paropts++" -RTS")
      cmd = (if rst->showTime then timecmd else "") ++ maincmd
  writeInfo rst $ "Executing: " ++ maincmd
  system (if rst->interactive then execInteractive cmd else cmd)
 where
  isUbuntu = do
    bsid <- connectToCommand "lsb_release -i" >>= hGetContents
    return (not (isEmpty (set1 findUbuntu bsid)))
   where
    findUbuntu (_++"Ubuntu"++_) = ()

-- all the available commands:
allCommands = ["quit","help","?","load","reload","add",
               "programs","edit","show","set","save"]

-- Process a command of the REPL
processCommand :: ReplState -> String -> IO (Maybe ReplState)
processCommand rst cmds
  | null cmds = putStrLn "Error: unknown command" >> return Nothing
  | head cmds == '!' = system (tail cmds) >> return (Just rst)
  | otherwise = let (cmd,args) = break (==' ') cmds
                    allcmds = filter (isPrefixOf (map toLower cmd)) allCommands
                 in
      if null allcmds
      then putStrLn ("Error: unknown command: ':"++cmds++"'") >> return Nothing
      else if length allcmds > 1
           then putStrLn ("Error: ambiguous command: ':"++cmds++"'") >>
                return Nothing
           else processThisCommand rst (head allcmds) (strip args)

processThisCommand :: ReplState -> String -> String -> IO (Maybe ReplState)
processThisCommand rst cmd args
  | cmd=="quit" = return (Just { quit := True | rst })
  | cmd=="help" || cmd=="?"
   = do printHelpOnCommands
        putStrLn "...or type any <expression> to evaluate\n"
        return (Just rst)
  | cmd == "load"
   = do let modname = stripSuffix args
        compileCurryProgram rst (rst->quiet) modname
        return (Just { mainMod := modname, addMods := [] | rst })
  | cmd == "reload"
   = if rst->mainMod == "Prelude"
     then putStrLn "No program loaded!" >> return Nothing
     else do compileCurryProgram rst (rst->quiet) (rst->mainMod)
             return (Just rst)
  | cmd=="add"
   = do let modname = stripSuffix args
        mbf <- lookupFileInPath modname [".curry", ".lcurry"]
                                ("." : rst->importPaths)
        maybe (putStrLn "Source file of module not found!" >> return Nothing)
              (\_ -> return (Just { addMods := modname : rst->addMods | rst}))
              mbf
  | cmd=="programs" = printAllLoadPathPrograms rst >> return (Just rst)
  | cmd=="edit"
   = do let modname = if null args then rst->mainMod else stripSuffix args
        mbf <- lookupFileInPath modname [".curry", ".lcurry"]
                                ("." : rst->importPaths)
        editenv <- getEnviron "EDITOR"
        let editprog = if null editenv then "vi" else editenv
        maybe (putStrLn "Source file not found!" >> return Nothing)
              (\fn -> system (editprog++" "++fn++"& ") >> return (Just rst))
              mbf
  | cmd=="show"
   = do let modname = if null args then rst->mainMod else stripSuffix args
        mbf <- lookupFileInPath modname [".curry", ".lcurry"]
                                ("." : rst->importPaths)
        maybe (putStrLn "Source file not found!" >> return Nothing)
              (\fn -> system ("cat "++fn) >> putStrLn "" >> return (Just rst))
              mbf
  | cmd=="set" = processSetOption rst args
  | cmd=="save"
   = if rst->mainMod == "Prelude"
     then putStrLn "No program loaded!" >> return Nothing
     else do
       status <- compileProgramWithGoal rst (if null args then "main" else args)
       unless (status>0) $ do
          renameFile ("." </> rst -> outputSubdir </> "Main") (rst->mainMod)
          putStrLn ("Executable saved in '"++rst->mainMod++"'")
       cleanMainGoalFile
       return (Just rst)
  | otherwise = putStrLn ("Error: unknown command: ':"++cmd++"'") >>
                return Nothing

-- Process setting of an option
processSetOption :: ReplState -> String -> IO (Maybe ReplState)
processSetOption rst option
  | null (strip option) = printOptions rst >> return (Just rst)
  | otherwise = let (opt,args) = break (==' ') option
                    allopts = filter (isPrefixOf (map toLower opt)) allOptions
                 in
     if null allopts
     then putStrLn ("Error: unknown option: ':"++option++"'") >> return Nothing
     else if length allopts > 1
          then putStrLn ("Error: ambiguous option: ':"++option++"'") >>
               return Nothing
          else processThisOption rst (head allopts) (strip args)

allOptions = ["bfs","dfs","prdfs","ids","par","supply","rts"] ++
             concatMap (\f->['+':f,'-':f])
                       ["interactive","first","optimize","quiet","bindings",
                        "time"]

processThisOption :: ReplState -> String -> String -> IO (Maybe ReplState)
processThisOption rst option args
  | option=="bfs" = return (Just { ndMode := BFS | rst })
  | option=="dfs" = return (Just { ndMode := DFS | rst })
  | option=="prdfs" = return (Just { ndMode := PrDFS | rst })
  | option=="ids"
   = if null args
     then return (Just { ndMode := IDS 100 | rst })
     else maybe (putStrLn "Illegal number" >> return Nothing)
                (\ (n,s) -> if null (strip s)
                            then return (Just { ndMode := IDS n | rst })
                            else putStrLn "Illegal number" >> return Nothing)
                (readNat args)
  | option=="par"
   = if null args
     then return (Just { ndMode := Par 0 | rst })
     else maybe (putStrLn "Illegal number" >> return Nothing)
                (\ (n,s) -> if null (strip s)
                            then return (Just { ndMode := Par n | rst })
                            else putStrLn "Illegal number" >> return Nothing)
                (readNat args)
  | option=="supply"       = return (Just { idSupply := args | rst })
  | option=="+interactive" = return (Just { interactive := True  | rst })
  | option=="-interactive" = return (Just { interactive := False | rst })
  | option=="+first"       = return (Just { firstSol := True  | rst })
  | option=="-first"       = return (Just { firstSol := False | rst })
  | option=="+optimize"    = return (Just { optim := True  | rst })
  | option=="-optimize"    = return (Just { optim := False | rst })
  | option=="+quiet"       = return (Just { quiet := True  | rst })
  | option=="-quiet"       = return (Just { quiet := False | rst })
  | option=="+bindings"    = return (Just { showBindings := True  | rst })
  | option=="-bindings"    = return (Just { showBindings := False | rst })
  | option=="+time"        = return (Just { showTime := True  | rst })
  | option=="-time"        = return (Just { showTime := False | rst })
  | option=="rts"          = return (Just { rtsOpts := args | rst })
  | otherwise = putStrLn ("Error: unknown option: '"++option++"'") >>
                return Nothing

printOptions rst = putStrLn $
  "Options for ':set' command:\n"++
  "prdfs          - set search mode to primitive depth-first search\n"++
  "dfs            - set search mode to depth-first search\n"++
  "bfs            - set search mode to breadth-first search\n"++
  "ids [<n>]      - set search mode to iterative deepening (initial depth <n>)\n"++
  "par [<n>]      - set search mode to parallel search with <n> threads\n"++
  "supply <I>     - set idsupply implementation (integer or ioref)\n"++
  "+/-interactive - turn on/off interactive execution of main goal\n"++
  "+/-first       - turn on/off printing only first solution\n"++
  "+/-optimize    - turn on/off optimization\n"++
  "+/-quiet       - set quiet mode\n"++
  "+/-bindings    - show bindings of free variables in initial goal\n"++
  "+/-time        - show execution time\n"++
  "rts <opts>     - run-time options for ghc (+RTS <opts> -RTS)\n" ++
  showCurrentOptions rst

showCurrentOptions rst = "\nCurrent settings:\n"++
  "search mode      : " ++
      (case (rst->ndMode) of
         PrDFS -> "primitive non-monadic depth-first search"
         DFS -> "depth-first search"
         BFS -> "breadth-first search"
         IDS d -> "iterative deepening (initial depth: "++show d++")"
         Par s -> "parallel search with "++show s++" threads"
      ) ++ "\n" ++
  "idsupply         : " ++ rst->idSupply ++ "\n" ++
  "run-time options : " ++ rst->rtsOpts ++ "\n" ++
  showOnOff (rst->interactive) ++ "interactive " ++
  showOnOff (rst->firstSol) ++ "first " ++
  showOnOff (rst->optim) ++ "optimize " ++
  showOnOff (rst->quiet) ++ "quiet " ++
  showOnOff (rst->showBindings) ++ "bindings " ++
  showOnOff (rst->showTime) ++ "time "
 where
   showOnOff b = if b then "+" else "-"

printHelpOnCommands = putStrLn $
  "Commands (can be abbreviated to a prefix if unique)\n"++
  ":load <prog>  - load program \"<prog>.[l]curry\" as main module\n"++
  ":add  <prog>  - add module \"<prog>\" to currently loaded modules\n"++
  ":reload       - recompile currently loaded modules\n"++
  ":programs     - show names of all Curry programs available in load path\n"++
  ":edit         - load source of currently loaded module into editor\n"++
  ":edit <mod>   - load source of module <m> into editor\n"++
  ":show         - show currently loaded source program\n"++
  ":show <mod>   - show source of module <m>\n"++
  ":set <option> - set an option\n"++
  ":set          - see help on options and current options\n"++
  ":save         - save executable with main expression 'main'\n"++
  ":save <exp>   - save executable with main expression <exp>\n"++
  ":help         - show this message\n"++
  ":!<command>   - execute <command> in shell\n"++
  ":quit         - leave the system\n"

-- Print all Curry programs in current load path:
printAllLoadPathPrograms rst = mapIO_ printDirPrograms ("." : rst->importPaths)
 where
  printDirPrograms dir = do
    putStrLn $ "Curry programs in directory "++dir++":"
    files <- getDirectoryContents dir
    putStrLn $ concat $ mergeSort (<=) $
      map (\f -> if take 6 (reverse f) == "yrruc." ||
                    take 7 (reverse f) == "yrrucl."
                 then let fb = stripSuffix f
                       in (if null fb then "" else f++" ")
                 else "") files
    putStrLn ""

-----------------------------------------------------------------------
-- Auxiliaries:

unless :: Bool -> IO () -> IO ()
unless p act = if p then done else act

strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- Interactive execution of a main search command: current, a new
-- terminal is opened due to problematic interaction with readline
execInteractive cmd = "xterm -e " ++ cmd

-----------------------------------------------------------------------
