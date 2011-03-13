--- Read-Eval-Print loop for IDC

import System(getEnviron,system)
import Char(isSpace)
import IO
import IOExts
import SetFunctions
import FileGoodies
import Directory (doesFileExist,removeFile)
import Names
import ReadShowTerm (readQTermFile)
import List (isPrefixOf,intersperse)
import FlatCurry(flatCurryFileName)

banner = bannerLine ++ '\n' : bannerText ++ '\n' : bannerLine ++ "\n"
 where
   bannerText = "ID-based Curry->Haskell Compiler (Version of 13/03/11)"
   bannerLine = take (length bannerText) (repeat '=')

mainGoalFile = "Curry_Main_Goal.curry"

-- REPL state:
type ReplState =
  { idcHome     :: String  -- installation directory of the system
  , idSupply    :: String  -- IDSupply implementation (ioref or integer)
  , quiet       :: Bool    -- be quiet?
  , importPaths :: [String] -- additional directories to search for imports
  , mainMod     :: String  -- main module
  , optim       :: Bool    -- compile with optimization
  , ndMode      :: NonDetMode -- mode for nd main goal
  , oneSol      :: Bool    -- print only first solution to nd main goal?
  , interactive :: Bool -- interactive execution of goal?
  }

-- Mode for non-deterministic evaluation of main goal
data NonDetMode = DFS | BFS | IDS | Par

initReplState :: ReplState
initReplState = { idcHome     = ""
                , idSupply    = "integer"
                , quiet       = False
                , importPaths = []
                , mainMod     = "Prelude"
                , optim       = False
                , ndMode      = DFS
                , oneSol      = False
                , interactive = False
                }

-- Show an info message if not quiet
writeInfo :: ReplState -> String -> IO ()
writeInfo rst msg = if rst->quiet then done else putStrLn msg

main = do
  idchome <- getEnviron "IDCHOME"
  if null idchome
   then putStrLn "Environment variable IDCHOME undefined!"
   else do let rst = { idcHome := idchome
                     , importPaths := [idchome++"/lib"] | initReplState }
           writeInfo rst banner
           writeInfo rst "Type \":h\" for help"
           repl rst

-- The main read-eval-print loop:
repl rst = do
  putStr ((rst -> mainMod) ++ "> ")
  hFlush stdout
  eof <- isEOF
  if eof then done
   else do input <- getLine
           processInput rst (strip input)

processInput rst g
  | null g = repl rst
  | head g == ':' = processCommand rst (strip (tail g))
  | otherwise = compileAndExecGoal rst g >> repl rst

-- Compile main program with goal:
compileAndExecGoal rst goal = do
  oldmaincurryexists <- doesFileExist (funcInfoFile mainGoalFile)
  unless (not oldmaincurryexists) $ removeFile (funcInfoFile mainGoalFile)
  oldmainfcyexists <- doesFileExist (flatCurryFileName mainGoalFile)
  unless (not oldmainfcyexists) $ removeFile (flatCurryFileName mainGoalFile)
  writeFile mainGoalFile
            ("import "++(rst -> mainMod)++"\nidcMainGoal = "++goal++"\n")
  status <- compileCurryProgram rst mainGoalFile
  exinfo <- doesFileExist (funcInfoFile mainGoalFile)
  unless (status>0 || not exinfo) $ createAndExecMain rst

-- Compile a Curry program with IDC compiler:
compileCurryProgram :: ReplState -> String -> IO Int
compileCurryProgram rst curryprog = do
  let compileProg = (rst->idcHome)++"/idc"
      idcoptions  = "-q " ++ --(if rst->quiet then "-q " else "") ++
                    (concatMap (\i -> "-i "++i) (rst->importPaths))
      compileCmd  = unwords [compileProg,idcoptions,curryprog]
  writeInfo rst $ "Executing: "++compileCmd
  system compileCmd

createAndExecMain rst = do
  infos <- readQTermFile (funcInfoFile mainGoalFile)
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
  createHaskellMain rst isdet isio
  let ghcImports = [rst->idcHome,rst->idcHome++"/idsupply"++rst->idSupply]
                   ++ rst->importPaths
      ghcCompile = unwords ["ghc",
                            if rst->optim then "-O2" else "",
                            "--make",
                            if rst->ndMode == Par then "-threaded" else "",
                            "-i"++concat (intersperse ":" ghcImports),"Main.hs"]
                     -- also: -fforce-recomp -funbox-strict-fields ?
  writeInfo rst $ "Compiling Main.hs with: "++ghcCompile
  status <- system ghcCompile
  unless (status>0) (execMain rst)

-- Create the Main.hs program containing the call to the initial expression:
createHaskellMain rst isdet isio =
  let mainPrefix = if isdet then "d_C_" else "nd_C_"
      mainOperation =
        if isio then (if isdet then "evalDIO" else "evalIO" ) else
        if isdet then "evalD"
        else "print"++show (rst->ndMode)++
             (if (rst->interactive) then "i" else
              if (rst->oneSol) then "1" else "")
   in writeFile "Main.hs" $
       "module Main where\n"++
       "import Basics\n"++
       "import Curry_"++stripSuffix mainGoalFile++"\n"++
       "main = "++mainOperation++" "++mainPrefix++"idcMainGoal\n"
     

-- Execute main program and show run time:
execMain rst = do
  isubuntu <- isUbuntu
  let timecmd = (if isubuntu
                 then "time --format=\"Execution time: %Us\" "
                 else -- for Debian-PCs:
                      "export TIMEFORMAT=\"Execution time: %2Us\" && time ")
                ++ " ./Main"
  writeInfo rst "Executing main goal..."
  system (if rst->interactive then "xterm -e ./Main" else timecmd) >> done
 where
  isUbuntu = do
    bsid <- connectToCommand "lsb_release -i" >>= hGetContents
    return (not (isEmpty (set1 findUbuntu bsid)))
   where
    findUbuntu (_++"Ubuntu"++_) = ()

-- all the available commands:
allCommands = ["quit","help","?","load","show","set"]

-- Process a command of the REPL
processCommand rst cmds 
  | null cmds = putStrLn "Error: unknown command" >> repl rst
  | head cmds == '!' = system (tail cmds) >> repl rst
  | otherwise = let (cmd,args) = break (==' ') cmds
                    allcmds = filter (isPrefixOf cmd) allCommands
                 in
      if null allcmds
      then putStrLn ("Error: unknown command: ':"++cmds++"'") >> repl rst
      else if length allcmds > 1
           then putStrLn ("Error: ambiguous command: ':"++cmds++"'") >> repl rst
           else processThisCommand rst (head allcmds) (strip args)

processThisCommand rst cmd args
  | cmd=="quit" = done
  | cmd=="help" || cmd=="?" = printHelp >> repl rst
  | cmd == "load"
   = do let modname = stripSuffix args
        compileCurryProgram rst modname
        repl { mainMod := modname | rst }
  | cmd=="show"
   = do mbf <- lookupFileInPath (rst->mainMod) [".curry", ".lcurry"]
                                ("." : rst->importPaths)
        maybe (putStrLn "Source file not found!")
              (\fn -> system ("cat "++fn) >> done)
              mbf
        repl rst
  | cmd=="set" = processSetOption rst args >>= repl
  | otherwise = putStrLn ("Error: unknown command: ':"++cmd++"'") >> repl rst

-- Process setting of an option
processSetOption rst option
  | null (strip option) = printOptions rst >> return rst
  | option=="bfs" = return { ndMode := BFS | rst }
  | option=="dfs" = return { ndMode := DFS | rst }
  | option=="ids" = return { ndMode := IDS | rst }
  | option=="par" = return { ndMode := Par | rst }
  | take 8 option == "idsupply"
   = return { idSupply := strip (drop 8 option) | rst }
  | option=="+interactive" = return { interactive := True  | rst }
  | option=="-interactive" = return { interactive := False | rst }
  | option=="+one" = return { oneSol := True  | rst }
  | option=="-one" = return { oneSol := False | rst }
  | option=="+optimize" = return { optim := True  | rst }
  | option=="-optimize" = return { optim := False | rst }
  | option=="+quiet" = return { quiet := True  | rst }
  | option=="-quiet" = return { quiet := False | rst }
  | otherwise = putStrLn ("Error: unknown option: '"++option++"'") >> return rst

printOptions rst = putStrLn $
  "Options for ':set' command:\n"++
  "dfs            - set search mode to depth-first search\n"++
  "bfs            - set search mode to breadth-first search\n"++
  "ids            - set search mode to iterative deepening search\n"++
  "par            - set search mode to parallel search\n"++
  "idsupply <I>   - set idsupply implementation (integer or ioref)\n"++
  "+/-interactive - turn on/off interactive execution of main goal\n"++
  "+/-one         - turn on/off printing only first solution\n"++
  "+/-optimize    - turn on/off optimization\n"++
  "+/-quiet       - set quiet mode\n"++
  showCurrentOptions rst

showCurrentOptions rst = "Current settings:\n"++
  "search mode  : " ++ (case (rst->ndMode) of
                        DFS -> "depth-first search"
                        BFS -> "breadth-first search"
                        IDS -> "iterative deepening"
                     ) ++ "\n" ++
  "idsupply     : " ++ rst->idSupply ++ "\n" ++
  showOnOff (rst->interactive) ++ "interactive " ++
  showOnOff (rst->oneSol) ++ "one " ++
  showOnOff (rst->optim) ++ "optimize " ++
  showOnOff (rst->quiet) ++ "quiet "
 where
   showOnOff b = if b then "+" else "-"
  
printHelp = putStrLn $ banner ++
  "Commands (can be abbreviated to a prefix if unique)\n"++
  ":load <prog>  - load program \"<prog>.curry\" as main program\n"++
  "<expression>  - evaluate <expression> to normal form and show results\n"++
  ":show         - show currently loaded source program\n"++
  ":set <option> - set an option\n"++
  ":set          - see help on options and current options\n"++
  ":help         - show this message\n"++
  ":!<command>   - execute <command> in shell\n"++
  ":quit         - leave the system\n"

-----------------------------------------------------------------------
-- Auxiliaries:

unless :: Bool -> IO () -> IO ()
unless p act = if p then done else act

strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-----------------------------------------------------------------------
