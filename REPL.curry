--- Read-Eval-Print loop for IDC

import System(getEnviron,system)
import Char(isSpace)
import IO
import IOExts
import SetFunctions
import FileGoodies
import Directory (doesFileExist)
import Names
import ReadShowTerm (readQTermFile)
import List (isPrefixOf)

banner = bannerLine ++ '\n' : bannerText ++ '\n' : bannerLine ++ "\n"
 where
   bannerText = "ID-based Curry->Haskell Compiler (Version of 11/03/11)"
   bannerLine = take (length bannerText) (repeat '=')

mainGoalFile = "Curry_Main_Goal.curry"

-- REPL state:
type ReplState =
  { idcHome :: String  -- installation directory of the system
  , quiet   :: Bool    -- be quiet?
  , importPaths :: [String] -- additional directories to search for imports
  , mainMod :: String  -- main module
  , optim   :: Bool    -- compile with optimization
  , ndMode  :: NonDetMode -- mode for nd main goal
  , oneSol  :: Bool    -- print only first solution to nd main goal?
  , interactive :: Bool -- interactive execution of goal?
  }

-- Mode for non-deterministic evaluation of main goal
data NonDetMode = DFS | BFS | IDS

initReplState :: ReplState
initReplState = { idcHome = ""
                , quiet   = True
                , importPaths = []
                , mainMod = "Prelude"
                , optim   = False
                , ndMode  = DFS
                , oneSol  = False
                , interactive = False
                }

main = do
  putStrLn banner
  putStrLn "Type \":h\" for help"
  idchome <- getEnviron "IDCHOME"
  if null idchome
   then putStrLn "Environment variable IDCHOME undefined!"
   else repl { idcHome := idchome
             , importPaths := [idchome++"/lib"] | initReplState }

-- The main read-eval-print loop:
repl rst = do
  putStr ((rst -> mainMod) ++ "> ")
  hFlush stdout
  eof <- isEOF
  if eof then done
   else getLine >>= processInput rst

processInput rst g
  | null g = repl rst
  | head g == ':' = processCommand rst (strip (tail g))
  | otherwise = compileAndExecGoal rst g >> repl rst

-- Compile main program with goal:
compileAndExecGoal rst goal = do
  writeFile mainGoalFile
            ("import "++(rst -> mainMod)++"\nidcMainGoal = "++goal++"\n")
  let compileCmd = (rst->idcHome)++"/idc"
      idcoptions = (if rst->quiet then "-q " else "") ++
                   (concatMap (\i -> "-i "++i) (rst->importPaths))
  putStrLn $ "Compiling with options: "++idcoptions
  status <- system (unwords [compileCmd,idcoptions,mainGoalFile])
  exinfo <- doesFileExist (funcInfoFile mainGoalFile)
  unless (status>0 || not exinfo) (createAndExecMain rst)

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
  putStrLn $ "Initial goal is " ++
             (if isdet then "" else "non-") ++ "deterministic and " ++
             (if isio then "" else "not ") ++ "of IO type..."
  let compilescript = (rst->idcHome)++"/compilecurry "
      options = "-e idcMainGoal --nocompile "++
                (if (rst->optim) then "-o " else "") ++
                (if isdet then "-d " else
                 if (rst->interactive) then
                    case rst->ndMode of
                      DFS -> "--dfsi "
                      BFS -> "--bfsi "
                      IDS -> "--idsi "
                 else if (rst->oneSol) then
                    case rst->ndMode of
                      DFS -> "--dfs1 "
                      BFS -> "--bfs1 "
                      IDS -> "--ids1 "
                  else
                    case rst->ndMode of
                      DFS -> "--dfs "
                      BFS -> "--bfs "
                      IDS -> "--ids ") ++
                (if isio then "-io " else "")
  putStrLn $ "Compiling with options: "++options
  status <- system (unwords [compilescript,options,mainGoalFile])
  unless (status>0) (execMain rst)

-- Execute main program and show run time:
execMain rst = do
  isubuntu <- isUbuntu
  let timecmd = (if isubuntu
                 then "time --format=\"Execution time: %Us\" "
                 else -- for Debian-PCs:
                      "export TIMEFORMAT=\"Execution time: %2Us\" && time ")
                ++ " ./Main"
  putStrLn "Executing main goal..."
  system (if rst->interactive then "xterm -e ./Main" else timecmd) >> done
 where
  isUbuntu = do
    bsid <- connectToCommand "lsb_release -i" >>= hGetContents
    return (not (isEmpty (set1 findUbuntu bsid)))
   where
    findUbuntu (_++"Ubuntu"++_) = ()

allCommands = ["quite","help","?","load","show","optimize","interactive",
               "one","dfs","bfs","ids"]

-- Process a command of the REPL
processCommand rst cmds 
  | null cmds = putStrLn "Error: unknown command" >> repl rst
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
            compilescript = (rst->idcHome)++"/compilecurry "
        system (unwords [compilescript,"--nomain",modname])
        repl { mainMod := modname | rst }
  | cmd=="show"
   = do mbf <- lookupFileInPath (rst->mainMod) [".curry", ".lcurry"]
                                ("." : rst->importPaths)
        maybe (putStrLn "Source file not found!")
              (\fn -> system ("cat "++fn) >> done)
              mbf
        repl rst
  | cmd=="optimize"
   = do putStrLn $ "Optimization turned "++
                   if (rst -> optim) then "off" else "on"
        repl { optim := not (rst -> optim) | rst }
  | cmd=="interactive"
   = do putStrLn $ "Interactive execution turned "++
                   if (rst -> interactive) then "off" else "on"
        repl { interactive := not (rst -> interactive) | rst }
  | cmd=="one" = do putStrLn $ "First solution execution turned "++
                               if (rst -> oneSol) then "off" else "on"
                    repl { oneSol := not (rst -> oneSol) | rst }
  | cmd=="dfs" = do putStrLn "Search mode set to DFS"
                    repl { ndMode := DFS | rst }
  | cmd=="bfs" = do putStrLn "Search mode set to BFS"
                    repl { ndMode := BFS | rst }
  | cmd=="ids" = do putStrLn "Search mode set to IDS"
                    repl { ndMode := IDS | rst }
  | otherwise = putStrLn ("Error: unknown command: ':"++cmd++"'") >> repl rst

printHelp = putStrLn $ banner ++
  "Commands (can be abbreviated to a prefix if unique)\n"++
  ":load <prog>  - load program \"<prog>.curry\" as main program\n"++
  "<expression>  - evaluate <expression> to normal form and show results\n"++
  ":show         - show currently loaded source program\n"++
  ":optimize     - turn on/off optimization\n"++
  ":interactive  - turn on/off interactive execution of main goal\n"++
  ":one          - turn on/off printing only first solution\n"++
  ":dfs          - set search mode to depth-first search\n"++
  ":bfs          - set search mode to breadth-first search\n"++
  ":ids          - set search mode to iterative deepening search\n"++
  ":help         - show this message\n"++
  ":quite        - leave the system\n"

-----------------------------------------------------------------------
-- Auxiliaries:

unless :: Bool -> IO () -> IO ()
unless p act = if p then done else act

strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-----------------------------------------------------------------------
