--- Read-Eval-Print loop for IDC

import System(getEnviron,system)
import Char(isSpace)
import IO
import IOExts
import SetFunctions
import FileGoodies

banner = bannerLine ++ '\n' : bannerText ++ '\n' : bannerLine ++ "\n"
 where
   bannerText = "ID-based Curry->Haskell Compiler (Version of 11/03/11)"
   bannerLine = take (length bannerText) (repeat '=')

mainGoalFile = "Curry_Main_Goal.curry"

-- REPL state:
type ReplState =
  { idcHome :: String  -- installation directory of the system
  , mainMod :: String  -- main module
  , optim   :: Bool    -- compile with optimization
  , detGoal :: Bool    -- call deterministic version of main goal?
  , ioGoal  :: Bool    -- call main goal of IO type?
  , ndMode  :: NonDetMode -- mode for nd main goal
  , oneSol  :: Bool    -- print only first solution to nd main goal?
  , interactive :: Bool -- interactive execution of goal?
  }

-- Mode for non-deterministic evaluation of main goal
data NonDetMode = DFS | BFS | IDS

initReplState :: ReplState
initReplState = { idcHome = ""
                , mainMod = "Prelude"
                , optim   = False
                , detGoal = False
                , ioGoal  = False
                , ndMode  = DFS
                , oneSol  = False
                , interactive = False
                }

main = do
  putStrLn banner
  idchome <- getEnviron "IDCHOME"
  if null idchome
   then putStrLn "Environment variable IDCHOME undefined!"
   else repl { idcHome := idchome | initReplState }

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
compileAndExecGoal rst g = do
  writeMain (rst -> mainMod) g
  let compilescript = (rst->idcHome)++"/compilecurry "
      options = "-e idcMainGoal "++
                (if (rst->optim) then "-o " else "") ++
                (if (rst->ioGoal) then "-io " else "") ++
                (if (rst->detGoal) then "-d " else
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
                      IDS -> "--ids ")
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

-- Process a command of the REPL
processCommand rst cmd 
  | null cmd = putStrLn "Error: unknown command" >> repl rst
  | cmd=="q" = done
  | cmd=="h" || cmd=="?" = printHelp >> repl rst
  | head cmd == 'l' = repl { mainMod := stripSuffix (strip (tail cmd)) | rst }
  | cmd=="o" = do putStrLn $ "Optimization turned "++
                            if (rst -> optim) then "off" else "on"
                  repl { optim := not (rst -> optim) | rst }
  | cmd=="d" = do putStrLn $ "Determinism version turned "++
                            if (rst -> detGoal) then "off" else "on"
                  repl { detGoal := not (rst -> detGoal) | rst }
  | cmd=="int" = do putStrLn $ "Interactive execution turned "++
                               if (rst -> interactive) then "off" else "on"
                    repl { interactive := not (rst -> interactive) | rst }
  | cmd=="io"  = do putStrLn $ "IO execution turned "++
                               if (rst -> ioGoal) then "off" else "on"
                    repl { ioGoal := not (rst -> ioGoal) | rst }
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

-- Write main program containing the goal:
writeMain mainprog goal =
  writeFile mainGoalFile
            ("import "++mainprog++"\nidcMainGoal = "++goal++"\n")

printHelp = putStrLn $ banner ++
  "Commands\n"++
  ":l <prog>     - load program \"<prog>.curry\" as main program\n"++
  "<expression>  - evaluate <expression> to normal form and show results\n"++
  ":o            - turn on/off optimization\n"++
  ":d            - turn on/off call deterministic main goal\n"++
  ":int          - turn on/off interactive execution of main goal\n"++
  ":io           - turn on/off execution of main goal of IO type\n"++
  ":one          - turn on/off printing only first solution\n"++
  ":dfs          - set search mode to depth-first search\n"++
  ":bfs          - set search mode to breadth-first search\n"++
  ":ids          - set search mode to iterative deepening search\n"++
  ":h            - show this message\n"++
  ":q            - leave the system\n"

-----------------------------------------------------------------------
-- Auxiliaries:

unless :: Bool -> IO () -> IO ()
unless p act = if p then done else act

strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-----------------------------------------------------------------------
