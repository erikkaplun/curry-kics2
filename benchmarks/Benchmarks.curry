-- This program defines the execution of all benchmarks and summarizes
-- their results.

import List(isPrefixOf,intersperse)
import IO
import IOExts
import System
import Time
import SetFunctions
import Char
import ReadShowTerm
import Float

-- home directory of IDC:
idcHome = "../.."

-- home directory of the monadic curry compiler
monHome = "$HOME/.cabal/bin"
monlib  = "$HOME/.cabal/share/curry2monad-0.1"

unless :: Bool -> IO () -> IO ()
unless p act = if p then done else act

evalCmd :: String -> IO String
evalCmd cmd = connectToCommand cmd >>= hGetContents

--isUbuntu :: IO Bool
isUbuntu = do
  bsid <- evalCmd "lsb_release -i"
  return (not (isEmpty (set1 findUbuntu bsid)))
 where
  findUbuntu (_++"Ubuntu"++_) = ()
  -- isInfixOf ? =)

-- Execute shell command and return time of its execution:
benchmarkCommand cmd = do
  isubuntu <- isUbuntu
  let timecmd = if isubuntu
                then "time --format=\"BENCHMARKTIME=%U\" "++cmd
                else -- for Debian-PCs:
                     "export TIMEFORMAT=\"BENCHMARKTIME=%2U\" && time "++cmd
  (hin,hout,herr) <- execCmd timecmd
  outcnt <- hGetContents hout
  errcnt <- hGetContents herr
  hClose hin
  putStr outcnt
  putStr errcnt
  return (extractTimeInOutput errcnt)

-- extract benchmark time from timing output:
extractTimeInOutput =
  tail . snd . break (=='=') . head . filter ("BENCHMARKTIME" `isPrefixOf`)
       . lines

-- Run a set of benchmarks and return the timings
runBenchmarks num benchmarks = do
  results <- mapIO (runBenchmark num) benchmarks
  let maxnamelength = foldr1 max (map (length . fst) results)
  return $ unlines $
    take 8 (repeat '-') :
    map (\ (n,ts) -> n ++ take (maxnamelength - length n) (repeat ' ') ++
                     "|" ++ concat (intersperse "|" (map showFloat ts)))
        (if all (not . null) (map snd results)
         then processResults results
         else results)
 where
  showFloat x = let (x1,x2) = break (=='.') (show x)
                 in take (3-length x1) (repeat ' ') ++ x1 ++ x2 ++
                    take (3-length x2) (repeat '0') ++ " "

processResults results =
  let (names,times) = unzip results
   in zip names (processTimes times)

processTimes timings =
  let means = map mean timings
      roundedmeans = map truncateFloat means
      mintime = foldr1 min means
      minNonZero = if mintime==0.0 then 0.0001 else mintime
      normalized = map (truncateFloat . (/.minNonZero)) means
   in zipWith (:) normalized (if length (head timings) == 1
                              then timings
                              else zipWith (:) roundedmeans timings)
 where
  mean :: [Float] -> Float
  mean xs = (foldr1 (+.) xs) /. (i2f (length xs))

  truncateFloat x = i2f (round (x*.100)) /. 100

-- Run a benchmark and return the timings
runBenchmark :: Int -> (String,IO Int,String,String) -> IO (String,[Float])
runBenchmark num (name,preparecmd,benchcmd,cleancmd) = do
  let line = take 8 (repeat '-')
  putStr (unlines [line, "Running benchmark: "++name, line])
  preparecmd
  times <- mapIO (\_ -> benchmarkCommand benchcmd) [1..num]
  system cleancmd
  putStrLn ("RUNTIMES: " ++ concat (intersperse " | " times))
  if all isFloatString times
   then return (name,map readFloat times)
   else return ("ERROR: "++name++": "++ concat (intersperse " | " times),[])

----------------------------------------------------------------------
-- Does the string contains a float number?
isFloatString :: String -> Bool
isFloatString s = let p = dropWhile isDigit (dropWhile isSpace s) in
  if null p || head p /= '.'
  then False
  else null (dropWhile isSpace (dropWhile isDigit (tail p)))

readFloat :: String -> Float
readFloat s = if isFloatString s then readQTerm s
                                 else error ("ERROR: readFloat: "++s)

----------------------------------------------------------------------
-- Command to compile a module and execute main with idcompiler:
-- arg1: module name
-- arg2: compile with optimization?
-- arg3: idsupply implementation (integer or ioref)
-- arg4: main (Haskell!) call
idcCompile mod optim idsupply mainexp = do
  let compileCmd = idcHome++"/idc -q -i "++idcHome++"/lib"++" "++mod
  putStrLn $ "Executing: "++compileCmd
  system compileCmd
  createHaskellMainAndCompile mod optim idsupply mainexp

-- Create the Main.hs program containing the call to the initial expression:
createHaskellMainAndCompile mod optim idsupply mainexp = do
  writeFile "Main.hs" $
       "module Main where\n"++
       "import Basics\n"++
       "import Curry_"++mod++"\n"++
       "main = "++mainexp++"\n"
  putStrLn $ "Main expression: " ++ mainexp
  let imports = [idcHome,idcHome++"/idsupply"++idsupply,idcHome++"/lib"]
      compileCmd = unwords ["ghc",if optim then "-O2" else "","--make",
                            "-XMultiParamTypeClasses","-XFlexibleInstances",
                            "-fforce-recomp",
                            "-i"++concat (intersperse ":" imports),"Main.hs"]
                     -- also:  -funbox-strict-fields ?
  putStrLn $ "Executing: "++compileCmd
  system compileCmd

----------------------------------------------------------------------
-- Command to compile a module and execute main with monadic curry:
-- arg1: module name
-- arg2: compile with optimization?
-- arg3: main (Curry!) call
monCompile mod optim mainexp = do
  let compileCmd = monHome++"/curry2monad -m"++mainexp++" "++mod
  putStrLn $ "Executing: "++compileCmd
  system compileCmd
  createMonHaskellMainAndCompile mod optim mainexp

createMonHaskellMainAndCompile mod optim mainexp = do
  let haskellMain = "cM_" ++ mainexp
  writeFile "Main.hs" $
       "module Main where\n" ++
       "import Curry_"++mod++"\n"++
       "main = print $ "++haskellMain++"\n"
  putStrLn $ "Main expression: " ++ haskellMain
  let imports = [monlib]
      compileCmd = unwords ["ghc",if optim then "-O2" else "","--make",
                            "-fforce-recomp",
                            "-i"++concat (intersperse ":" imports),"Main.hs"]
  putStrLn $ "Executing: "++ compileCmd
  system compileCmd
----------------------------------------------------------------------
-- Command to compile a module and execute main with MCC:
--mccCompile mod = "/home/mcc/bin/cyc -e\"print main\" " ++ mod ++".curry"
mccCompile options mod = system $
  "/home/mcc/bin/cyc " ++
  (if null options then "-e\"main\"" else options) ++
  " " ++ mod ++".curry"


-- Command to compile a module and execute main with GHC:
ghcCompile mod = system $ "ghc --make -fforce-recomp " ++ mod

-- Command to compile a module and execute main with GHC (optimized):
ghcCompileO mod = system $ "ghc -O2 --make -fforce-recomp " ++ mod

-- Command to compile a module and print main in PAKCS:
pakcsCompile options mod = system $
  "/home/pakcs/pakcs/bin/pakcs "++
  (if null options then "-m \"print main\"" else options) ++" -s  " ++ mod

-- Command to compile a Prolog program and run main in SICStus-Prolog:
sicstusCompile mod = system $
  "echo \"compile("++mod++"), save_program('"++mod++".state',main).\" | /home/sicstus/sicstus4/bin/sicstus && chmod +x "++mod++".state"

-- Command to compile a Prolog program and run main in SWI-Prolog:
swiCompile mod = system $
  "echo \"compile("++mod++"), qsave_program('"++mod++".state',[toplevel(main)]).\" | /home/swiprolog/bin/swipl"

----------------------------------------------------------------------
-- Benchmarks for various systems

idcBenchmark tag mod optim idsupply mainexp =
  (mod++"@"++tag, idcCompile mod optim idsupply mainexp,
   "./Main", "rm Main* Curry_*")
monBenchmark tag mod optim mainexp =
  (mod++"@"++tag, monCompile mod optim mainexp,
   "./Main", "rm Main* Curry_*") 
pakcsBenchmark options mod =
  (mod++"@PAKCS ",pakcsCompile options mod,
   "./"++mod++".state","rm "++mod++".state")
mccBenchmark options mod =
  (mod++"@MCC   ",mccCompile options mod,"./a.out +RTS -h512m -RTS",
   "rm a.out "++mod++".icurry")
ghcBenchmark mod =
  (mod++"@GHC   ",ghcCompile mod,"./"++mod,
   "rm "++mod++" "++mod++".hi "++mod++".o")
ghcOBenchmark mod =
  (mod++"@GHC+  ",ghcCompileO mod,"./"++mod,
   "rm "++mod++" "++mod++".hi "++mod++".o")
sicsBenchmark mod =
  (mod++"@SICS  ", sicstusCompile mod,
   "./"++mod++".state", "rm "++mod++".state")
swiBenchmark  mod =
  (mod++"@SWI   ", swiCompile mod, "./"++mod++".state", "rm "++mod++".state")

----------------------------------------------------------------------
-- The various kinds of benchmarks:

-- Benchmarking functional programs with idc/pakcs/mcc/ghc/prolog
benchFPpl prog withMon =
 [idcBenchmark "IDC_D"  prog False "integer" "evalD d_C_main"
 ,idcBenchmark "IDC+_D" prog True  "integer" "evalD d_C_main"
 ,pakcsBenchmark "" prog
 ,mccBenchmark ""   prog
 ,ghcBenchmark   prog
 ,ghcOBenchmark  prog
 ,sicsBenchmark  (map toLower prog)
 ,swiBenchmark   (map toLower prog)
 ] 
 ++ (if withMon then [monBenchmark "MON+" prog True "main"] else [])

-- Benchmarking higher-order functional programs with idc/pakcs/mcc/ghc
benchHOFP prog withMon =
 [idcBenchmark "IDC"    prog False "integer" "eval nd_C_main"
 ,idcBenchmark "IDC+"   prog True  "integer" "eval nd_C_main"
 ,idcBenchmark "IDC_D"  prog False "integer" "evalD d_C_main"
 ,idcBenchmark "IDC+_D" prog True  "integer" "evalD d_C_main"
 ,pakcsBenchmark "" prog
 ,mccBenchmark ""   prog
 ,ghcBenchmark   prog
 ,ghcOBenchmark  prog
 ]
 ++ if withMon then [monBenchmark "MON+" prog True "main"] else []

-- Benchmarking functional logic programs with idc/pakcs/mcc in DFS mode
benchFLPDFS prog withMon =
 [idcBenchmark "IDC_PrDFS"        prog False "integer" "prdfs nd_C_main"
 ,idcBenchmark "IDC+_PrDFS"       prog True  "integer" "prdfs nd_C_main"
 ,idcBenchmark "IDC+_PrDFS_IORef" prog True  "ioref"   "prdfs nd_C_main"
 ,pakcsBenchmark "" prog
 ,mccBenchmark ""   prog
 ]++ if withMon then [monBenchmark "MON+" prog True "main"] else []

-- Benchmarking functional logic programs with unification with idc/pakcs/mcc
benchFLPDFSU prog =
 [idcBenchmark "IDC+_PrDFS_IORef" prog True  "ioref"   "prdfs nd_C_main"
 ,idcBenchmark "IDC+_DFS_IORef"   prog True  "ioref"   "printDFS nd_C_main"
 ,pakcsBenchmark "" prog
 ,mccBenchmark ""   prog
 ]

-- Benchmarking functional patterns with idc/pakcs
benchFunPats prog =
 [idcBenchmark "IDC+_PrDFS_IORef" prog True  "ioref"   "prdfs nd_C_main"
 ,idcBenchmark "IDC+_DFS_IORef"   prog True  "ioref"   "printDFS nd_C_main"
 ,pakcsBenchmark "" prog
 ]

-- Benchmarking functional logic programs with idc/pakcs/mcc in DFS mode
-- with a given name for the main operation
benchFLPDFSWithMain prog name =
 [idcBenchmark ("IDC_PrDFS:"++name)  prog False "integer" ("prdfs nd_C_"++name)
 ,idcBenchmark ("IDC+_PrDFS:"++name) prog True  "integer" ("prdfs nd_C_"++name)
 ,idcBenchmark ("IDC+_PrDFS_IORef:"++name)
               prog True  "ioref" ("prdfs nd_C_"++name)
 ,pakcsBenchmark ("-m \"print "++name++"\"") prog
 ,mccBenchmark ("-e\""++name++"\"")   prog
 ]

-- Benchmarking functional logic programs with different search strategies
benchFLPSearch prog =
 [idcBenchmark "IDC+PrDFS_IOREF" prog True "ioref" "prdfs nd_C_main"
 ,idcBenchmark "IDC+DFS_IOREF"   prog True "ioref" "printDFS nd_C_main"
 ,idcBenchmark "IDC+BFS_IOREF"   prog True "ioref" "printBFS nd_C_main"
 ,idcBenchmark "IDC+IDS_IOREF"   prog True "ioref" "printIDS 100 nd_C_main"
 ]

-- Benchmarking FL programs that require complete search strategy
benchFLPCompleteSearch prog =
 [idcBenchmark "IDC+BFS_IOREF"   prog True "ioref" "printBFS1 nd_C_main"
 ,idcBenchmark "IDC+IDS_IOREF"   prog True "ioref" "printIDS1 100 nd_C_main"
 ]


allBenchmarks =
  [ benchFPpl "ReverseUser"     True
  , benchFPpl "Reverse"         True
  , benchFPpl "Tak"             True
  , benchFPpl "TakPeano"        True
  , benchHOFP "ReverseHO"       True
  , benchHOFP "ReverseBuiltin"  False
  , benchHOFP "Primes"          True
  , benchHOFP "PrimesPeano"     True
  , benchHOFP "PrimesBuiltin"   True
  , benchHOFP "Queens"          True
  , benchHOFP "QueensUser"      True
  , benchFLPDFS "PermSort"      True
  , benchFLPDFS "PermSortPeano" True
  , benchFLPDFS "Half"          False
  , benchFLPSearch "PermSort"
  , benchFLPSearch "PermSortPeano"
  , benchFLPSearch "Half"
  , benchFLPCompleteSearch "NDNums"
  , benchFLPDFSWithMain "ShareNonDet" "goal1"
  , benchFLPDFSWithMain "ShareNonDet" "goal2"
  , benchFLPDFSWithMain "ShareNonDet" "goal3"
  , benchFLPDFSU "Last"
  , benchFLPDFSU "RegExp"
  , benchFunPats "LastFunPats"
  , benchFunPats "ExpVarFunPats"
  , benchFunPats "ExpSimpFunPats"
  , benchFunPats "PaliFunPats"
  ]

-- Run all benchmarks and show results
run num benchmarks = do
  args <- getArgs
  results <- mapIO (runBenchmarks num) benchmarks
  ltime <- getLocalTime
  info <- evalCmd "uname -a"
  mach <- evalCmd "uname -n"
  let res = "Benchmarks at " ++ info ++ "\n" ++
            "Format of timings: normalized|mean|runtimes...\n\n" ++
            concat results
  putStrLn res
  unless (null args) $ writeFile (outputFile (head args) (init mach) ltime) res
    where
      init :: [a] -> [a]
      init []  = []
      init [_] = []
      init (x:y:zs) = x : init (y : zs)

outputFile :: String -> String -> CalendarTime -> String
outputFile name mach (CalendarTime ye mo da ho mi se _) = "../results/" ++
  name ++ '@' : mach ++ (concat $ intersperse "_" $  (map show [ye, mo, da, ho, mi, se])) ++ ".bench"

--main = run 2 allBenchmarks
--main = run 1 allBenchmarks
--main = run 1 [benchFLPCompleteSearch "NDNums"]
--main = run 1 (map (\g -> benchFLPDFSWithMain "ShareNonDet" g)
--                  ["goal1","goal2","goal3"])
--main = run 3 [benchHOFP "PrimesPeano"]
--main = run 1 [benchFLPDFS "PermSort",benchFLPDFS "PermSortPeano"]
--main = run 1 [benchFLPSearch "PermSort",benchFLPSearch "PermSortPeano"]
--main = run 1 [benchFLPSearch "Half"]
main = run 1 [benchFLPDFSU "Last"]

--main = run 1 [benchFLPDFSU "RegExp"]
--main = run 1 (map benchFunPats ["ExpVarFunPats","ExpSimpFunPats","PaliFunPats"
