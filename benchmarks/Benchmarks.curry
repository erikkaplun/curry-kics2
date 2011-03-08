-- This program defines the execution of all benchmarks and summarizes
-- their results.

import List(isPrefixOf,intersperse)
import IO
import IOExts
import System
import Time
import SetFunctions
import Char(toLower)

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
                     "export TIMEFORMAT=\"BENCHMARKTIME=%3lU\" && time "++cmd
  (hin,hout,herr) <- execCmd timecmd
  outcnt <- hGetContents hout
  errcnt <- hGetContents herr
  hClose hin
  putStr outcnt
  return (extractTimeInOutput errcnt)

-- extract benchmark time from timing output:
extractTimeInOutput =
  tail . snd . break (=='=') . head . filter ("BENCHMARKTIME" `isPrefixOf`)
       . lines

-- Run a benchmark and the timings
runBenchmark num (name,preparecmd,benchcmd,cleancmd) = do
  let line = take 70 (repeat '=')
  putStr (unlines [line, "Running benchmark: "++name, line])
  system preparecmd
  times <- mapIO (\_ -> benchmarkCommand benchcmd) [1..num]
  system cleancmd
  return (name++": "++ concat (intersperse " | " times))

-- Command to compile a module and execute main with idcompiler:
idcCompile options mod = "../compilecurry " ++ options ++ " " ++ mod

-- Command to compile a module and execute main with GHC:
--mccCompile mod = "/home/mcc/bin/cyc -e\"print main\" " ++ mod ++".curry"
mccCompile mod = "/home/mcc/bin/cyc -e\"main\" " ++ mod ++".curry"

-- Command to compile a module and execute main with GHC:
ghcCompile mod = "ghc --make -fforce-recomp " ++ mod

-- Command to compile a module and execute main with GHC (optimized):
ghcCompileO mod = "ghc -O2 --make -fforce-recomp " ++ mod

-- Command to compile a module and print main in PAKCS:
pakcsCompile mod =
  "/home/pakcs/pakcs/bin/pakcs -m \"print main\" -s  " ++ mod

-- Command to compile a Prolog program and run main in SICStus-Prolog:
sicstusCompile mod =
  "echo \"compile("++mod++"), save_program('"++mod++".state',main).\" | /home/sicstus/sicstus4/bin/sicstus && chmod +x "++mod++".state"

-- Command to compile a Prolog program and run main in SWI-Prolog:
swiCompile mod =
  "echo \"compile("++mod++"), qsave_program('"++mod++".state',[toplevel(main)]).\" | /home/swiprolog/bin/swipl"

idcBenchmark   mod = (mod++"@IDC   ",idcCompile "" mod,
                      "./Main","rm Main* Curry_*")
idcBenchmarkO  mod = (mod++"@IDC+  ",idcCompile "-o" mod,
                      "./Main","rm Main* Curry_*")
idcBenchmarkD  mod = (mod++"@IDC_D ",idcCompile "-d" mod,
                      "./Main","rm Main* Curry_*")
idcBenchmarkOD mod = (mod++"@IDC+_D",idcCompile "-o -d" mod,
                      "./Main","rm Main* Curry_*")
idcBenchmarkDFS mod = (mod++"@IDC_DFS ",idcCompile "--prdfs" mod,
                       "./Main", "rm Main* Curry_*")
idcBenchmarkODFS mod = (mod++"@IDC+_DFS",idcCompile "-o --prdfs" mod,
                        "./Main", "rm Main* Curry_*")
idcBenchmarkODFSIORef mod = (mod++"@IDC+_DFS_IORef",
                             idcCompile "-o --prdfs --idsupply ioref" mod,
                             "./Main", "rm Main* Curry_*")
pakcsBenchmark mod = (mod++"@PAKCS ",pakcsCompile mod,"./"++mod++".state",
                      "rm "++mod++".state")
mccBenchmark   mod = (mod++"@MCC   ",mccCompile mod,
                      "./a.out +RTS -h512m -RTS",
                      "rm a.out "++mod++".icurry")
ghcBenchmark   mod = (mod++"@GHC   ",ghcCompile mod,"./"++mod,
                      "rm "++mod++" "++mod++".hi "++mod++".o")
ghcOBenchmark  mod = (mod++"@GHC+  ",ghcCompileO mod,"./"++mod,
                      "rm "++mod++" "++mod++".hi "++mod++".o")
sicsBenchmark  mod = (mod++"@SICS  ", sicstusCompile mod,
                      "./"++mod++".state", "rm "++mod++".state")
swiBenchmark   mod = (mod++"@SWI   ", swiCompile mod,
                      "./"++mod++".state", "rm "++mod++".state")

----------------------------------------------------------------------
-- The various kinds of benchmarks:

-- Benchmarking functional programs with idc/pakcs/mcc/ghc/prolog
benchFPpl prog =
 [idcBenchmarkD  prog
 ,idcBenchmarkOD prog
 ,pakcsBenchmark prog
 ,mccBenchmark   prog
 ,ghcBenchmark   prog
 ,ghcOBenchmark  prog
 ,sicsBenchmark  (map toLower prog)
 ,swiBenchmark   (map toLower prog)
 ]

-- Benchmarking higher-order functional programs with idc/pakcs/mcc/ghc
benchHOFP prog =
 [idcBenchmark   prog
 ,idcBenchmarkO  prog
 ,idcBenchmarkD  prog
 ,idcBenchmarkOD prog
 ,pakcsBenchmark prog
 ,mccBenchmark   prog
 ,ghcBenchmark   prog
 ,ghcOBenchmark  prog
 ]

-- Benchmarking functional logic programs with idc/pakcs/mcc in DFS mode
benchFLPDFS prog =
 [idcBenchmarkDFS  prog
 ,idcBenchmarkODFS prog
 ,idcBenchmarkODFSIORef prog
 ,pakcsBenchmark prog
 ,mccBenchmark   prog
 ]


allBenchmarks = concat
  [ benchFPpl "Reverse"
  , benchFPpl "ReversePrimList"
  , benchFPpl "Tak"
  , benchFPpl "TakPeano"
  , benchHOFP  "ReverseHO"
  , benchHOFP "PrimReverse"
  , benchHOFP "PrimesPeano"
  , benchHOFP "Primes"
  , benchHOFP "PrimPrimes"
  , benchHOFP "Queens"
  , benchHOFP "PrimQueens"
  , benchFLPDFS "PermSort"
  , benchFLPDFS "PermSortPeano"
  ]

-- Run all benchmarks and show results
run num benchmarks = do
  args <- getArgs
  results <- mapIO (runBenchmark num) benchmarks
  ltime <- getLocalTime
  info <- evalCmd "uname -a"
  mach <- evalCmd "uname -n"
  let res = unlines $ ("Benchmarks at " ++ info) : results
  putStrLn res
  unless (null args) $ writeFile (outputFile (head args) (init mach) ltime) res
    where
      init :: [a] -> [a]
      init []  = []
      init [_] = []
      init (x:y:zs) = x : init (y : zs)

outputFile :: String -> String -> CalendarTime -> String
outputFile name mach (CalendarTime ye mo da ho mi se _) = "./results/" ++
  name ++ '@' : mach ++ (concat $ intersperse "_" $  (map show [ye, mo, da, ho, mi, se])) ++ ".bench"

--main = run 3 allBenchmarks
main = run 1 allBenchmarks
--main = run 1 (benchFLPDFS "PermSort")
--main = run 1 (benchFLPDFS "PermSort" ++ benchFLPDFS "PermSortPeano")
--main = run 1 (benchFPpl "Tak")
