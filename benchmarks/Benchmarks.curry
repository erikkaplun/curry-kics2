-- This program defines the execution of all benchmarks and summarizes
-- their results.

import List(isPrefixOf,intersperse)
import IO
import IOExts
import System
import Time
import SetFunctions

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
idcCompile mod = "../compilecurry " ++ mod

-- Command to compile a module and execute main with idcompiler (optimized):
idcCompileO mod = "../compilecurry -o " ++ mod

-- Command to compile a module and execute main with idcompiler:
idcCompileD mod = "../compilecurry -d " ++ mod

-- Command to compile a module and execute main with idcompiler (optimized):
idcCompileOD mod = "../compilecurry -o -d " ++ mod

-- Command to compile a module and execute main with GHC:
mccCompile mod = "/home/mcc/bin/cyc -e\"print main\" " ++ mod ++".curry"

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

idcBenchmark   mod = (mod++"@IDC   ",idcCompile mod,"./Main","rm Main* Curry_*")
idcOBenchmark  mod = (mod++"@IDC+  ",idcCompileO mod,"./Main","rm Main* Curry_*")
idcBenchmarkD  mod = (mod++"@IDC_D ",idcCompileD mod,"./Main","rm Main* Curry_*")
idcOBenchmarkD mod = (mod++"@IDC+_D",idcCompileOD mod,"./Main","rm Main* Curry_*")
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

reverseBench =
 [idcBenchmarkD  "Reverse"
 ,idcOBenchmarkD "Reverse"
 ,pakcsBenchmark "Reverse"
 ,mccBenchmark   "Reverse"
 ,ghcBenchmark   "Reverse"
 ,ghcOBenchmark  "Reverse"
 ,sicsBenchmark  "reverse"
 ,swiBenchmark   "reverse"
 ]

reversePrimListBench =
 [idcBenchmarkD  "ReversePrimList"
 ,idcOBenchmarkD "ReversePrimList"
 ,pakcsBenchmark "ReversePrimList"
 ,mccBenchmark   "ReversePrimList"
 ,ghcBenchmark   "ReversePrimList"
 ,ghcOBenchmark  "ReversePrimList"
 ,sicsBenchmark  "reverseprimlist"
 ,swiBenchmark   "reverseprimlist"
 ]

takBench =
 [idcBenchmarkD  "Tak"
 ,idcOBenchmarkD "Tak"
 ,pakcsBenchmark "Tak"
 ,mccBenchmark   "Tak"
 ,ghcBenchmark   "Tak"
 ,ghcOBenchmark  "Tak"
 ,sicsBenchmark  "tak"
 ,swiBenchmark   "tak"
 ]

takPeanoBench =
 [idcBenchmarkD  "TakPeano"
 ,idcOBenchmarkD "TakPeano"
 ,pakcsBenchmark "TakPeano"
 ,mccBenchmark   "TakPeano"
 ,ghcBenchmark   "TakPeano"
 ,ghcOBenchmark  "TakPeano"
 ,sicsBenchmark  "takpeano"
 ,swiBenchmark   "takpeano"
 ]

reverseHOBench =
 [idcBenchmark   "ReverseHO"
 ,idcOBenchmark  "ReverseHO"
 ,idcBenchmarkD  "ReverseHO"
 ,idcOBenchmarkD "ReverseHO"
 ,pakcsBenchmark "ReverseHO"
 ,mccBenchmark   "ReverseHO"
 ,ghcBenchmark   "ReverseHO"
 ,ghcOBenchmark  "ReverseHO"
 ]

primReverseBench =
 [idcBenchmark   "PrimReverse"
 ,idcOBenchmark  "PrimReverse"
 ,idcBenchmarkD  "PrimReverse"
 ,idcOBenchmarkD "PrimReverse"
 ,pakcsBenchmark "PrimReverse"
 ,mccBenchmark   "PrimReverse"
 ,ghcBenchmark   "PrimReverse"
 ,ghcOBenchmark  "PrimReverse"
 ]

primesPeanoBench =
 [idcBenchmark   "PrimesPeano"
 ,idcOBenchmark  "PrimesPeano"
 ,idcBenchmarkD  "PrimesPeano"
 ,idcOBenchmarkD "PrimesPeano"
 ,pakcsBenchmark "PrimesPeano"
 ,mccBenchmark   "PrimesPeano"
 ,ghcBenchmark   "PrimesPeano"
 ,ghcOBenchmark  "PrimesPeano"
 ]

primesBench =
 [idcBenchmark   "Primes"
 ,idcOBenchmark  "Primes"
 ,idcBenchmarkD  "Primes"
 ,idcOBenchmarkD "Primes"
 ,pakcsBenchmark "Primes"
 ,mccBenchmark   "Primes"
 ,ghcBenchmark   "Primes"
 ,ghcOBenchmark  "Primes"
 ]

primPrimesBench =
 [idcBenchmark   "PrimPrimes"
 ,idcOBenchmark  "PrimPrimes"
 ,idcBenchmarkD  "PrimPrimes"
 ,idcOBenchmarkD "PrimPrimes"
 ,pakcsBenchmark "PrimPrimes"
 ,mccBenchmark   "PrimPrimes"
 ,ghcBenchmark   "PrimPrimes"
 ,ghcOBenchmark  "PrimPrimes"
 ]

queensBench =
 [idcBenchmark   "Queens"
 ,idcOBenchmark  "Queens"
 ,idcBenchmarkD  "Queens"
 ,idcOBenchmarkD "Queens"
 ,pakcsBenchmark "Queens"
 ,mccBenchmark   "Queens"
 ,ghcBenchmark   "Queens"
 ,ghcOBenchmark  "Queens"
 ]


allBenchmarks = concat
  [ reverseBench
  , reversePrimListBench
  , takBench
  , takPeanoBench
  , reverseHOBench
  , primReverseBench
  , primesPeanoBench
  , primesBench
  , primPrimesBench
  , queensBench
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

main = run 3 allBenchmarks
-- main = run 1 allBenchmarks
-- main = run 1 queensBench
--main = run 1 primReverseBench
-- main = run 3 (takBench ++ queensBench)
