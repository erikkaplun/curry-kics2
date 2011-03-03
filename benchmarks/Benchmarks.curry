-- This program defines the execution of all benchmarks and summarizes
-- their results.

import List(isPrefixOf,intersperse)
import IO
import IOExts
import System
import Time

-- Execute shell command and return time of its execution:
benchmarkCommand cmd = do
  -- for Debian-PCs:
  --let timecmd = "export TIMEFORMAT=\"BENCHMARKTIME=%3lU\" && time "++cmd
  -- for Ubuntu:
  let timecmd = "time --format=\"BENCHMARKTIME=%U\" "++cmd
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
  -- Current hack: remove Prelude for compilation since PAKCS has a different
  "cleancurry && mv Prelude.curry Prelude.curry.ID && "++
  "/home/pakcs/pakcs/bin/pakcs -m \"print main\" -s  " ++ mod ++ " && " ++
  "mv Prelude.curry.ID Prelude.curry"

-- Command to compile a Prolog program and run main in SICStus-Prolog:
sicstusCompile mod =
  "echo \"compile("++mod++"), save_program('"++mod++".state',main).\" | /home/sicstus/sicstus4/bin/sicstus && chmod +x "++mod++".state"

-- Command to compile a Prolog program and run main in SWI-Prolog:
swiCompile mod =
  "echo \"compile("++mod++"), qsave_program('"++mod++".state',[toplevel(main)]).\" | /home/swiprolog/bin/swipl"

idcBenchmark   mod = (mod++"@IDC  ",idcCompile mod,"./Main","rm Main* Curry_*")
idcOBenchmark  mod = (mod++"@IDC+ ",idcCompileO mod,"./Main","rm Main* Curry_*")
idcBenchmarkD  mod = (mod++"@IDC  ",idcCompileD mod,"./Main","rm Main* Curry_*")
idcOBenchmarkD mod = (mod++"@IDC+ ",idcCompileOD mod,"./Main","rm Main* Curry_*")
pakcsBenchmark mod = (mod++"@PAKCS",pakcsCompile mod,"./"++mod++".state",
                      "rm "++mod++".state")
mccBenchmark   mod = (mod++"@MCC  ",mccCompile mod,
                      "./a.out +RTS -h512m -RTS",
                      "rm a.out "++mod++".icurry")
ghcBenchmark   mod = (mod++"@GHC  ",ghcCompile mod,"./"++mod,
                      "rm "++mod++" "++mod++".hi "++mod++".o")
ghcOBenchmark  mod = (mod++"@GHC+ ",ghcCompileO mod,"./"++mod,
                      "rm "++mod++" "++mod++".hi "++mod++".o")
sicsBenchmark  mod = (mod++"@SICS ", sicstusCompile mod,
                      "./"++mod++".state", "rm "++mod++".state")
swiBenchmark   mod = (mod++"@SWI  ", swiCompile mod,
                      "./"++mod++".state", "rm "++mod++".state")

reverseBench =
 [idcBenchmark   "Reverse"
 ,idcOBenchmark  "Reverse"
 ,pakcsBenchmark "Reverse"
 ,mccBenchmark   "Reverse"
 ,ghcBenchmark   "Reverse"
 ,ghcOBenchmark  "Reverse"
 ,sicsBenchmark  "reverse"
 ,swiBenchmark   "reverse"
 ]

reversePrimListBench =
 [idcBenchmark   "ReversePrimList"
 ,idcOBenchmark  "ReversePrimList"
 ,pakcsBenchmark "ReversePrimList"
 ,mccBenchmark   "ReversePrimList"
 ,ghcBenchmark   "ReversePrimList"
 ,ghcOBenchmark  "ReversePrimList"
 ,sicsBenchmark  "reverseprimlist"
 ,swiBenchmark   "reverseprimlist"
 ]

takBench =
 [idcBenchmark   "Tak"
 ,idcOBenchmark  "Tak"
 ,pakcsBenchmark "Tak"
 ,mccBenchmark   "Tak"
 ,ghcBenchmark   "Tak"
 ,ghcOBenchmark  "Tak"
 ,sicsBenchmark  "tak"
 ,swiBenchmark   "tak"
 ]

takPeanoBench =
 [idcBenchmark   "TakPeano"
 ,idcOBenchmark  "TakPeano"
 ,pakcsBenchmark "TakPeano"
 ,mccBenchmark   "TakPeano"
 ,ghcBenchmark   "TakPeano"
 ,ghcOBenchmark  "TakPeano"
 ,sicsBenchmark  "takpeano"
 ,swiBenchmark   "takpeano"
 ]

reverseHOBench =
 [idcBenchmarkD  "ReverseHO"
 ,idcOBenchmarkD "ReverseHO"
 ,pakcsBenchmark "ReverseHO"
 ,mccBenchmark   "ReverseHO"
 ,ghcBenchmark   "ReverseHO"
 ,ghcOBenchmark  "ReverseHO"
 ]

primesPeanoBench =
 [idcBenchmarkD  "PrimesPeano"
 ,idcOBenchmarkD "PrimesPeano"
 ,pakcsBenchmark "PrimesPeano"
 ,mccBenchmark   "PrimesPeano"
 ,ghcBenchmark   "PrimesPeano"
 ,ghcOBenchmark  "PrimesPeano"
 ]

allBenchmarks = reverseBench++reversePrimListBench++takBench++takPeanoBench

-- Run all benchmarks and show results
run num benchmarks = do
  results <- mapIO (runBenchmark num) benchmarks
  ltime <- getLocalTime
  putStrLn (unlines (("Benchmarks at "++calendarTimeToString ltime) : results))

--main = run 3 allBenchmarks
--main = run 1 allBenchmarks
main = run 1 primesPeanoBench
