import List(isPrefixOf,intersperse)
import IO
import IOExts
import System

-- Execute shell command and return time of its execution:
benchmarkCommand cmd = do
  let timecmd = "export TIMEFORMAT=\"BENCHMARKTIME=%3lU\" && time "++cmd
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
runBenchmark num name preparecmd benchcmd cleancmd = do
  system preparecmd
  times <- mapIO (\_ -> benchmarkCommand benchcmd) [1..num]
  system cleancmd
  return (name++": "++ concat (intersperse " | " times))

-- Command to compile a module and execute main with idcompiler:
idcCompile mod = "../compilecurry " ++ mod

-- Command to compile a module and execute main with idcompiler (optimized):
idcCompileO mod = "../compilecurry -o " ++ mod

-- Command to compile a module and execute main with GHC:
mccCompile mod = "/home/mcc/bin/cyc -e\"print main\" " ++ mod ++".curry"

-- Command to compile a module and execute main with GHC:
ghcCompile mod = "ghc --make -fforce-recomp " ++ mod

-- Command to compile a module and execute main with GHC (optimized):
ghcCompileO mod = "ghc -O2 --make -fforce-recomp " ++ mod

-- Command to compile a module and print main in PAKCS:
pakcsCmd mod = "/home/pakcs/pakcs/bin/pakcs -m \"print main\" -s  " ++ mod

-- Command to compile a Prolog program and run main in SICStus-Prolog:
sicstusCompile mod = 
  "echo \"compile("++mod++"), save_program('"++mod++".state',main).\" | /home/sicstus/sicstus4/bin/sicstus && chmod +x "++mod++".state"

-- Command to run a saved state compiled with SICStus-Prolog:
sicstusExec state =
  "(export PATH=/home/sicstus/sicstus4/bin:$PATH && "++state++" )"

-- Command to compile a Prolog program and run main in SWI-Prolog:
swiCompile mod = 
  "echo \"compile("++mod++"), qsave_program('"++mod++".state',[toplevel(main)]).\" | /home/swiprolog/bin/swipl"

-- Command to run a saved state compiled with SICStus-Prolog:
swiExec state =
  "(export SWIPL=/home/swiprolog/bin/swipl && "++state++" )"

idcBenchmark   mod = (mod++"@IDC  ",idcCompile mod,"./Main","rm Main* Curry_*")
idcOBenchmark  mod = (mod++"@IDC+ ",idcCompileO mod,"./Main","rm Main* Curry_*")
pakcsBenchmark mod = (mod++"@PAKCS",pakcsCmd mod,"./"++mod++".state",
                      "rm "++mod++".state")
mccBenchmark   mod = (mod++"@MCC  ",mccCompile mod,"./a.out",
                      "rm a.out "++mod++".icurry")
ghcBenchmark   mod = (mod++"@GHC  ",ghcCompile mod,"./"++mod,
                      "rm "++mod++" "++mod++".hi "++mod++".o")
ghcOBenchmark  mod = (mod++"@GHC+ ",ghcCompileO mod,"./"++mod,
                      "rm "++mod++" "++mod++".hi "++mod++".o")
sicsBenchmark  mod = (mod++"@SICS ",sicstusCompile mod,
                      sicstusExec ("./"++mod++".state"),"rm "++mod++".state")
swiBenchmark   mod = (mod++"@SWI  ",swiCompile mod,
                      swiExec ("./"++mod++".state"),"rm "++mod++".state")

takBench =
 [pakcsBenchmark "Tak"
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
 [pakcsBenchmark "ReversePrimList"
 ,mccBenchmark   "ReversePrimList"
 ,ghcBenchmark   "ReversePrimList"
 ,ghcOBenchmark  "ReversePrimList"
 ,sicsBenchmark  "reverseprimlist"
 ,swiBenchmark   "reverseprimlist"
 ]

allBenchmarks = reverseBench++reversePrimListBench++takBench++takPeanoBench

-- Run all benchmarks and show results
run num benchmarks = do
  results <- mapIO (\ (n,p,b,c) -> runBenchmark num n p b c) benchmarks
  putStrLn (unlines results)

main = run 3 allBenchmarks
