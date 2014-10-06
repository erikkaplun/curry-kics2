{-# OPTIONS_CYMAKE -F --pgmF=../../../bin/currypp #-}

-----------------------------------------------------------------------
--- A benchmark to test different Curry system with various benchmarks.
-----------------------------------------------------------------------

import Benchmarks
import BenchmarkGoodies
import Directory
import Format
import List(transpose)
import System

-- Time limit for individual benchmarks:
timeLimit = 100.0 -- seconds

-- Number of runs for each benchmark:
numberOfRuns = 1

-- The KiCS2 systems to be benchmarked:
kics2Systems = [KiCS2 "0.2.4", KiCS2 "0.3.0", KiCS2 "0.3.1", KiCS2 ""]

-- Directory containing the benchmark programs:
benchProgDir = "../../suite"

-----------------------------------------------------------------------
-- Basic operations to compile a Curry program with different Curry systems
-- (e.g., pakcs, kics2, mcc) and execute some benchmarks.

-- The Curry systems (compilers) used in the benchmarks:
data CurrySystem = PAKCS | MCC | KiCS2 String -- version

-- Show the name of the Curry system as a LaTeX string (used as a label
-- of a column of a table):
showSystemLabel PAKCS           = "PAKCS"
showSystemLabel MCC             = "MCC"
showSystemLabel (KiCS2 version) =
  if null version then "KiCS2"
                  else "\\parbox{1cm}{KiCS2 " ++ version ++ "}"

-- The binary of the Curry system:
binOfSystem PAKCS           = "/opt/pakcs/bin/pakcs"
binOfSystem MCC             = "/opt/mcc/bin/cyc"
binOfSystem (KiCS2 version) = "/opt/kics2/kics2"++
                              (if null version then "" else '-':version)++
                              "/bin/kics2"

-- Compile a program with a given Curry system:
compileCurry :: CurrySystem -> String -> String-> String -> IO ()
compileCurry cs opts prog mainexp = system compilecmd >> done
 where
   currybin = binOfSystem cs

   compilecmd = case cs of
     MCC     -> unwords [currybin,"-o",prog,prog++".curry"]
     PAKCS   -> unwords [currybin,":load",prog,":save",mainexp,":quit"]
     KiCS2 _ -> unwords [currybin,opts,":load",prog,":save",mainexp,":quit"]

-- The command to execute a compiled Curry program:
runCurryCmd :: CurrySystem -> String -> String
runCurryCmd cs prog =
  "./"++prog ++ (if cs==MCC then " +RTS -h512m -RTS" else "")

-- Remove generated files:
cleanCurry cs prog = case cs of
  MCC   -> system ("/bin/rm -f "++prog++".icurry "++prog) >> done
  PAKCS -> do system ("/opt/pakcs/bin/cleancurry "++prog)
              system ("/bin/rm -f "++prog)
              done
  KiCS2 _ -> do system ("/opt/kics2/bin/cleancurry "++prog)
                system ("/bin/rm -f "++prog)
                done

-- Executes a benchmark (program name, compile options, main expression or "")
-- on a given Curry system and return the required cpu time.
benchProgram :: CurrySystem -> (String,String,String)
             -> Benchmark (Maybe Float)
benchProgram currysystem (progname,options,mainexp) =
  (numberOfRuns *>- execProgBench progname)
    `withPrepare` compileCurry currysystem options progname mainexp
    `withCleanup` cleanCurry   currysystem progname
 where
  execProgBench prog =
    mapBench (maybe Nothing (Just . cpuTime))
             (benchCommandWithLimit (runCurryCmd currysystem prog) timeLimit)

-- Executes a list of benchmarks on a given Curry system and
-- return the list of pairs consisting of a program name and the result
-- (required cpu time or "OOT" if time limit is reached).
benchPrograms :: CurrySystem -> [(String,String,String)]
              -> Benchmark [(String, String)]
benchPrograms currysystem programs =
  mapBench (map (\ ((p,o,m),f) -> (p ++ (if null m then "" else '.':m)
                                     ++ showOpts o,
                                   maybe "--" showF f)))
           (runOn (benchProgram currysystem) programs)
 where
  showF x = ``format "%.2f",x''

  showOpts o = if null o then ""
               else " ("++ unwords (filter (/=":set") (words o)) ++")"

-----------------------------------------------------------------------
-- The naive reverse benchmark executed for different list lengths
-- until a given time limit is reached:

nrevProgBenchsWithLimit :: CurrySystem -> String -> Float -> [Int]
                        -> Benchmark [(Int,Float)]
nrevProgBenchsWithLimit currysystem options tlimit listlens =
  runUntilNothingOn (\n -> numberOfRuns *>- nrevBenchWithLimit n) listlens
    `withPrepare` compileCurry currysystem options "NRev" ""
    `withCleanup` cleanCurry   currysystem "NRev"
 where
  nrevBenchWithLimit n =
    mapBench (maybe Nothing (Just . cpuTime))
             (benchCommandWithLimit ("./NRev "++show n) tlimit)

benchNRevAndPlot tlimit inputs jpgfile = do
  pdata   <- execBench (nrevProgBenchsWithLimit PAKCS      "" tlimit inputs)
  kdata   <- execBench (nrevProgBenchsWithLimit (KiCS2 "") "" tlimit inputs)
  kwodata <- execBench (nrevProgBenchsWithLimit (KiCS2 "") ":set -opt"
                                                tlimit inputs)
  ko3data <- execBench (nrevProgBenchsWithLimit (KiCS2 "") ":set ghc -optc-O3"
                                                tlimit inputs)
  mccdata <- execBench (nrevProgBenchsWithLimit MCC "" tlimit inputs)
  plotResults jpgfile
              [Lines, Title "nrev run times",
               XLabel "list length", YLabel "run time (seconds)"]
              [("pakcs",pdata)
              ,("kics2",kdata)
              ,("kics2 no opt",kwodata)
              ,("kics2 -optc-O3",ko3data)
              ,("mcc",mccdata)
              ]
  return ("\\includegraphics[width=\\linewidth]{"++jpgfile++".jpg}")

-----------------------------------------------------------------------
-- The benchmark programs to test different KiCS2 versions:

-- Deterministic benchmark programs:
detBench _ = map (\p -> (p,"",""))
  ["ReverseUser","Reverse"
  ,"Tak","TakPeano"
  ,"ReverseHO","ReverseBuiltin"
  ,"Primes","PrimesPeano","PrimesBuiltin"
  ,"Queens","QueensUser"]

-- Non-deterministic benchmark programs testing the computation of the
-- first solution via DFS in PAKCS vs. KiCS2:
nondetBenchDFS currysystem =
  map (\p -> (p,dfsFirstOption,""))
      ["PermSort","PermSortPeano","Half",
       "Last","RegExp",
       "LastFunPats","ExpVarFunPats","ExpSimpFunPats","PaliFunPats"]
 where
  dfsFirstOption =
    if currysystem==PAKCS
    then ":set +first"
    else ":set dfs :set +first" -- KiCS2

-- Non-deterministic benchmark programs:
nondetBench _ =
  "PermSort"       `withSearchFirst` ["dfs","bfs","ids"] ++
  "PermSortPeano"  `withSearchFirst` ["dfs","bfs","ids"] ++
  "Half"           `withSearchFirst` ["dfs","bfs","ids"] ++
  "NDNums"         `withSearchFirst` ["bfs","ids"]       ++
  map (\m -> ("ShareNonDet","",m)) ["goal1","goal2","goal3"] ++
  "Last"           `withSearchFirst` ["dfs","bfs","ids"] ++
  "RegExp"         `withSearchFirst` ["dfs","bfs","ids"] ++
  "LastFunPats"    `withSearchFirst` ["dfs","bfs","ids"] ++
  "ExpVarFunPats"  `withSearchFirst` ["dfs","bfs","ids"] ++
  "ExpSimpFunPats" `withSearchFirst` ["dfs","bfs","ids"] ++
  "PaliFunPats"    `withSearchFirst` ["dfs","bfs","ids"]

-- transform program into programs testing all search strategies to
-- compute first solution:
withSearchFirst prog = map (\s->(prog,":set "++s++" :set +first",""))

-- Encapsulated search benchmarks:
encapsBench _ =
  [("PermSortSearchTree",":set dfs","mainsort")
  ,("PermSortSearchTree",":set dfs","maintree")]

                    
mainBench systems benchprogs = do
  curdir <- getCurrentDirectory
  setCurrentDirectory benchProgDir
  bdata <- mapIO (\s -> execBench (benchPrograms s (benchprogs s))) systems
  let tabledata = map (\ ((prg,p):ds) -> prg:p:map snd ds)
                      (transpose bdata)
  setCurrentDirectory curdir
  return (benchResultsAsTable ("Program" : map showSystemLabel systems)
                              tabledata)


-----------------------------------------------------------------------
