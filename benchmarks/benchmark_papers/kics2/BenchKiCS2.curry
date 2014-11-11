{-# OPTIONS_CYMAKE -F --pgmF=currypp #-}

-----------------------------------------------------------------------
--- Benchmark different Curry systems with various example programs.
---
--- This program is used in the *executable benchmark paper* `bench_kics2.tex`.
-----------------------------------------------------------------------

import Benchmarks
import BenchmarkGoodies
import Directory
import Format
import List(transpose)
import System

-- Time limit for individual benchmarks:
timeLimit = 150.0 -- seconds

-- Number of runs for each benchmark:
numberOfRuns = 1

-- The KiCS2 systems to be benchmarked:
kics2Systems = [KiCS2 "0.2.3", KiCS2 "0.2.4",
                KiCS2 "0.3.0", KiCS2 "0.3.1", KiCS2 ""]

-- Directory containing the benchmark programs:
benchProgDir = "../../suite"

-----------------------------------------------------------------------
-- Basic operations to compile a Curry program with different Curry systems
-- (e.g., pakcs, kics2, mcc) and execute some benchmarks.

-- The Curry systems (compilers) used in the benchmarks:
data CurrySystem = PAKCS | MCC | KiCS2 String -- version

-- Show the name of the Curry system:
showSystemName PAKCS           = "PAKCS"
showSystemName MCC             = "MCC"
showSystemName (KiCS2 version) = "KiCS2 " ++ version

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
   compilecmd = case cs of
     MCC -> unwords
              [currybin,"-o",prog,
               if null mainexp then "-e\"main\"" else "-e\""++mainexp++"\"",
               prog++".curry"]
     _   -> unwords [currybin,defopts,opts,":load",prog,":save",mainexp,":quit"]

   currybin = binOfSystem cs

   -- default options for PAKCS and KiCS2:
   defopts = ":set -first :set -time"

-- The command to execute a compiled Curry program:
-- (for MCC: increase heap size and set non-interative option)
runCurryCmd :: CurrySystem -> String -> String
runCurryCmd cs prog =
  "./"++prog ++ (if cs==MCC then " +RTS -h512m -RTS -n" else "")

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
-- on a given Curry system and return the output (and error if exit status
-- is non-zero).
benchExpResult :: CurrySystem -> (String,String,String)
               -> Benchmark String
benchExpResult currysystem (progname,options,mainexp) =
  (mapBench checkError (benchCommandOutput ("./"++progname)))
    `withPrepare` compileCurry currysystem options progname mainexp
    `withCleanup` cleanCurry   currysystem progname
 where
  checkError (status,output,error) =
    if status==0 then output
                 else "***ERROR*** Exit status: "++show status++"\n"++
                      output++error

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
  showOpts o = if null o then ""
               else " ("++ unwords (filter (/=":set") (words o)) ++")"

-- Shows a floating point number.
showF x = ``format "%.2f",x''

-- Executes a benchmark with parallel strategies (with a KiCS2 system)
-- and return the required elapsed time.
benchParProg :: CurrySystem -> String -> Benchmark [(String,String)]
benchParProg currysystem progname =
  mapBench (map (\ (p,mbf) -> (p, maybe "-" showF mbf)))
   ((runOn execProgBench [""," +RTS -N2 -RTS"," +RTS -N4 -RTS"," +RTS -N -RTS"])
      `withPrepare` compileCurry currysystem options progname ""
      `withCleanup` cleanCurry   currysystem progname)
 where
  options = ":set parallel"

  execProgBench rtsopt = numberOfRuns *>- 
    mapBench (maybe Nothing (Just . elapsedTime))
             (benchCommandWithLimit
                (runCurryCmd currysystem progname ++ rtsopt)
                timeLimit)

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

benchNRevAndPlot tlimit inputs outfile = do
  pdata   <- execBench (nrevProgBenchsWithLimit PAKCS      "" tlimit inputs)
  kdata   <- execBench (nrevProgBenchsWithLimit (KiCS2 "") "" tlimit inputs)
  kwodata <- execBench (nrevProgBenchsWithLimit (KiCS2 "") ":set -opt"
                                                tlimit inputs)
  ko3data <- execBench (nrevProgBenchsWithLimit (KiCS2 "") ":set ghc -optc-O3"
                                                tlimit inputs)
  mccdata <- execBench (nrevProgBenchsWithLimit MCC "" tlimit inputs)
  plotResults outfile
              [Lines, Title "nrev run times",
               XLabel "list length", YLabel "run time (seconds)"]
              [("pakcs",pdata)
              ,("kics2",kdata)
              ,("kics2 no opt",kwodata)
              ,("kics2 -optc-O3",ko3data)
              ,("mcc",mccdata)
              ]
  return ("\\includegraphics[width=\\linewidth]{"++outfile++"}")

-----------------------------------------------------------------------
-- The benchmark programs to test different KiCS2 versions:

-- Deterministic benchmark programs:
detBench _ = map (\p -> (p,"",""))
  ["ReverseUser","Reverse"
  ,"Tak","TakPeano"
  ,"ReverseHO","ReverseBuiltin"
  ,"Primes","PrimesPeano","PrimesBuiltin"
  ,"Queens","QueensUser"]

-- Non-deterministic benchmark programs testing the computation of
-- all values via DFS:
nondetBenchDFS currysystem =
  map (\p -> (p,dfsOption currysystem,""))
      ["PermSort","PermSortPeano","Half","Last","RegExp"]

-- Benchmark programs with functional patterns
-- (all values via DFS are computed):
funpatBenchDFS currysystem =
  map (\p -> (p,dfsOption currysystem,""))
      ["LastFunPats","ExpVarFunPats","ExpSimpFunPats","PaliFunPats"]

-- Set the DFS option for KiCS2:
dfsOption cs = if cs `elem` [PAKCS,MCC] then "" else ":set dfs"

-- Non-deterministic benchmark programs:
nondetBench _ =
  "PermSort"       `withSearch` ["dfs","bfs","ids"] ++
  "PermSortPeano"  `withSearch` ["dfs","bfs","ids"] ++
  "Half"           `withSearch` ["dfs","bfs","ids"] ++
  map (\m -> ("ShareNonDet",":set dfs",m)) ["goal1","goal2","goal3"] ++
  "Last"           `withSearch` ["dfs","bfs","ids"] ++
  "RegExp"         `withSearch` ["dfs","bfs","ids"] ++
  "LastFunPats"    `withSearch` ["dfs","bfs","ids"] ++
  "ExpVarFunPats"  `withSearch` ["dfs","bfs","ids"] ++
  "ExpSimpFunPats" `withSearch` ["dfs","bfs","ids"] ++
  "PaliFunPats"    `withSearch` ["dfs","bfs","ids"]

-- transform program into programs testing all given search strategies:
withSearch prog = map (\s->(prog,":set "++s,""))

-- Non-deterministic benchmark programs where only the first solution
-- is computed (due to infinite search tree):
nondetBenchFirst _ =
  "NDNums" `withSearchFirst` ["bfs","ids"]

-- transform program into programs testing all given search strategies to
-- compute a first solution/value:
withSearchFirst prog = map (\s->(prog,":set "++s++" :set +first",""))

-- Parallel search benchmark programs:
parSearchBench = ["PermSort","PermSortPeano"]

-- Encapsulated search benchmarks:
encapsBench _ =
  [("PermSortSearchTree",":set dfs","mainsort")
  ,("PermSortSearchTree",":set dfs","maintree")]

-- Benchmark a list of Curry systems with a list of benchmark programs
-- and show the result as a table.
mainBench :: [CurrySystem] -> (CurrySystem -> [(String,String,String)])
          -> IO String
mainBench systems benchprogs = do
  curdir <- getCurrentDirectory
  setCurrentDirectory benchProgDir
  bdata <- mapIO (\s -> execBench (benchPrograms s (benchprogs s))) systems
  let tabledata = map (\ ((prg,p):ds) -> prg:p:map snd ds)
                      (transpose bdata)
  setCurrentDirectory curdir
  return (benchResultsAsTable ("Program" : map showSystemLabel systems)
                              tabledata)

-- Benchmark the parallel search strategy of a Curry system (KiCS2)
-- with a list of benchmark programs and show the result as a table.
mainParBench :: CurrySystem -> [String] -> IO String
mainParBench system benchprogs = do
  curdir <- getCurrentDirectory
  setCurrentDirectory benchProgDir
  bdata <- mapIO (\prog -> execBench (benchParProg system prog)) benchprogs
  let tabledata = map (\ ((prg,p):ds) -> prg:p:map snd ds)
                      (transpose bdata)
  setCurrentDirectory curdir
  return (benchResultsAsTable ("RTS Parameters" : benchprogs)
                              tabledata)

-----------------------------------------------------------------------
-- Benchmark to show some distribution information of a Curry system.
distInfo :: CurrySystem -> IO String
distInfo currysystem = do
  execBench (benchExpResult currysystem ("DistInfo","","runtimeSystem"))

-- Benchmark to show the distribution information of a
-- list of Curry systems as a LaTeX table.
distInfosAsTable :: [CurrySystem] -> IO String
distInfosAsTable currysystems = do
  infos <- mapIO (\cs -> distInfo cs >>= \i -> return [showSystemName cs,i])
                 currysystems
  return (benchResultsAsTable ["System","Info"] infos)

-----------------------------------------------------------------------
