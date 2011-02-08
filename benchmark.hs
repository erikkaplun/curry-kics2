{-# LANGUAGE NamedFieldPuns #-}

import System.Environment
import qualified System.Cmd as S
import Control.Monad
import Debug.Trace
import Prelude hiding (read)
import qualified Prelude

import SearchMode

curryLibPath = "./"

read s = trace ("reading: " ++ s)
  Prelude.read s


allBench = [
           benchmarkGhc
           --,benchmarkGood
           --,ghcOpt benchmarkGood
           ,hoOpt benchmarkGood True
           ,ghcOpt (hoOpt benchmarkGood True)
           ,benchmarkMonad
           ,ghcOpt benchmarkMonad
           ,benchmarkCyc
           ,benchmarkPakcs
           ]

dfsBench = [
  benchmarkGoodHO
  ,ghcOpt benchmarkGoodHO
  ,benchmarkMonad
  ,ghcOpt benchmarkMonad
  ,benchmarkCyc
  ,benchmarkPakcs
  ]

bfsBench = [
  benchmarkGoodHO
  ,ghcOpt benchmarkGoodHO
  ,benchmarkMonad
  ,ghcOpt benchmarkMonad
  ,benchmarkCyc
  ]

idfsBench = bfsBench

parBench = [
  benchmarkGoodHO
  ,ghcOpt benchmarkGoodHO
  ,benchmarkMonad
  ,ghcOpt benchmarkMonad
  ]

main = do
  cmds <- getArgs
  rm $ benchFile cmds
  case cmds of
    [p]   -> mapM_ ($defaults 0 p) allBench
    [p,n] -> sequence_
               [ mapM_ ($defaults i p) allBench | i <- [from n .. to n]]
    (p:n:"GHC":s:ops) -> do
      ghcOpt benchmarkGhc (defaults' p n s ops)
    (p:n:"ID":s:ops) -> do
      ghcOpt benchmarkGood (defaults' p n s ops)
    (p:n:"IDAll":s:ops) -> do
      sequence_ [ghcOpt benchmarkGood (defaults' p (show i) s ops) | i <- [from n .. to n]]
    (p:n:"CM":s:ops) -> do
      ghcOpt benchmarkMonad $ defaults' p n s ops
    (p:n:"GHC":ops) -> do
      benchmarkGhc $ defaults'  p n "NoSearch" ops
    (p:n:"PAKCS":s:ops) -> do
      benchmarkPakcs $ defaults' p n s ops
    (p:n:"MCC":s:ops) -> do
      benchmarkCyc $ defaults' p n s ops
    (p:n:s@"NoSearch":ops) -> do
      let opts i = defaults' p (show i) s ops
      sequence_ [ mapM_ ($opts i) allBench | i <- [from n .. to n]]
    (p:n:s@"DFS":ops) -> do
      let opts i = defaults' p (show i) s ops
      sequence_ [ mapM_ ($opts i) dfsBench | i <- [from n .. to n]]
    (p:n:s@"BFS":ops) -> do
      let opts i = defaults' p (show i) s ops
      sequence_ [ mapM_ ($opts i) bfsBench | i <- [from n .. to n]]
    (p:n:s@"IterDFS":ops) -> do
      let opts i = defaults' p (show i) s ops
      sequence_ [ mapM_ ($opts i) idfsBench | i <- [from n .. to n]]
    (p:n:s@"PAR":ops) -> do
      let opts i = defaults' p (show i) s ops
      sequence_ [ mapM_ ($opts i) parBench | i <- [from n .. to n]]
    (p:n:('[':compilers):s:ops) -> do
      let opts i = defaults' p (show i) s ops
      sequence_ [ mapM_ ($opts i) (benchmark ('[':compilers)) | i <- [from n .. to n]]

    _ -> error $ "unknown parameters " ++ show cmds
  system $ "cat " ++ benchFile cmds

benchmark s = comps (read (concatMap addSeps s)) where
  comps [] = []
  comps ("ID":cs)    = ghcOpt benchmarkGoodHO : comps cs
  comps ("PAKCS":cs) = benchmarkPakcs         : comps cs
  comps ("MCC":cs)   = benchmarkCyc           : comps cs

  addSeps ',' = "\",\""
  addSeps '[' = "[\""
  addSeps ']' = "\"]"
  addSeps x   = [x]

from, to :: String -> Int
from s = case reads s :: [(Int,String)] of
  [(i,"")]     -> 0
  [(i,'-':_)]  -> i
  _            -> error $ "reading from of example number " ++ s
to s = case reads s :: [(Int,String)] of
  [(i,"")]     -> i
  [(i,'-':s')] -> read s'
  _            -> error $ "reading to of example number " ++ s



data Opt = Opt{goalNumber::Int,
               withHOOptimization::Bool,
               withGHCOptimization::Bool,
               search::SearchMode,
               idSupplyLib :: String,
               testName::String,
               rtsHeap,rtsStack::Maybe Int}

defaults :: Int -> String -> Opt
defaults n s = Opt{goalNumber=n,
                 testName=s,
                 withHOOptimization=False,
                 withGHCOptimization=False,
                 idSupplyLib="IDSupplyIORef.hs",
                 search=NoSearch,
                 rtsHeap=Nothing,rtsStack=Nothing
                }

defaults' :: String -> String -> String -> [String] -> Opt
defaults' p n s = addOps ((defaults (read n) p) {search=read s})

addOps :: Opt -> [String] -> Opt
addOps o@Opt{search=PAR} = addOps' o{idSupplyLib="IDSupplyInteger.hs"}
addOps o = addOps' o


addOps' o []           = o
addOps' o ["Free"]     = o{idSupplyLib="IDSupplyIORefFree.hs"}
addOps' o ["SetValue"] = o{idSupplyLib="IDSupplyCoveredInteger.hs"}
addOps' o ["Encaps"]   = o{idSupplyLib="IDSupplyInteger.hs"}
addOps' o ["Unify"]    = o{idSupplyLib="IDSupplyUnification.hs"}
addOps' o (('H':s):xs) = addOps' o{rtsHeap=Just (read s)} xs
addOps' o (('K':s):xs) = addOps' o{rtsStack=Just (read s)} xs
addOps' _ xs           = error $ "error while parsing additional options " ++ show xs

hoOpt  f b o = f o{withHOOptimization=b}
ghcOpt f o = f o{withGHCOptimization=True}

debug = True

system call = do
  when debug (print call)
  S.system call
  return ()

benchmarkGoodHO = hoOpt benchmarkGood True

benchmarkGood :: Opt -> IO ()
benchmarkGood o@Opt{withHOOptimization=hoopt,
                  withGHCOptimization=withopt,
                  search,
                  goalNumber=n,
                  testName=s} = do
  rm $ "C_"++s
  let lib = curryLibPath
  -- system $ lib ++ "lift.state " ++ s ++ " > " ++ s ++ "Lifted.fcy"
  system $ "./Compile.state " ++ s ++ ' ':show n
            ++ (if hoopt then " HO " else "")
            ++ (if search/=NoSearch then " " ++ show search ++ " " else "")
{-
  system $ lib ++ "PrettyND.state C_" ++ s ++ "Lifted "
               ++ idSupplyLib o
               ++ " > C_" ++ s ++ ".hs"
-}
  system $ ghcCall withopt ++ "C_"++s
  let opt1 = if withopt then " -O2 " else " -O0 "
      opt2 = if hoopt   then " +HO " else " -HO "
  time search n s ("ID" ++ opt1 ++ opt2)
                  ("./C_"++ s)
                  (heap o 'H')

benchmarkPakcs :: Opt -> IO ()
benchmarkPakcs opt@Opt{goalNumber=n,testName=s} = do
  rm $ s++".state"
  system $ "echo :save main_ | \
           \/home/pakcs/pakcs/bin/pakcs --set path PAKCS -l " ++ s
  let output = "pakcs"
  time (search opt) n s output
       (searchStrategy (" ./" ++ s ++ ".state " ++ show n) opt)
       ""

benchmarkCyc :: Opt -> IO ()
benchmarkCyc opt@Opt{goalNumber=n,testName=s} = do
  rm "a.out"
  system $ "/home/mcc/bin/cyc -i./MCC -emain_ MCC/SetFunctions.curry Benchmark.curry " ++ s ++ ".curry"
  time (search opt) n s "mcc"
       (searchStrategy ("./a.out " ++ show n) opt)
       (heap opt 'h')

searchStrategy :: String -> Opt -> String
searchStrategy s Opt{search=NoSearch} = s
searchStrategy s Opt{search=st} = s ++ " " ++ show st

benchmarkGhc :: Opt -> IO ()
benchmarkGhc Opt{goalNumber=n,testName=s} = do
  rm s
  system $ ghcCall True ++ s
  time NoSearch n s "ghc" ("./" ++ s ++ " " ++ show n) ""

benchmarkMonad :: Opt -> IO ()
benchmarkMonad o@Opt{withGHCOptimization=withopt,
                   search,
                   goalNumber=n,testName=s1} = do
  let dir = "Monad/"
      bin = "/home/bbr/.cabal/bin/curry2monad -mcmgoal" ++ show n ++ " "
      --s  = s1 ++ "_Goal" ++ show n
      s = s1
      ex = "Curry_"++s
      hs = dir ++ ex ++ ".hs"
  rm hs
  let cdmon = "cd " ++ dir  ++ ";"
  curry <- readFile $ s ++ ".curry"
  writeFile (dir ++ s ++ ".curry") curry
  --writeFile (dir ++ s ++ ".curry") (curry ++ "\n\nmain = goal" ++ show n)
  system $ cdmon ++ bin ++ (monad search) ++ s
  cont <- readFile hs
  print (length cont)
  let ls = lines cont
      l  = "\n\nmain = " ++ result search
  writeFile hs (unlines $ take 2 ls ++ drop 3 ls ++ [l])
  system $ ghcCall withopt ++ "-i"++dir
              ++ ":/home/bbr/.cabal/share/curry2monad-0.1/ "
                   ++ dir ++ ex
  time search n s1 (if withopt then "monad opt" else "monad unopt")
                   ("./" ++ dir ++ ex)
                   (heap o 'H')
 where
   cmGoal = "cM_cmgoal" ++ show n
   result NoSearch = "print " ++ cmGoal
   result DFS      = "print $ length $ FuncList.runFuncList " ++ cmGoal
   result BFS      = "print $ Data.FMList.length " ++ cmGoal
   result IterDFS  = "print $ length $ IterativeDepthFirst.runIterDFS " ++ cmGoal
   result PAR      = "print $ length $ Parallel.parSearch " ++ cmGoal

   monad NoSearch = ""
   monad DFS      = " -wFuncList "
   monad BFS      = " -wLevel "
   monad IterDFS  = " -wIterDFS "
   monad PAR      = " -wParallel "


time :: SearchMode -> Int -> String -> String -> String -> String -> IO ()
time s n out compiler cmd heap = do
  let output = benchFileS out s
      headline = "\n\ngoal" ++ show n ++ " " ++ compiler
  appendFile output (headline ++ heap ++ "\n\n")
  let call = "/usr/bin/time -f '%U' --append -o " ++ output ++ " " ++ cmd
      pipe = " >> " ++ output
  system (call ++ heap ++ pipe)
  if (not (null heap))
   then do
     system (call ++ pipe)
   else return ()
  system ("setsid ./memusg " ++ cmd ++ pipe)


rm :: String -> IO ()
rm s = system $ "rm -f "++s

ghcCall withopt =
  let opt = if withopt then " -O2 " else "" in
  "/home/ghc/ghc-6.12.1/bin/ghc --make -fforce-recomp " ++ opt

benchFile [p,n]    = bench $ p ++ show NoSearch
benchFile (p:cmds) = bench $ p ++
  (show $ head [ x :: SearchMode | [(x,"")] <- map reads cmds])

benchFileS out s = bench (out ++ show s)
bench = (++ ".benchmarks")

heap Opt{rtsHeap=Nothing,rtsStack=Nothing,search=BFS}     h = " +RTS -"++h:"300M -RTS "
heap Opt{rtsHeap=Nothing,rtsStack=Nothing,search=IterDFS} h = " +RTS -"++h:"300M -k100M -RTS "
heap Opt{rtsHeap=Nothing,rtsStack=Nothing} _ = ""
heap o@Opt{rtsHeap,rtsStack} h =
  " +RTS " ++ maybe "" (\ i -> '-':h:show i++"M") rtsHeap
           ++ maybe "" (\ i -> "-k"++show i++"M") rtsStack
           ++ " -RTS "