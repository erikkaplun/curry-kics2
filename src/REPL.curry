--- --------------------------------------------------------------------------
--- Read-Eval-Print loop for KiCS2
---
--- @author Michael Hanus
--- @version April 2011
--- --------------------------------------------------------------------------

import RCFile
import System(system,getArgs,getEnviron,setEnviron,getPID)
import Char(isAlpha,isDigit,isSpace,toLower)
import IO
import IOExts
import FileGoodies
import Directory
import ReadShowTerm (readQTermFile)
import ReadNumeric (readNat)
import List (isPrefixOf,isInfixOf,intersperse,nub,union)
import FlatCurry (flatCurryFileName)
import Sort (mergeSort)
import AbstractCurry
import Distribution
import qualified Installation as Inst
import Files
import Names (funcInfoFile)

banner = unlines [bannerLine,bannerText,bannerDate,bannerLine]
 where
   bannerText = "KiCS2 Curry->Haskell Compiler (Version "++
                show Inst.majorVersion ++ "." ++ show Inst.minorVersion ++
                " of "++ Inst.compilerDate ++ ")"
   bannerDate = "(installed at "++Inst.installDate++")"
   bannerLine = take (length bannerText) (repeat '=')

mainGoalFile = "Curry_Main_Goal.curry"

-- Remove mainGoalFile and auxiliaries
cleanMainGoalFile :: ReplState -> IO ()
cleanMainGoalFile rst
  | rcValue (rst->rcvars) "keepfiles" == "yes" = done
  | otherwise = do
     system $ Inst.installDir++"/bin/cleancurry "++mainGoalFile
     goalfileexists <- doesFileExist mainGoalFile
     unless (not goalfileexists) $ removeFile mainGoalFile

-- REPL state:
type ReplState =
  { idcHome      :: String     -- installation directory of the system
  , rcvars       :: [(String,String)] -- content of rc file
  , idSupply     :: String     -- IDSupply implementation (ioref or integer)
  , verbose      :: Int        -- verbosity level: 0 = quiet,
                               -- 1 = show frontend (module) compile/load
                               -- 2 = show backend (Haskell) compile/load
                               -- 3 = show intermediate messages, commands
                               -- 4 = show intermediate results
  , importPaths  :: [String]   -- additional directories to search for imports
  , outputSubdir :: String
  , mainMod      :: String     -- name of main module
  , addMods      :: [String]   -- names of additionally added modules
  , optim        :: Bool       -- compile with optimization
  , ndMode       :: NonDetMode -- mode for non-deterministic main goal
  , firstSol     :: Bool       -- print only first solution to nd main goal?
  , interactive  :: Bool       -- interactive execution of goal?
  , showBindings :: Bool       -- show free variables in main goal in output?
  , showTime     :: Bool       -- show execution of main goal?
  , rtsOpts      :: String     -- run-time options for ghc
  , quit         :: Bool       -- terminate the REPL?
  , sourceguis   :: [(String,(String,Handle))] -- handles to SourceProgGUIs
  }

-- Mode for non-deterministic evaluation of main goal
data NonDetMode = DFS | BFS | IDS Int | Par Int | PrDFS

-- Result of compiling main goal
data MainGoalCompile =
   GoalError                               -- error occurred
 | GoalWithoutBindings CurryProg           -- goal does not contain free vars
 | GoalWithBindings CurryProg Int String   -- number of vars / new goal

-- Result of compiling main program
data MainCompile = MainError | MainDet | MainNonDet

-- Initial state of REPL:
initReplState :: ReplState
initReplState =
  { idcHome      = ""
  , rcvars       = []
  , idSupply     = "integer"
  , verbose      = 1
  , importPaths  = []
  , outputSubdir = "/.curry/kics2/"
  , mainMod      = "Prelude"
  , addMods      = []
  , optim        = True
  , ndMode       = DFS
  , firstSol     = False
  , interactive  = False
  , showBindings = True
  , showTime     = False
  , rtsOpts      = ""
  , quit         = False
  , sourceguis   = []
  }

-- Show an info message for a given verbosity level
writeVerboseInfo :: ReplState -> Int -> String -> IO ()
writeVerboseInfo rst level msg =
  if rst -> verbose < level then done else putStrLn msg

-- Show an error message
writeErrorMsg :: String -> IO ()
writeErrorMsg msg = putStrLn ("ERROR: "++msg)

main = do
  rcdefs <- readRC
  args   <- getArgs
  let rst = { idcHome := Inst.installDir
            , rcvars := rcdefs 
            | initReplState }
  processArgsAndStart { importPaths := defaultImportPaths rst | rst }
    (map strip (words (rcValue (rst->rcvars) "defaultparams")) ++ args)

-- The default import paths of KiCS2.
defaultImportPaths :: ReplState -> [String]
defaultImportPaths rst =
  let rclibs = rcValue (rst->rcvars) "libraries"
   in (if null rclibs then [] else splitPath rclibs) ++
      map (Inst.installDir </>) ["/lib","/lib/meta"]

defaultImportPathsWith :: ReplState -> String -> [String]
defaultImportPathsWith rst dirs = splitPath dirs ++ defaultImportPaths rst

processArgsAndStart rst [] =
  if rst -> quit
  then cleanUpRepl rst
  else do writeVerboseInfo rst 1 banner
          writeVerboseInfo rst 1 "Type \":h\" for help"
          repl rst
processArgsAndStart rst (arg:args) =
  if head arg /= ':'
  then writeErrorMsg ("unknown command: " ++ unwords (arg:args)) >> printHelp
  else do let (cmdargs,more) = break (\a -> head a == ':') args
          mbrst <- processCommand rst (tail (unwords (arg:cmdargs)))
          maybe printHelp (\rst' -> processArgsAndStart rst' more) mbrst
 where
  printHelp = putStrLn "Usage: kics2 <list of commands>\n" >> printHelpOnCommands

-- The main read-eval-print loop:
repl :: ReplState -> IO ()
repl rst = do
  putStr (unwords (rst->addMods ++ [rst-> mainMod]) ++ "> ")
  hFlush stdout
  eof <- isEOF
  if eof then cleanUpRepl rst
   else do input <- getLine
           processInput rst (strip input)

-- Clean resources of REPL before terminating it.
cleanUpRepl :: ReplState -> IO ()
cleanUpRepl rst = terminateSourceProgGUIs rst >> done

processInput :: ReplState -> String -> IO ()
processInput rst g
  | null g = repl rst
  | head g == ':' = do mbrst <- processCommand rst (strip (tail g))
                       maybe (repl rst)
                             (\rst' -> if (rst'->quit) then cleanUpRepl rst'
                                                       else repl rst')
                             mbrst
  | otherwise = evalExpression rst g >> repl rst

-- Evaluate an expression w.r.t. currently loaded modules
evalExpression :: ReplState -> String -> IO ()
evalExpression rst expr = do
  status <- compileProgramWithGoal rst expr
  unless (status==MainError) (execMain rst status expr >> done)
  cleanMainGoalFile rst

-- Generate, read, and delete .acy file of main goal file.
-- Return Nothing if some error occurred during parsin.
getAcyOfMainGoal :: ReplState -> IO (Maybe CurryProg)
getAcyOfMainGoal rst = do
  let mainGoalProg = stripSuffix mainGoalFile
      acyMainGoalFile = inCurrySubdir (mainGoalProg ++ ".acy")
      frontendParams = setQuiet (if rst->verbose < 2 then True else False)
                         (setFullPath ("." : rst->importPaths) defaultParams)
  callFrontendWithParams ACY frontendParams mainGoalProg
  acyexists <- doesFileExist acyMainGoalFile
  if not acyexists then return Nothing else do
    prog <- readAbstractCurryFile acyMainGoalFile
    removeFile acyMainGoalFile
    return (Just prog)

-- Show the type of goal w.r.t. main program:
showTypeOfGoal :: ReplState -> String -> IO Bool
showTypeOfGoal rst goal = do
  writeMainGoalFile rst [] Nothing goal
  mbprog <- getAcyOfMainGoal rst
  removeFile mainGoalFile
  maybe (return False)
        (\ (CurryProg _ _ _ [mfunc] _) -> do
           let (CFunc _ _ _ maintype _) = mfunc
           putStrLn $ goal ++ " :: " ++ showMonoTypeExpr False False maintype
           return True)
        mbprog

-- Get the module of a function visible in the main program:
getModuleOfFunction :: ReplState -> String -> IO String
getModuleOfFunction rst funname = do
  writeMainGoalFile rst [] Nothing
    (if isAlpha (head funname) then funname else '(':funname++")")
  mbprog <- getAcyOfMainGoal rst
  removeFile mainGoalFile
  maybe (return "")
        (\ (CurryProg _ _ _ [mfunc] _) -> do
           let (CFunc _ _ _ _ mainrules) = mfunc
           return (modOfMain mainrules))
        mbprog
 where
  modOfMain r = case r of
                  CRules _ [CRule [] [(_,CSymbol (mod,_))] []] -> mod
                  _ -> ""

-- Compile main program with goal:
compileProgramWithGoal :: ReplState -> String -> IO MainCompile
compileProgramWithGoal rst goal = do
  let infoFile = funcInfoFile (rst -> outputSubdir) mainGoalFile
  oldmaincurryexists <- doesFileExist infoFile
  unless (not oldmaincurryexists) $ removeFile infoFile
  oldmainfcyexists <- doesFileExist (flatCurryFileName mainGoalFile)
  unless (not oldmainfcyexists) $ removeFile (flatCurryFileName mainGoalFile)
  writeMainGoalFile rst [] Nothing goal
  goalstate <- insertFreeVarsInMainGoal rst goal
  if goalstate==GoalError then return MainError else do
    let (newprog,newgoal) = case goalstate of
                    GoalWithBindings p _ g -> (p,g)
                    GoalWithoutBindings p  -> (p,goal)
    typeok <- makeMainGoalMonomorphic rst newprog newgoal
    if typeok
     then do
      status <- compileCurryProgram rst mainGoalFile True
      exinfo <- doesFileExist infoFile
      if status==0 && exinfo then createAndCompileMain rst goalstate
                             else return MainError
     else return MainError

-- write the file with the main goal where necessary imports
-- and possibly a type string is provided:
writeMainGoalFile :: ReplState -> [String] -> Maybe String -> String -> IO ()
writeMainGoalFile rst imports mtype goal =
  writeFile mainGoalFile
            (unlines $ map ("import "++)
                           (nub (rst->mainMod : rst->addMods ++ imports)) ++
                       (maybe [] (\ts -> ["idcMainGoal :: "++ts]) mtype) ++
                       ["idcMainGoal = "++goal])

--- If the main goal is polymorphic, make it monomorphic by adding a type
--- declaration where type variables are replaced by type "()".
--- If the main goal has type "IO t" where t is monomorphic, t/=(),
--- and t is not a function, then ">>= print" is added to the goal.
--- The result is False if the main goal contains some error.
makeMainGoalMonomorphic :: ReplState -> CurryProg -> String -> IO Bool
makeMainGoalMonomorphic rst (CurryProg _ _ _ [mfunc] _) goal = do
  let (CFunc _ _ _ maintype _) = mfunc
      newgoal = goal ++ (if isIOReturnType maintype then " >>= print" else "")
  if isFunctionalType maintype
   then writeErrorMsg "expression is of functional type" >> return False
   else
    if isPolyType maintype
     then do writeMainGoalFile rst (modsOfType maintype)
                               (Just (showMonoTypeExpr True False maintype))
                               goal
             writeVerboseInfo rst 2
               ("Type of main expression \"" ++
                showMonoTypeExpr False False maintype ++
                "\" made monomorphic")
             writeVerboseInfo rst 1
                "Type variables of main expression replaced by \"()\""
             return True
     else
      if newgoal==goal
       then return True
       else writeMainGoalFile rst [] Nothing newgoal >> return True

-- Insert free variables occurring in the main goal as components
-- of the main goal so that their bindings are shown
-- The status of the main goal is returned.
insertFreeVarsInMainGoal :: ReplState -> String -> IO MainGoalCompile
insertFreeVarsInMainGoal rst goal = getAcyOfMainGoal rst >>=
  maybe (return GoalError)
   (\ prog@(CurryProg _ _ _ [mfunc] _) -> do
    let freevars = freeVarsInFuncRule mfunc
    if null freevars || not (rst -> showBindings) || length freevars > 5
     then return (GoalWithoutBindings prog)
     else let (exp,whereclause) = break (=="where") (words goal)
           in if null whereclause then return (GoalWithoutBindings prog) else do
              let newgoal = unwords $
                        ["("] ++
                        exp ++ [",["] ++
                        intersperse "," (map (\v->"\""++v++"\"") freevars) ++
                        ["]"] ++
                        map (\v->',':v) freevars ++
                        ")":whereclause
              writeMainGoalFile rst [] Nothing newgoal
              writeVerboseInfo rst 2
                ("Adding printing of bindings for free variables: "++
                 concat (intersperse "," freevars))
              mbprog <- getAcyOfMainGoal rst
              return (maybe GoalError
                            (\p -> GoalWithBindings p (length freevars) newgoal)
                            mbprog)
   )
 where
  freeVarsInFuncRule (CFunc _ _ _ _ (CRules _ [CRule _ _ ldecls])) =
    concatMap lvarName ldecls

  lvarName ldecl = case ldecl of CLocalVar (_,v) -> [v]
                                 _               -> []

-- Compile a Curry program with IDC compiler:
compileCurryProgram :: ReplState -> String -> Bool -> IO Int
compileCurryProgram rst curryprog ismain = do
  let compileProg = (rst->idcHome) </> "bin" </> "idc"
      idcoptions  = --(if rst->verbose < 2 then "-q " else "") ++
                    "-v " ++ show (verbREPL2IDC (rst->verbose)) ++ " " ++
                    (concatMap (\i -> " -i "++i) (rst->importPaths))
      compileCmd  = unwords [compileProg,idcoptions,curryprog]
  writeVerboseInfo rst 3 $ "Executing: "++compileCmd
  system compileCmd
 where
  verbREPL2IDC v | v==1 && not ismain = 2 -- to get frontend messages
                 | otherwise          = v

readInfoFile :: ReplState -> IO [((String,String),Bool)]
readInfoFile rst = do
  readQTermFile (funcInfoFile (rst -> outputSubdir) mainGoalFile)

-- Create and compile the main module containing the main goal
createAndCompileMain :: ReplState -> MainGoalCompile -> IO MainCompile
createAndCompileMain rst goalstate = do
  infos <- readInfoFile rst
  --print infos
  let isdet = not (null (filter (\i -> (snd (fst i)) == "d_C_idcMainGoal")
                                infos))
      isio  = snd
               (head
                (filter (\i -> snd (fst i) ==
                           (if isdet then "d" else "nd") ++ "_C_idcMainGoal")
                        infos))
  writeVerboseInfo rst 3 $ "Initial goal is " ++
                  (if isdet then "" else "non-") ++ "deterministic and " ++
                  (if isio then "" else "not ") ++ "of IO type..."
  createHaskellMain rst goalstate isdet isio
  let ghcImports = [ rst -> idcHome ++ "/runtime"
                   , rst -> idcHome ++ "/runtime/idsupply" ++ rst -> idSupply
                   , "." </> rst -> outputSubdir
                   ]
                   ++ map (</> rst -> outputSubdir) (rst -> importPaths)
      ghcCompile = unwords ["ghc"
                           ,if rst->optim then "-O2" else ""
                           ,"--make"
                           ,if rst->verbose < 2 then "-v0" else "-v1"
                           ,"-XMultiParamTypeClasses"
                           ,"-XFlexibleInstances"
                           ,"-XRelaxedPolyRec" --due to problem in FlatCurryShow
                           ,case rst->ndMode of
                              Par _ -> "-threaded"
                              _     -> ""
                           ,"-i"++concat (intersperse ":" ghcImports)
                           ,"." </> rst -> outputSubdir </> "Main.hs"]
                     -- also: -fforce-recomp -funbox-strict-fields ?
  writeVerboseInfo rst 2 $ "Compiling Main.hs with: "++ghcCompile
  status <- system ghcCompile
  return (if status>0 then MainError else
          if isdet || isio then MainDet else MainNonDet)

-- Create the Main.hs program containing the call to the initial expression:
createHaskellMain rst goalstate isdet isio =
  let printOperation = case goalstate of
                         GoalWithBindings _ n _ -> "printWithBindings"++show n
                         _                      -> "print"
      mainPrefix = if isdet then "d_C_" else "nd_C_"
      mainOperation =
        if isio then (if isdet then "evalDIO" else "evalIO" ) else
        if isdet then "evalD" else
        if rst->ndMode == PrDFS then "prdfs "++printOperation
        else let moreDefault = case rcValue (rst->rcvars) "moresolutions" of
                                 "yes" -> "MoreYes"
                                 "no"  -> "MoreNo"
                                 "all" -> "MoreAll"
                                 _     -> "MoreYes"
                 searchSuffix = if rst->interactive then "i "++moreDefault else
                                if rst->firstSol    then "1" else ""
              in ("print" ++ case (rst->ndMode) of
                              DFS   -> "DFS"++searchSuffix
                              BFS   -> "BFS"++searchSuffix
                              IDS d -> "IDS"++searchSuffix++" "++show d
                              Par _ -> "Par"++searchSuffix )++' ':printOperation
   in writeFile ("." </> rst -> outputSubdir </> "Main.hs") $
       "module Main where\n"++
       "import MonadList\n"++
       "import Basics\n"++
       (if printOperation=="print" then "" else "import PrintBindings\n") ++
       "import Curry_"++stripSuffix mainGoalFile++"\n"++
       "main = "++mainOperation++" "++mainPrefix++"idcMainGoal\n"


-- Execute main program and show run time:
execMain :: ReplState -> MainCompile -> String -> IO Int
execMain rst cmpstatus mainexp = do
  timecmd <- getTimeCmd
  let paropts = case rst->ndMode of
                  Par n -> "-N" ++ (if n==0 then "" else show n)
                  _     -> ""
      maincmd = ("." </> rst -> outputSubdir </> "Main") ++
                (if null (rst->rtsOpts) && null paropts
                 then " "
                 else " +RTS "++rst->rtsOpts++" "++paropts++" -RTS")
      tcmd    = timecmd ++ maincmd
      icmd    = if rst->interactive && cmpstatus==MainNonDet
                then execInteractive rst tcmd
                else tcmd
  writeVerboseInfo rst 1 $ "Evaluating expression: " ++ strip mainexp
  writeVerboseInfo rst 3 $ "Executing: " ++ icmd
  system icmd
 where
  getTimeCmd = if rst->showTime
               then getDistribution >>= return . getTimeCmdForDist
               else return ""

  -- Time command for specific distributions. It might be necessary
  -- to adapt this command.
  getTimeCmdForDist dist
    | "Ubuntu" `isInfixOf` dist
     = "time --format=\"Execution time: %Us / elapsed: %E\" "
    | "Debian" `isInfixOf` dist
      = "export TIMEFORMAT=\"Execution time: %2Us / elapsed: %2Es\" && time "
    | otherwise = "time "

  getDistribution = do
    (hin,hout,herr) <- execCmd "lsb_release -i"
    dist <- hGetContents hout
    hClose hin
    hClose hout
    hClose herr
    return dist

-- all the available commands:
allCommands = ["quit","help","?","load","reload","add","eval",
               "browse","cd","fork",
               "programs","edit","interface","source","show","set","save",
               "type","usedimports"]

-- Process a command of the REPL.
-- The result is either just a new ReplState or Nothing if an error occurred.
processCommand :: ReplState -> String -> IO (Maybe ReplState)
processCommand rst cmds
  | null cmds = writeErrorMsg "unknown command" >> return Nothing
  | head cmds == '!' = system (tail cmds) >> return (Just rst)
  | otherwise = let (cmd,args) = break (==' ') cmds
                    allcmds = filter (isPrefixOf (map toLower cmd)) allCommands
                 in
      if null allcmds
      then writeErrorMsg ("unknown command: ':"++cmds++"'") >> return Nothing
      else if length allcmds > 1
           then writeErrorMsg ("ambiguous command: ':"++cmds++"'") >>
                return Nothing
           else processThisCommand rst (head allcmds) (strip args)

processThisCommand :: ReplState -> String -> String -> IO (Maybe ReplState)
processThisCommand rst cmd args
  | cmd=="quit" = return (Just { quit := True | rst })
  | cmd=="help" || cmd=="?"
   = do printHelpOnCommands
        putStrLn "...or type any <expression> to evaluate\n"
        return (Just rst)
  | cmd=="load"
   = do rst' <- terminateSourceProgGUIs rst
        let dirmodname = stripSuffix args
        if null dirmodname
         then writeErrorMsg "missing module name" >> return Nothing
         else do
          let (dirname,modname) = splitDirectoryBaseName dirmodname
          if dirname=="." then done else setCurrentDirectory dirname
          mbf <- lookupFileInPath modname [".curry", ".lcurry"] ["."]
          maybe (writeErrorMsg "source file of module not found" >>
                 return Nothing)
                (\_ -> compileCurryProgram rst' modname False >>
                     return (Just { mainMod := modname, addMods := [] | rst' }))
                mbf
  | cmd=="reload"
   = if rst->mainMod == "Prelude"
     then writeErrorMsg "no program loaded!" >> return Nothing
     else if null (stripSuffix args)
          then do compileCurryProgram rst (rst->mainMod) False
                  return (Just rst)
          else writeErrorMsg "superfluous argument" >> return Nothing
  | cmd=="add"
   = do let modname = stripSuffix args
        if null modname
         then writeErrorMsg "missing module name" >> return Nothing
         else do
          mbf <- lookupFileInPath modname [".curry", ".lcurry"]
                                          ("." : rst->importPaths)
          maybe (writeErrorMsg "source file of module not found" >>
                 return Nothing)
                (\_ -> return (Just { addMods := modname : rst->addMods | rst}))
                mbf
  | cmd=="eval"
   = do evalExpression rst args
        return (Just rst)
  | cmd=="type"
   = do typeok <- showTypeOfGoal rst args
        return (if typeok then Just rst else Nothing)
  | cmd=="cd"
   = do exdir <- doesDirectoryExist args
        if exdir
         then (setCurrentDirectory args >> return (Just rst))
         else (writeErrorMsg "directory does not exist" >> return Nothing)
  | cmd=="programs" = printAllLoadPathPrograms rst >> return (Just rst)
  | cmd=="edit"
   = do let modname = if null args then rst->mainMod else stripSuffix args
        mbf <- lookupFileInPath modname [".curry", ".lcurry"]
                                ("." : rst->importPaths)
        editenv <- getEnviron "EDITOR"
        let editcmd = rcValue (rst->rcvars) "editcommand"
            editprog = if null editcmd then editenv else editcmd
        if null editenv && null editcmd
         then writeErrorMsg "no editor defined" >> return Nothing
         else maybe (writeErrorMsg "source file not found" >> return Nothing)
                (\fn -> system (editprog++" "++fn++"& ") >> return (Just rst))
                mbf
  | cmd=="source"
   = let (mod,dotfun) = break (=='.') args
      in if null dotfun
         then do m <- getModuleOfFunction rst args
                 if null m
                  then writeErrorMsg "function not found" >> return Nothing
                  else showFunctionInModule rst m args
         else showFunctionInModule rst mod (tail dotfun)
  | cmd=="show"
   = do let modname = if null args then rst->mainMod else stripSuffix args
        mbf <- lookupFileInPath modname [".curry", ".lcurry"]
                                ("." : rst->importPaths)
        maybe (writeErrorMsg "source file not found" >> return Nothing)
              (\fn -> do let showcmd = rcValue (rst->rcvars) "showcommand"
                         system $ (if null showcmd then "cat" else showcmd)
                                  ++' ':fn
                         putStrLn ""
                         return (Just rst)
              )
              mbf
  | cmd=="interface"
   = do let modname  = if null args then rst->mainMod else stripSuffix args
            toolexec = "tools/GenInt"
            genint   = rst->idcHome </> toolexec
        giexists <- doesFileExist genint
        if giexists
         then system (genint ++ " -int " ++ modname) >> return (Just rst)
         else errorMissingTool toolexec >> return Nothing
  | cmd=="browse"
   = if not (null (stripSuffix args))
     then writeErrorMsg "superfluous argument" >> return Nothing
     else do
        let toolexec = "tools/browser/BrowserGUI"
            browser  = rst->idcHome </> toolexec
        cbexists <- doesFileExist browser
        if cbexists
         then system (browser ++ " " ++ rst->mainMod) >> return (Just rst)
         else errorMissingTool toolexec >> return Nothing
  | cmd=="usedimports"
   = do let modname  = if null args then rst->mainMod else stripSuffix args
            toolexec = "tools/ImportCalls"
            icalls   = rst->idcHome </> toolexec
        icexists <- doesFileExist icalls
        if icexists
         then system (icalls ++ " " ++ modname) >> return (Just rst)
         else errorMissingTool toolexec >> return Nothing
  | cmd=="set" = processSetOption rst args
  | cmd=="save"
   = if rst->mainMod == "Prelude"
     then writeErrorMsg "no program loaded" >> return Nothing
     else do
       status <- compileProgramWithGoal rst (if null args then "main" else args)
       unless (status==MainError) $ do
          renameFile ("." </> rst -> outputSubdir </> "Main") (rst->mainMod)
          writeVerboseInfo rst 1 ("Executable saved in '"++rst->mainMod++"'")
       cleanMainGoalFile rst
       return (Just rst)
  | cmd=="fork"
   = if rst->mainMod == "Prelude"
     then writeErrorMsg "no program loaded" >> return Nothing
     else do
       status <- compileProgramWithGoal rst (if null args then "main" else args)
       unless (status==MainError) $ do
          pid <- getPID
          let execname = "/tmp/kics2fork" ++ show pid
          system ("mv ." </> rst -> outputSubdir </> "Main " ++ execname)
          writeVerboseInfo rst 3 ("Starting executable '"++execname++"'...")
          system ("( "++execname++" && rm -f "++execname++ ") "++
                  "> /dev/null 2> /dev/null &") >> done
       cleanMainGoalFile rst
       return (Just rst)
  | otherwise = writeErrorMsg ("unknown command: ':"++cmd++"'") >>
                return Nothing

-- Process setting of an option
processSetOption :: ReplState -> String -> IO (Maybe ReplState)
processSetOption rst option
  | null (strip option) = printOptions rst >> return (Just rst)
  | otherwise = let (opt,args) = break (==' ') option
                    allopts = filter (isPrefixOf (map toLower opt)) allOptions
                 in
     if null allopts
     then writeErrorMsg ("unknown option: ':"++option++"'") >> return Nothing
     else if length allopts > 1
          then writeErrorMsg ("ambiguous option: ':"++option++"'") >>
               return Nothing
          else processThisOption rst (head allopts) (strip args)

allOptions = ["bfs","dfs","prdfs","ids","par","paths","supply","rts",
              "v0","v1","v2","v3","v4"] ++
             concatMap (\f->['+':f,'-':f])
                       ["interactive","first","optimize","bindings",
                        "time"]

processThisOption :: ReplState -> String -> String -> IO (Maybe ReplState)
processThisOption rst option args
  | option=="paths"
   = if null args
     then return (Just { importPaths := defaultImportPaths rst | rst })
     else return (Just { importPaths := defaultImportPathsWith rst args | rst })
  | option=="bfs" = return (Just { ndMode := BFS | rst })
  | option=="dfs" = return (Just { ndMode := DFS | rst })
  | option=="prdfs" = return (Just { ndMode := PrDFS | rst })
  | option=="ids"
   = if null args
     then return (Just { ndMode := IDS 100 | rst })
     else maybe (writeErrorMsg "illegal number" >> return Nothing)
                (\ (n,s) -> if null (strip s)
                            then return (Just { ndMode := IDS n | rst })
                            else writeErrorMsg "illegal number" >> return Nothing)
                (readNat args)
  | option=="par"
   = if null args
     then return (Just { ndMode := Par 0 | rst })
     else maybe (writeErrorMsg "illegal number" >> return Nothing)
                (\ (n,s) -> if null (strip s)
                            then return (Just { ndMode := Par n | rst })
                            else writeErrorMsg "illegal number" >> return Nothing)
                (readNat args)
  | option=="supply"       = return (Just { idSupply := args | rst })
  | option=="v0"           = return (Just { verbose := 0 | rst })
  | option=="v1"           = return (Just { verbose := 1 | rst })
  | option=="v2"           = return (Just { verbose := 2 | rst })
  | option=="v3"           = return (Just { verbose := 3 | rst })
  | option=="v4"           = return (Just { verbose := 4 | rst })
  | option=="+interactive" = return (Just { interactive := True  | rst })
  | option=="-interactive" = return (Just { interactive := False | rst })
  | option=="+first"       = return (Just { firstSol := True  | rst })
  | option=="-first"       = return (Just { firstSol := False | rst })
  | option=="+optimize"    = return (Just { optim := True  | rst })
  | option=="-optimize"    = return (Just { optim := False | rst })
  | option=="+bindings"    = return (Just { showBindings := True  | rst })
  | option=="-bindings"    = return (Just { showBindings := False | rst })
  | option=="+time"        = return (Just { showTime := True  | rst })
  | option=="-time"        = return (Just { showTime := False | rst })
  | option=="rts"          = return (Just { rtsOpts := args | rst })
  | otherwise = writeErrorMsg ("unknown option: '"++option++"'") >>
                return Nothing

printOptions rst = putStrLn $
  "Options for ':set' command:\n"++
  "path <paths>   - set additional search paths for imported modules\n"++
  "prdfs          - set search mode to primitive depth-first search\n"++
  "dfs            - set search mode to depth-first search\n"++
  "bfs            - set search mode to breadth-first search\n"++
  "ids [<n>]      - set search mode to iterative deepening (initial depth <n>)\n"++
  "par [<n>]      - set search mode to parallel search with <n> threads\n"++
  "supply <I>     - set idsupply implementation (integer or ioref)\n"++
  "v<n>           - verbosity level (0: quiet; 1: front end messages;\n"++
  "                 2: backend messages, 3: intermediate messages and commands;\n"++
  "                 4: all intermediate results)\n"++
  "+/-interactive - turn on/off interactive execution of main goal\n"++
  "+/-first       - turn on/off printing only first solution\n"++
  "+/-optimize    - turn on/off optimization\n"++
  "+/-bindings    - show bindings of free variables in initial goal\n"++
  "+/-time        - show execution time\n"++
  "rts <opts>     - run-time options for ghc (+RTS <opts> -RTS)\n" ++
  showCurrentOptions rst

showCurrentOptions rst = "\nCurrent settings:\n"++
  "import paths     : " ++
     concat (intersperse ":" ("." : rst->importPaths)) ++ "\n" ++
  "search mode      : " ++
      (case (rst->ndMode) of
         PrDFS -> "primitive non-monadic depth-first search"
         DFS -> "depth-first search"
         BFS -> "breadth-first search"
         IDS d -> "iterative deepening (initial depth: "++show d++")"
         Par s -> "parallel search with "++show s++" threads"
      ) ++ "\n" ++
  "idsupply         : " ++ rst->idSupply ++ "\n" ++
  "run-time options : " ++ rst->rtsOpts ++ "\n" ++
  "verbosity        : " ++ show (rst->verbose) ++ "\n" ++
  showOnOff (rst->interactive)  ++ "interactive " ++
  showOnOff (rst->firstSol)     ++ "first " ++
  showOnOff (rst->optim)        ++ "optimize " ++
  showOnOff (rst->showBindings) ++ "bindings " ++
  showOnOff (rst->showTime)     ++ "time "
 where
   showOnOff b = if b then "+" else "-"

printHelpOnCommands = putStrLn $
  "Commands (can be abbreviated to a prefix if unique)\n"++
  ":load <prog>     - load program \"<prog>.[l]curry\" as main module\n"++
  ":add  <prog>     - add module \"<prog>\" to currently loaded modules\n"++
  ":reload          - recompile currently loaded modules\n"++
  ":eval <expr>     - evaluate expression <expr>\n"++
  ":type <expr>     - show type of expression <expr>\n"++
  ":programs        - show names of all Curry programs available in load path\n"++
  ":cd <dir>        - change current directory to <dir>\n"++
  ":edit            - load source of currently loaded module into editor\n"++
  ":edit <mod>      - load source of module <m> into editor\n"++
  ":show            - show currently loaded source program\n"++
  ":show <mod>      - show source of module <m>\n"++
  ":source <f>      - show source of (visible!) function <f>\n"++
  ":source <m>.<f>  - show source of function <f> in module <m>\n"++
  ":browse          - browse program and its imported modules\n"++
  ":interface       - show currently loaded source program\n"++
  ":interface <mod> - show source of module <m>\n"++
  ":usedimports     - show all used imported functions/constructors\n"++
  ":set <option>    - set an option\n"++
  ":set             - see help on options and current options\n"++
  ":save            - save executable with main expression 'main'\n"++
  ":save <expr>     - save executable with main expression <expr>\n"++
  ":fork <expr>     - fork new process evaluating <expr>\n"++
  ":help            - show this message\n"++
  ":!<command>      - execute <command> in shell\n"++
  ":quit            - leave the system\n"

-- Print all Curry programs in current load path:
printAllLoadPathPrograms rst = mapIO_ printDirPrograms ("." : rst->importPaths)
 where
  printDirPrograms dir = do
    putStrLn $ "Curry programs in directory "++dir++":"
    files <- getDirectoryContents dir
    putStrLn $ concat $ mergeSort (<=) $
      map (\f -> if take 6 (reverse f) == "yrruc." ||
                    take 7 (reverse f) == "yrrucl."
                 then let fb = stripSuffix f
                       in (if null fb then "" else f++" ")
                 else "") files
    putStrLn ""

-- Print error message for missing executable for some tool
errorMissingTool execfile = writeErrorMsg $
  Inst.installDir ++ '/' : execfile ++ " not found\n" ++
  "Possible solution: run \"cd "++Inst.installDir++" && make install\""

-----------------------------------------------------------------------
-- Showing source code of functions via SourcProgGUI tool.
-- If necessary, open a new connection and remember it in the repl state.
showFunctionInModule :: ReplState -> String -> String -> IO (Maybe ReplState)
showFunctionInModule rst mod fun = do
  let mbh      = lookup mod (rst->sourceguis)
      toolexec = "tools/SourceProgGUI"
      spgui    = rst->idcHome </> toolexec
  spgexists <- doesFileExist spgui
  if not spgexists
   then errorMissingTool toolexec >> return Nothing
   else do
    (rst',h') <- maybe (do h <- connectToCommand (spgui++" "++mod)
                           return ({sourceguis := (mod,(fun,h))
                                              : rst->sourceguis | rst}, h))
                       (\ (f,h) -> do
                           hPutStrLn h ('-':f)
                           hFlush h
                           return ({sourceguis := updateFun (rst->sourceguis)
                                                          mod fun | rst }, h))
                       mbh
    hPutStrLn h' ('+':fun)
    hFlush h'
    return (Just rst')
 where
   updateFun [] _ _ = []
   updateFun ((m,(f,h)):sguis) md fn =
     if m==md then (m,(fn,h)):sguis
              else (m,(f,h)) : updateFun sguis md fn

-- Terminate all open SourceProgGUIs
terminateSourceProgGUIs rst = let sguis = rst -> sourceguis in
  if null sguis
  then return rst
  else do
   writeVerboseInfo rst 1 "Terminating source program GUIs..."
   catch (mapIO_ (\ (_,(_,h)) -> hPutStrLn h "q" >> hFlush h >> hClose h) sguis)
         (\_ -> done)
   return { sourceguis := [] | rst }

-----------------------------------------------------------------------
-- Auxiliaries:

unless :: Bool -> IO () -> IO ()
unless p act = if p then done else act

-- Interactive execution of a main search command: current, a new
-- terminal is opened due to problematic interaction with readline
execInteractive rst cmd =
  rcValue (rst->rcvars) "interactivecommand" ++ ' ':cmd

-----------------------------------------------------------------------
-- Auxiliaries for process AbstractCurry type expressions

--- Returns true if the type expression contains type variables.
isPolyType :: CTypeExpr -> Bool
isPolyType (CTVar _) = True
isPolyType (CFuncType domain range) = isPolyType domain || isPolyType range
isPolyType (CTCons _ typelist) = any isPolyType typelist

--- Returns true if the type expression is a functional type.
isFunctionalType :: CTypeExpr -> Bool
isFunctionalType texp = case texp of CFuncType _ _ -> True
                                     _             -> False

--- Returns true if the type expression is (IO t) with t/=() and
--- t is not functional
isIOReturnType :: CTypeExpr -> Bool
isIOReturnType (CTVar _) = False
isIOReturnType (CFuncType _ _) = False
isIOReturnType (CTCons tc typelist) =
  tc==(prelude,"IO") && head typelist /= CTCons (prelude,"()") []
  && not (isFunctionalType (head typelist))

--- Returns all modules used in the given type.
modsOfType :: CTypeExpr -> [String]
modsOfType (CTVar _) = []
modsOfType (CFuncType t1 t2) = union (modsOfType t1) (modsOfType t2)
modsOfType (CTCons (mod,_) typelist) =
  foldr union [mod] (map modsOfType typelist)

--- Shows an AbstractCurry type expression in standard Curry syntax.
--- If the first argument is True, all occurrences of type variables
--- are replaced by "()".
--- If the second argument is True, the type expression is enclosed
--- in brackets.
showMonoTypeExpr :: Bool -> Bool -> CTypeExpr -> String
showMonoTypeExpr mono _ (CTVar (_,name)) =
   if mono then "()" else showIdentifier name
showMonoTypeExpr mono nested (CFuncType domain range) =
   maybeShowBrackets nested
                     (showMonoTypeExpr mono (isCFuncType domain) domain ++
                      " -> " ++ showMonoTypeExpr mono False range)
showMonoTypeExpr mono nested (CTCons (mod,name) typelist)
   | mod==prelude && name == "untyped" = "-"
   | otherwise  = maybeShowBrackets (nested && not (null typelist))
                                    (showTypeCons mono mod name typelist)

showTypeCons :: Bool -> String -> String -> [CTypeExpr] -> String
showTypeCons _ _ name [] = name
showTypeCons mono mod name (t:ts)
   | mod == prelude = showPreludeTypeCons mono name (t:ts)
   | otherwise = name ++ (prefixMap (showMonoTypeExpr mono True) (t:ts) " ")

showPreludeTypeCons :: Bool -> String -> [CTypeExpr] -> String
showPreludeTypeCons mono name typelist
  | name == "[]" && head typelist == CTCons (prelude,"Char") [] = "String"
  | name == "[]" = "[" ++ (showMonoTypeExpr mono False (head typelist)) ++ "]"
  | isTuple name = "(" ++
                   combineMap (showMonoTypeExpr mono False) typelist "," ++ ")"
  | otherwise    = name ++ (prefixMap (showMonoTypeExpr mono True) typelist " ")

-- Remove characters '<' and '>' from identifiers sind these characters
-- are sometimes introduced in new identifiers generated by the front end (for sections)
showIdentifier :: String -> String
showIdentifier = filter (not . flip elem "<>")

isCFuncType t = case t of
                  CFuncType _ _ -> True
                  _ -> False

prelude = "Prelude"

-- enclose string with brackets, if required by first argument:
maybeShowBrackets nested s =
   (if nested then "(" else "") ++ s ++ (if nested then ")" else "")

prefixMap :: (a -> String) -> [a] ->  String -> String
prefixMap f xs s = concatMap ((++)s) (map f xs)

combineMap :: (a -> String) -> [a] ->  String -> String
combineMap _ [] _ = ""
combineMap f (x:xs) s = (f x) ++ (prefixMap f xs s)

isTuple :: String -> Bool
isTuple [] = False
isTuple (x:xs) = (x == '(') && (p1_isTuple xs)
   where
   p1_isTuple [] = False
   p1_isTuple (z:[]) = z == ')'
   p1_isTuple (z1:z2:zs) = (z1 == ',') && (p1_isTuple (z2:zs))

-----------------------------------------------------------------------
