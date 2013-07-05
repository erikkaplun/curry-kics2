--- --------------------------------------------------------------------------
--- This is the main module of the interactice system.
--- It implements the Read-Eval-Print loop for KiCS2
---
--- @author Michael Hanus, Bjoern Peemoeller
--- @version September 2012
--- --------------------------------------------------------------------------

module REPL where

import AbstractCurry
import Char          (isAlpha, isAlphaNum, isDigit, isSpace, toLower)
import Directory
import Distribution
import FilePath
  ( (</>), (<.>), dropExtension
  , splitSearchPath, splitFileName, splitExtension
  )
import FileGoodies   (lookupFileInPath, splitPath)
import FlatCurry     (flatCurryFileName)
import IO
import IOExts
import List          (intercalate, intersperse, isPrefixOf, isInfixOf, nub)
import ReadNumeric   (readNat)
import ReadShowTerm  (readsTerm)
import Sort          (mergeSort)
import System        (system, getArgs, getEnviron, setEnviron, getPID)
import Time

import AbstractCurryGoodies
import Files         (removeFileIfExists)
import GhciComm      (stopGhciComm)
import qualified Installation as Inst
import Names         (funcInfoFile)
import RCFile
import Utils         (liftIO, notNull, strip, unless)

import Linker

--- Result of compiling main goal
data GoalCompile
  = GoalError                             -- error occurred
  | GoalWithoutBindings CurryProg         -- goal does not contain free vars
  | GoalWithBindings CurryProg Int String -- number of vars / new goal

--- Show an error message
writeErrorMsg :: String -> IO ()
writeErrorMsg msg = putStrLn ("ERROR: " ++ msg)

-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  rcDefs <- readRC
  args   <- getArgs
  let rst = { kics2Home := Inst.installDir
            , rcvars    := rcDefs
            | initReplState
            }
  ipath  <- defaultImportPaths rst
  processArgsAndStart { importPaths := ipath | rst }
    (map strip (words (rcValue (rst :> rcvars) "defaultparams")) ++ args)

--- The default import paths of KiCS2.
--- It consists of the path defined by the environment variable CURRYPATH,
--- and the "libraries" property defined in ~/.kics2rc
defaultImportPaths :: ReplState -> IO [String]
defaultImportPaths rst = do
  currypath <- getEnviron "CURRYPATH"
  let rclibs = rcValue (rst :> rcvars) "libraries"
  return $ splitPath currypath ++ splitPath rclibs

defaultImportPathsWith :: ReplState -> String -> IO [String]
defaultImportPathsWith rst dirs =
  (splitPath dirs ++) `Utils.liftIO` defaultImportPaths rst

processArgsAndStart :: ReplState -> [String] -> IO ()
processArgsAndStart rst []
  | rst :> quit = cleanUpRepl rst
  | otherwise   = do getBanner >>= writeVerboseInfo rst 1
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

--- Retrieve the KiCS2 banner
getBanner :: IO String
getBanner = do
  logo <- readFile $ Inst.installDir </> "include" </> "logo" <.> "txt"
  return (logo ++ version)
 where version = "Version "
              ++ show Inst.majorVersion ++ "." ++ show Inst.minorVersion
              ++ "." ++ show Inst.revisionVersion
              ++ " of " ++ Inst.compilerDate
              ++ " (installed at " ++ Inst.installDate ++ ")"

-- ---------------------------------------------------------------------------

-- The main read-eval-print loop:
repl :: ReplState -> IO ()
repl rst = do
  putStr (calcPrompt rst) >> hFlush stdout
  eof <- isEOF
  if eof
    then cleanUpRepl rst
    else do getLine >>= processInput rst . strip

calcPrompt :: ReplState -> String
calcPrompt rst = subst (rst :> prompt)
 where
  loaded = unwords (rst :> addMods ++ [rst:> mainMod])
  subst []       = []
  subst [c]      = [c]
  subst (c:d:cs) = case c of
    '%' -> case d of
      '%' -> '%' : cs
      's' -> loaded ++ subst cs
      _   -> c : d : subst cs
    _   -> c : subst (d:cs)

-- Clean resources of REPL before terminating it.
cleanUpRepl :: ReplState -> IO ()
cleanUpRepl rst = terminateSourceProgGUIs rst >> done

processInput :: ReplState -> String -> IO ()
processInput rst g
  | null g      = repl rst
  | isCommand g = do mbrst <- processCommand rst (strip (tail g))
                     maybe (repl rst)
                           (\rst' -> if (rst':>quit) then cleanUpRepl rst'
                                                     else repl rst')
                           mbrst
  | otherwise   = evalExpression rst g >>= repl
 where isCommand str = notNull str && head str == ':'

--- Evaluate an expression w.r.t. currently loaded modules
evalExpression :: ReplState -> String -> IO ReplState
evalExpression rst expr = do
  (rst', status) <- compileProgramWithGoal rst (not (rst :> useGhci)) expr
  unless (status == MainError || (rst' :> useGhci && not (rst' :> interactive)))
         (execMain rst' status expr)
  cleanMainGoalFile rst'
  return rst'

-- ---------------------------------------------------------------------------
-- Main goal file stuff
-- ---------------------------------------------------------------------------

writeSimpleMainGoalFile :: ReplState -> String -> IO ()
writeSimpleMainGoalFile rst goal = writeMainGoalFile rst [] Nothing goal

-- write the file with the main goal where necessary imports
-- and possibly a type string is provided:
writeMainGoalFile :: ReplState -> [String] -> Maybe String -> String -> IO ()
writeMainGoalFile rst imports mtype goal = writeFile mainGoalFile $
  unlines $ map ("import " ++) allImports
         ++ maybe [] (\ts -> ["kics2MainGoal :: " ++ ts]) mtype
         ++ ["kics2MainGoal = " ++ goal]
  where allImports = nub $ rst :> mainMod : rst :> addMods ++ imports

-- Remove mainGoalFile and auxiliaries
cleanMainGoalFile :: ReplState -> IO ()
cleanMainGoalFile rst = unless keepfiles $ do
  system $ Inst.installDir </>  "bin" </> "cleancurry " ++ mainGoalFile
  removeFileIfExists mainGoalFile
 where keepfiles = rcValue (rst :> rcvars) "keepfiles" == "yes"

-- Generate, read, and delete .acy file of main goal file.
-- Return Nothing if some error occurred during parsing.
getAcyOfMainGoal :: ReplState -> IO (Maybe CurryProg)
getAcyOfMainGoal rst = do
  let mainGoalProg    = dropExtension mainGoalFile
      acyMainGoalFile = inCurrySubdir $ mainGoalProg <.> "acy"
      frontendParams  = setQuiet    (rst :> verbose < 2)
                      $ setFullPath    (loadPaths rst)
                      $ setExtended    (rcValue (rst :> rcvars) "curryextensions" /= "no")
                      $ setOverlapWarn (rcValue (rst :> rcvars) "warnoverlapping" /= "no")
                        defaultParams
  callFrontendWithParams ACY frontendParams mainGoalProg
  prog <- tryReadACYFile acyMainGoalFile
  removeFileIfExists acyMainGoalFile
  return prog
--   acyExists <- doesFileExist acyMainGoalFile
--   if not acyExists
--     then return Nothing
--     else do
--       acySize <- fileSize acyMainGoalFile
--       if acySize == 0
--         then return Nothing
--         else do
--           prog <- tryReadACYFile acyMainGoalFile
--           removeFile acyMainGoalFile
--           return prog

getAcyOfExpr :: ReplState -> String -> IO (Maybe CurryProg)
getAcyOfExpr rst expr = do
  writeSimpleMainGoalFile rst expr
  mbProg <- getAcyOfMainGoal rst
  removeFileIfExists mainGoalFile
  return mbProg

-- Show the type of goal w.r.t. main program:
showTypeOfGoal :: ReplState -> String -> IO Bool
showTypeOfGoal rst goal = do
  mbProg <- getAcyOfExpr rst goal
  maybe (return False)
        (\ (CurryProg _ _ _ [CFunc _ _ _ ty _] _) -> do
          putStrLn $ goal ++ " :: " ++ showMonoTypeExpr False ty
          return True)
        mbProg

-- Get the module of a function visible in the main program:
getModuleOfFunction :: ReplState -> String -> IO String
getModuleOfFunction rst funname = do
  mbprog <- getAcyOfExpr rst $
    if isAlpha (head funname) then funname else '(' : funname ++ ")"
  return $ maybe ""
                 (\ (CurryProg _ _ _ [CFunc _ _ _ _ mainrules] _) ->
                    modOfMain mainrules)
                 mbprog
 where modOfMain r = case r of
        CRules _ [CRule [] [(_, CSymbol (m, _))] []] -> m
        _ -> ""

-- Compile main program with goal:
compileProgramWithGoal :: ReplState -> Bool -> String -> IO (ReplState, MainCompile)
compileProgramWithGoal rst createExecutable goal = do
  let infoFile = funcInfoFile (rst :> outputSubdir) mainGoalFile
  removeFileIfExists infoFile
  removeFileIfExists $ flatCurryFileName mainGoalFile
  writeSimpleMainGoalFile rst goal
  goalstate <- getAcyOfMainGoal rst >>= insertFreeVarsInMainGoal rst goal
  if goalstate == GoalError
    then return (rst, MainError)
    else do
      let (newprog, newgoal, bindings) =
            case goalstate of
              GoalWithBindings p n g -> (p, g   , Just n )
              GoalWithoutBindings p  -> (p, goal, Nothing)
      typeok <- makeMainGoalMonomorphic rst newprog newgoal
      if typeok
        then do
          status <- compileCurryProgram rst mainGoalFile
          exinfo <- doesFileExist infoFile
          if status == 0 && exinfo
            then createAndCompileMain rst createExecutable goal bindings
            else return (rst, MainError)
        else return (rst, MainError)

-- Insert free variables occurring in the main goal as components
-- of the main goal so that their bindings are shown
-- The status of the main goal is returned.
insertFreeVarsInMainGoal :: ReplState -> String -> Maybe CurryProg -> IO GoalCompile
insertFreeVarsInMainGoal _ _ Nothing = return GoalError
insertFreeVarsInMainGoal rst goal (Just prog@(CurryProg _ _ _ [mfunc@(CFunc _ _ _ ty _)] _)) = do
  let freevars           = freeVarsInFuncRule mfunc
      (exp, whereclause) = break (== "where") (words goal)
  if null freevars
      || not (rst :> showBindings)
      || isPrtChoices (rst :> ndMode)
      || isIOType ty
      || length freevars > 10 -- due to limited size of tuples used
                              -- in PrintBindings
      || null whereclause
    then return (GoalWithoutBindings prog)
    else do
      let newgoal = unwords $
            ["("] ++ exp ++ [",["] ++
            intersperse "," (map (\v-> "\"" ++ v ++ "\"") freevars) ++
            ["]"] ++ map (\v->',':v) freevars ++ ")":whereclause
      writeVerboseInfo rst 2 $
        "Adding printing of bindings for free variables: " ++
          intercalate "," freevars
      writeSimpleMainGoalFile rst newgoal
      mbprog <- getAcyOfMainGoal rst
      return (maybe GoalError
                    (\p -> GoalWithBindings p (length freevars) newgoal)
                    mbprog)
 where
  isPrtChoices c = case c of
    PrtChoices _ -> True
    _            -> False
  freeVarsInFuncRule (CFunc _ _ _ _ (CRules _ [CRule _ _ ldecls])) =
    concatMap lvarName ldecls

  lvarName ldecl = case ldecl of CLocalVar (_,v) -> [v]
                                 _               -> []

--- If the main goal is polymorphic, make it monomorphic by adding a type
--- declaration where type variables are replaced by type "()".
--- If the main goal has type "IO t" where t is monomorphic, t /= (),
--- and t is not a function, then ">>= print" is added to the goal.
--- The result is False if the main goal contains some error.
makeMainGoalMonomorphic :: ReplState -> CurryProg -> String -> IO Bool
makeMainGoalMonomorphic rst (CurryProg _ _ _ [(CFunc _ _ _ ty _)] _) goal
  | isFunctionalType ty
  = writeErrorMsg "expression is of functional type" >> return False
  | isPolyType ty = do
    writeMainGoalFile rst (modsOfType ty) (Just $ showMonoTypeExpr True ty)
                          goal
    writeVerboseInfo rst 2 $
      "Type of main expression \"" ++ showMonoTypeExpr False ty
      ++ "\" made monomorphic"
    writeVerboseInfo rst 1
      "Type variables of main expression replaced by \"()\""
    return True
  | otherwise = do
    unless (newgoal == goal) $ writeSimpleMainGoalFile rst newgoal
    return True
 where newgoal = if isIOReturnType ty
                    then '(' : goal ++ ") >>= print"
                    else goal

-- Compile a Curry program with IDC compiler:
compileCurryProgram :: ReplState -> String -> IO Int
compileCurryProgram rst curryprog = do
  writeVerboseInfo rst 3 $ "Executing: " ++ kics2Cmd
  system kics2Cmd
 where
  kics2Bin  = rst :> kics2Home </> "bin" </> ".local" </> "kics2c"
  kics2Opts = unwords
              [ "-v" ++ show (rst :> verbose)
              , "-i" ++ intercalate ":" (loadPaths rst)
              ]
  kics2Cmd  = unwords [kics2Bin, kics2Opts, rst :> cmpOpts, curryprog]

--- Execute main program and show run time:
execMain :: ReplState -> MainCompile -> String -> IO ()
execMain rst _ mainexp = do -- _ was cmpstatus
  timecmd <- getTimeCmd
  let paropts = case rst :> ndMode of
                  Par n -> "-N" ++ (if n == 0 then "" else show n)
                  _     -> ""
      maincmd = ("." </> rst :> outputSubdir </> "Main") ++
                (if null (rst :> rtsOpts) && null paropts
                 then " "
                 else " +RTS " ++ rst :> rtsOpts ++ " " ++ paropts ++ " -RTS ") ++
                rst:>rtsArgs
      tcmd    = timecmd ++ maincmd
  writeVerboseInfo rst 1 $ "Evaluating expression: " ++ strip mainexp
  writeVerboseInfo rst 3 $ "Executing: " ++ tcmd
  system tcmd >> done
 where
  getTimeCmd | rst :> showTime = getTimeCmdForDist `Utils.liftIO` getDistribution
             | otherwise       = return ""

  -- Time command for specific distributions. It might be necessary
  -- to adapt this command.
  getTimeCmdForDist dist
    | True --"Ubuntu" `isInfixOf` dist
     = "time --format=\"Execution time: %Us / elapsed: %E\" "
    | "Debian" `isInfixOf` dist
      = "export TIMEFORMAT=\"Execution time: %2Us / elapsed: %2Es\" && time "
    | otherwise = "time "

  getDistribution = do
    (hin, hout, herr) <- execCmd "lsb_release -i"
    dist <- hGetContents hout
    hClose hin
    hClose hout
    hClose herr
    return dist

-- ---------------------------------------------------------------------------
-- Processing of REPL commands
-- ---------------------------------------------------------------------------

-- Process a command of the REPL.
-- The result is either just a new ReplState or Nothing if an error occurred.
processCommand :: ReplState -> String -> IO (Maybe ReplState)
processCommand rst cmds
  | null cmds        = skipCommand "unknown command"
  | head cmds == '!' = processSysCall rst (strip $ tail cmds)
  | otherwise        = case matchedCmds of
      []            -> skipCommand $ "unknown command: ':" ++ cmds ++ "'"
      [(_, act)]    -> act rst (strip args)
      (_:_:_)       -> skipCommand $ "ambiguous command: ':" ++ cmds ++ "'"
 where (cmd, args) = break (==' ') cmds
       matchedCmds = filter (isPrefixOf (map toLower cmd) . fst) replCommands

-- all available REPL commands
replCommands :: [(String, ReplState -> String -> IO (Maybe ReplState))]
replCommands =
  [ ("?"          , processHelp        )
  , ("add"        , processAdd         )
  , ("browse"     , processBrowse      )
  , ("cd"         , processCd          )
  , ("edit"       , processEdit        )
  , ("eval"       , processEval        )
  , ("fork"       , processFork        )
  , ("help"       , processHelp        )
  , ("interface"  , processInterface   )
  , ("load"       , processLoad        )
  , ("programs"   , processPrograms    )
  , ("reload"     , processReload      )
  , ("quit"       , processQuit        )
  , ("save"       , processSave        )
  , ("set"        , processSetOption   )
  , ("source"     , processSource      )
  , ("show"       , processShow        )
  , ("type"       , processType        )
  , ("usedimports", processUsedImports )
  ]

--- Skip an erroneous command with an error message
skipCommand :: String -> IO (Maybe ReplState)
skipCommand msg = writeErrorMsg msg >> return Nothing

--- Execute a call to a system command
processSysCall :: ReplState -> String -> IO (Maybe ReplState)
processSysCall rst cmd
  | null cmd  = skipCommand "missing system command"
  | otherwise = system cmd >> return (Just rst)

--- Process :quit command
processQuit :: ReplState -> String -> IO (Maybe ReplState)
processQuit rst _ = return (Just { quit := True | rst })

--- Process :help command
processHelp :: ReplState -> String -> IO (Maybe ReplState)
processHelp rst _ = do
  printHelpOnCommands
  putStrLn "... or type any <expression> to evaluate\n"
  return (Just rst)

--- Process :load command
processLoad :: ReplState -> String -> IO (Maybe ReplState)
processLoad rst args = do
  rst' <- terminateSourceProgGUIs rst
  let dirmodname = dropExtension args
  if null dirmodname
    then skipCommand "missing module name"
    else do
    let (dirname, modname) = splitFileName dirmodname
    unless (dirname == "./") $ setCurrentDirectory dirname
    mbf <- lookupFileInPath modname [".curry", ".lcurry"] ["."]
    maybe (skipCommand $ "source file of module " ++ dirmodname ++ " not found")
          (\fn ->
              readAndProcessSourceFileOptions rst' fn >>=
              maybe (return Nothing)
                (\rst'' -> compileCurryProgram rst'' modname >>
                return (Just { mainMod := modname, addMods := [] | rst'' }))
          )
          mbf

--- Process :reload command
processReload :: ReplState -> String -> IO (Maybe ReplState)
processReload rst args
  | rst :> mainMod == "Prelude"
  = skipCommand "no program loaded!"
  | null (dropExtension args)
  = compileCurryProgram rst (rst :> mainMod) >> return (Just rst)
  | otherwise
  = skipCommand "superfluous argument"

--- Process :add command
processAdd :: ReplState -> String -> IO (Maybe ReplState)
processAdd rst args = do
  let modname = dropExtension args
  if null modname
    then skipCommand "missing module name"
    else do
    mbf <- lookupFileInPath modname [".curry", ".lcurry"] (loadPaths rst)
    maybe (skipCommand "source file of module not found")
          (\ _ -> return (Just { addMods := modname : rst :> addMods | rst}))
          mbf

--- Process expression evaluation
processEval :: ReplState -> String -> IO (Maybe ReplState)
processEval rst args = evalExpression rst args >> return (Just rst)

--- Process :type command
processType :: ReplState -> String -> IO (Maybe ReplState)
processType rst args = do
  typeok <- showTypeOfGoal rst args
  return (if typeok then Just rst else Nothing)

--- Process :cd command
processCd :: ReplState -> String -> IO (Maybe ReplState)
processCd rst args = do
  exists <- doesDirectoryExist args
  if exists then setCurrentDirectory args >> return (Just rst)
            else skipCommand "directory does not exist"

--- Process :programs command
processPrograms :: ReplState -> String -> IO (Maybe ReplState)
processPrograms rst _ = printAllLoadPathPrograms rst >> return (Just rst)

--- Process :edit command
processEdit :: ReplState -> String -> IO (Maybe ReplState)
processEdit rst args = do
  let modname = if null args then rst :> mainMod else dropExtension args
  mbf <- lookupFileInPath modname [".curry", ".lcurry"]
                          ("." : rst :> importPaths)
  editenv <- getEnviron "EDITOR"
  let editcmd  = rcValue (rst :> rcvars) "editcommand"
      editprog = if null editcmd then editenv else editcmd
  if null editenv && null editcmd
    then skipCommand "no editor defined"
    else maybe (skipCommand "source file not found")
          (\fn -> system (editprog++" "++fn++"& ") >> return (Just rst))
          mbf

--- Process :source command
processSource :: ReplState -> String -> IO (Maybe ReplState)
processSource rst args
  | null args   = skipCommand "missing function name"
  | null dotfun = do m <- getModuleOfFunction rst args
                     if null m
                       then skipCommand "function not found"
                       else showFunctionInModule rst m args
  | otherwise   = showFunctionInModule rst mod (tail dotfun)
  where (mod, dotfun) = break (== '.') args

--- Process :show command
processShow :: ReplState -> String -> IO (Maybe ReplState)
processShow rst args = do
  let modname = if null args then rst :> mainMod else dropExtension args
  mbf <- lookupFileInPath modname [".curry", ".lcurry"] (loadPaths rst)
  case mbf of
    Nothing -> skipCommand "source file not found"
    Just fn -> do
      let showcmd = rcValue (rst :> rcvars) "showcommand"
      system $ (if null showcmd then "cat" else showcmd) ++ ' ' : fn
      putStrLn ""
      return (Just rst)

processInterface :: ReplState -> String -> IO (Maybe ReplState)
processInterface rst args = do
  let modname  = if null args then rst :> mainMod else dropExtension args
      toolexec = "currytools" </> "genint" </> "GenInt"
  callTool rst toolexec ("-int " ++ modname)

processBrowse :: ReplState -> String -> IO (Maybe ReplState)
processBrowse rst args
  | notNull $ dropExtension args = skipCommand "superfluous argument"
  | otherwise                  = do
      let toolexec = "currytools" </> "browser" </> "BrowserGUI"
      callTool rst toolexec (rst :> mainMod)

processUsedImports :: ReplState -> String -> IO (Maybe ReplState)
processUsedImports rst args = do
  let modname  = if null args then rst :> mainMod else dropExtension args
      toolexec = "currytools" </> "importcalls" </> "ImportCalls"
  callTool rst toolexec modname

processSave :: ReplState -> String -> IO (Maybe ReplState)
processSave rst args
  | rst :> mainMod == "Prelude" = skipCommand "no program loaded"
  | otherwise                   = do
    (rst', status) <- compileProgramWithGoal rst True
                      (if null args then "main" else args)
    unless (status == MainError) $ do
      renameFile ("." </> rst' :> outputSubdir </> "Main") (rst' :> mainMod)
      writeVerboseInfo rst' 1 ("Executable saved in '" ++ rst' :> mainMod ++ "'")
    cleanMainGoalFile rst'
    return (Just rst')

processFork :: ReplState -> String -> IO (Maybe ReplState)
processFork rst args
  | rst :> mainMod == "Prelude" = skipCommand "no program loaded"
  | otherwise                   = do
    (rst', status) <- compileProgramWithGoal rst True
                      (if null args then "main" else args)
    unless (status == MainError) $ do
      pid <- getPID
      tmp <- getTemporaryDirectory
      let execname = tmp </> "kics2fork" ++ show pid
      renameFile ("." </> rst' :> outputSubdir </> "Main") execname
      writeVerboseInfo rst' 3 ("Starting executable '" ++ execname ++ "'...")
      system ("( "++execname++" && rm -f "++execname++ ") "++
              "> /dev/null 2> /dev/null &") >> done
    cleanMainGoalFile rst'
    return (Just rst')

-- Process setting of an option
processSetOption :: ReplState -> String -> IO (Maybe ReplState)
processSetOption rst option
  | null (strip option) = printOptions rst >> return (Just rst)
  | otherwise           = case matched of
      []           -> skipCommand $ "unknown option: '" ++ option ++ "'"
      [(_,act)]    -> act rst (strip args)
      _            -> skipCommand $ "ambiguous option: ':" ++ option ++ "'"
 where (opt, args)  = break (==' ') option
       matched      = filter (isPrefixOf (map toLower opt) . fst) availOptions

-- In a global installation, the option for setting the identifier supply
-- is not available:
availOptions :: [(String, ReplState -> String -> IO (Maybe ReplState))]
availOptions = filter installOpts replOptions
  where installOpts (opt, _) = not Inst.installGlobal
                               || opt `notElem` ["supply"]

replOptions :: [(String, ReplState -> String -> IO (Maybe ReplState))]
replOptions =
  [ ("paths"        , setOptionPath                                      )
  , ("bfs"          , \r _ -> return (Just { ndMode       := BFS   | r }))
  , ("dfs"          , \r _ -> return (Just { ndMode       := DFS   | r }))
  , ("prdfs"        , \r _ -> return (Just { ndMode       := PrDFS | r }))
  , ("choices"      , setOptionNDMode PrtChoices 10                      )
  , ("ids"          , setOptionNDMode IDS        100                     )
  , ("par"          , setOptionNDMode Par        0                       )
  , ("supply"       , setOptionSupply                                    )
  , ("v0"           , \r _ -> return (Just { verbose      := 0     | r }))
  , ("v1"           , \r _ -> return (Just { verbose      := 1     | r }))
  , ("v2"           , \r _ -> return (Just { verbose      := 2     | r }))
  , ("v3"           , \r _ -> return (Just { verbose      := 3     | r }))
  , ("v4"           , \r _ -> return (Just { verbose      := 4     | r }))
  , ("prompt"       , setPrompt                                          )
  , ("+interactive" , \r _ -> return (Just { interactive  := True  | r }))
  , ("-interactive" , \r _ -> return (Just { interactive  := False | r }))
  , ("+first"       , \r _ -> return (Just { firstSol     := True  | r }))
  , ("-first"       , \r _ -> return (Just { firstSol     := False | r }))
  , ("+optimize"    , \r _ -> return (Just { optim        := True  | r }))
  , ("-optimize"    , \r _ -> return (Just { optim        := False | r }))
  , ("+bindings"    , \r _ -> return (Just { showBindings := True  | r }))
  , ("-bindings"    , \r _ -> return (Just { showBindings := False | r }))
  , ("+time"        , \r _ -> return (Just { showTime     := True  | r }))
  , ("-time"        , \r _ -> return (Just { showTime     := False | r }))
  , ("+ghci"        , \r _ -> return (Just { useGhci      := True  | r }))
  , ("-ghci"        , setNoGhci                                          )
  , ("cmp"          , \r a -> return (Just { cmpOpts      := a     | r }))
  , ("ghc"          , \r a -> return (Just { ghcOpts      := a     | r }))
  , ("rts"          , \r a -> return (Just { rtsOpts      := a     | r }))
  , ("args"         , \r a -> return (Just { rtsArgs      := a     | r }))
  ]

setPrompt :: ReplState -> String -> IO (Maybe ReplState)
setPrompt rst p
  | null rawPrompt = skipCommand "no prompt specified"
  | otherwise  = case head rawPrompt of
    '"' -> case readsTerm rawPrompt of
      [(strPrompt, [])] -> return (Just { prompt := strPrompt | rst })
      _                 -> skipCommand "could not parse prompt"
    _   -> return (Just { prompt := rawPrompt | rst })
 where rawPrompt = strip p

setNoGhci :: ReplState -> String -> IO (Maybe ReplState)
setNoGhci rst _ = do
  maybe done stopGhciComm (rst :> ghcicomm)
  return $ Just { useGhci := False, ghcicomm := Nothing | rst }

setOptionPath :: ReplState -> String -> IO (Maybe ReplState)
setOptionPath rst args = do
  ipath <- if null args then defaultImportPaths rst
                        else defaultImportPathsWith rst args
  return (Just { importPaths := ipath | rst })

setOptionNDMode :: (Int -> NonDetMode) -> Int
                -> ReplState -> String -> IO (Maybe ReplState)
setOptionNDMode mode defDepth rst args
  | null args = return (Just { ndMode := mode defDepth | rst })
  | otherwise = case readNat args of
      Nothing    -> skipCommand "illegal number"
      Just (n,s) -> if null (strip s)
                      then return (Just { ndMode := mode n | rst })
                      else skipCommand "illegal number"

setOptionSupply :: ReplState -> String -> IO (Maybe ReplState)
setOptionSupply rst args
  | args `elem` allSupplies = return (Just { idSupply := args | rst })
  | otherwise               = skipCommand "unknown identifier supply"
 where allSupplies = ["integer", "ghc", "ioref", "pureio", "giants"]

printOptions :: ReplState -> IO ()
printOptions rst = putStrLn $ unlines
  [ "Options for ':set' command:"
  , "path <paths>    - set additional search paths for imported modules"
  , "prdfs           - set search mode to primitive depth-first search"
  , "dfs             - set search mode to depth-first search"
  , "bfs             - set search mode to breadth-first search"
  , "ids [<n>]       - set search mode to iterative deepening (initial depth <n>)"
  , "par [<n>]       - set search mode to parallel search with <n> threads"
  , "choices [<n>]   - set search mode to print the choice structure as a tree"
  , "                  (up to level <n>)"
  , ifLocal "supply <I>      - set idsupply implementation (ghc|giants|integer|ioref|pureio)"
  , "v<n>            - verbosity level (0: quiet; 1: front end messages;"
  , "                  2: backend messages, 3: intermediate messages and commands;"
  , "                  4: all intermediate results)"
  , "prompt <prompt> - set the user prompt"
  , "+/-interactive  - turn on/off interactive execution of main goal"
  , "+/-first        - turn on/off printing only first solution"
  , "+/-optimize     - turn on/off optimization"
  , "+/-bindings     - show bindings of free variables in initial goal"
  , "+/-time         - show execution time"
  , "+/-ghci         - use ghci instead of ghc to evaluate main expression"
  , "cmp <opts>      - additional options passed to KiCS2 compiler"
  , "ghc <opts>      - additional options passed to GHC"
  , "rts <opts>      - run-time options for ghc (+RTS <opts> -RTS)"
  , "args <args>     - run-time arguments passed to main program"
  , showCurrentOptions rst
  ]

-- Hide string if the current installation is global:
ifLocal :: String -> String
ifLocal s = if Inst.installGlobal then "" else s

showCurrentOptions :: ReplState -> String
showCurrentOptions rst = "\nCurrent settings:\n"++
  "import paths      : " ++
     intercalate ":" ("." : rst :> importPaths) ++ "\n" ++
  "search mode       : " ++
      (case (rst :> ndMode) of
         PrDFS         -> "primitive non-monadic depth-first search"
         PrtChoices d  -> "show choice tree structure up to level " ++ show d
         DFS           -> "depth-first search"
         BFS           -> "breadth-first search"
         IDS d         -> "iterative deepening (initial depth: "++show d++")"
         Par s         -> "parallel search with "++show s++" threads"
      ) ++ "\n" ++
  "idsupply          : " ++ rst :> idSupply ++ "\n" ++
  "compiler options  : " ++ rst :> cmpOpts ++ "\n" ++
  "ghc options       : " ++ rst:>ghcOpts ++ "\n" ++
  "run-time options  : " ++ rst :> rtsOpts ++ "\n" ++
  "run-time arguments: " ++ rst :> rtsArgs ++ "\n" ++
  "verbosity         : " ++ show (rst :> verbose) ++ "\n" ++
  "prompt            : " ++ show (rst :> prompt)  ++ "\n" ++
  showOnOff (rst :> interactive ) ++ "interactive " ++
  showOnOff (rst :> firstSol    ) ++ "first "       ++
  showOnOff (rst :> optim       ) ++ "optimize "    ++
  showOnOff (rst :> showBindings) ++ "bindings "    ++
  showOnOff (rst :> showTime    ) ++ "time "        ++
  showOnOff (rst :> useGhci     ) ++ "ghci "
 where
   showOnOff b = if b then "+" else "-"

printHelpOnCommands :: IO ()
printHelpOnCommands = putStrLn $ unlines
  [ "Commands (can be abbreviated to a prefix if unique)"
  , ":load <prog>     - load program \"<prog>.[l]curry\" as main module"
  , ":add  <prog>     - add module \"<prog>\" to currently loaded modules"
  , ":reload          - recompile currently loaded modules"
  , ":eval <expr>     - evaluate expression <expr>"
  , ":type <expr>     - show type of expression <expr>"
  , ":programs        - show names of all Curry programs available in load path"
  , ":cd <dir>        - change current directory to <dir>"
  , ":edit            - load source of currently loaded module into editor"
  , ":edit <mod>      - load source of module <m> into editor"
  , ":show            - show currently loaded source program"
  , ":show <mod>      - show source of module <m>"
  , ":source <f>      - show source of (visible!) function <f>"
  , ":source <m>.<f>  - show source of function <f> in module <m>"
  , ":browse          - browse program and its imported modules"
  , ":interface       - show interface of currently loaded module"
  , ":interface <mod> - show interface of module <mod>"
  , ":usedimports     - show all used imported functions/constructors"
  , ":set <option>    - set an option"
  , ":set             - see help on options and current options"
  , ":save            - save executable with main expression 'main'"
  , ":save <expr>     - save executable with main expression <expr>"
  , ":fork <expr>     - fork new process evaluating <expr>"
  , ":help            - show this message"
  , ":!<command>      - execute <command> in shell"
  , ":quit            - leave the system"
  ]

--- Print all Curry programs in current load path
printAllLoadPathPrograms :: ReplState -> IO ()
printAllLoadPathPrograms rst = mapIO_ printDirPrograms (loadPaths rst)
 where
  printDirPrograms dir = do
    putStrLn $ "Curry programs in directory " ++ dir ++ ":"
    files <- getDirectoryContents dir
    putStrLn $ concat $ mergeSort (<=) $
      map (\f -> let (base, sfx) = splitExtension f
                  in if sfx `elem` ["curry", "lcurry"] && notNull base
                       then f ++ " "
                       else "") files
    putStrLn ""

-- Showing source code of functions via SourcProgGUI tool.
-- If necessary, open a new connection and remember it in the repl state.
showFunctionInModule :: ReplState -> String -> String -> IO (Maybe ReplState)
showFunctionInModule rst mod fun = do
  let mbh      = lookup mod (rst :> sourceguis)
      toolexec = "currytools" </> "browser" </> "SourceProgGUI"
      spgui    = rst:>kics2Home </> toolexec
  spgexists <- doesFileExist spgui
  if not spgexists
   then errorMissingTool toolexec
   else do
    (rst',h') <- maybe (do h <- connectToCommand (spgui++" "++mod)
                           return ({sourceguis := (mod,(fun,h))
                                              : rst:>sourceguis | rst}, h))
                       (\ (f,h) -> do
                           hPutStrLn h ('-':f)
                           hFlush h
                           return ({sourceguis := updateFun (rst:>sourceguis)
                                                          mod fun | rst }, h))
                       mbh
    hPutStrLn h' ('+':fun)
    hFlush h'
    return (Just rst')
 where
  updateFun []                _  _  = []
  updateFun ((m,(f,h)):sguis) md fn =
    if m==md then (m,(fn,h)):sguis
            else (m,(f,h)) : updateFun sguis md fn

callTool :: ReplState -> String -> String -> IO (Maybe ReplState)
callTool rst cmd args = do
  let path = rst :> kics2Home </> cmd
  exists <- doesFileExist path
  if exists
    then system (path ++ ' ' : args) >> return (Just rst)
    else errorMissingTool cmd

errorMissingTool :: String -> IO (Maybe ReplState)
errorMissingTool cmd = skipCommand $
     Inst.installDir ++ '/' : cmd ++ " not found\n"
  ++ "Possible solution: run \"cd " ++ Inst.installDir ++" && make install\""

-- ---------------------------------------------------------------------------
-- Read KiCS2 options in a Curry source file
-- Source file options are comments of the form
-- {-# KiCS2_OPTION <opt> #-}
-- occurring before the module header where <opt> is an option
-- of KiCS2 (i.e., ":set <opt>" is a valid KiCS2 command).
-- These options are read and processed when a module is loaded (not reloaded!).

readAndProcessSourceFileOptions :: ReplState -> String -> IO (Maybe ReplState)
readAndProcessSourceFileOptions rst fname = do
  opts <- readSourceFileOptions fname
  unless (null opts) $ writeVerboseInfo rst 1 $
    "Source file options: " ++ intercalate " | " (map unwords opts)
  processSourceFileOptions rst opts

processSourceFileOptions :: ReplState -> [[String]] -> IO (Maybe ReplState)
processSourceFileOptions rst []     = return (Just rst)
processSourceFileOptions rst (o:os) =
  processSetOption rst (unwords o) >>=
  maybe (return Nothing) (\rst' -> processSourceFileOptions rst' os)

readSourceFileOptions :: String -> IO [[String]]
readSourceFileOptions fname = do
  h <- openFile fname ReadMode
  headers <- readHeaderLines h
  hClose h
  return (filter notNull (map getOptions (filter isOptionComment headers)))
 where
  isOptionComment s = take 3 s == "{-#" -- #-}

  getOptions s =
   let optwords = words (extractOptionString (drop 3 s))
    in if null optwords || map toLower (head optwords) /= "kics2_option"
       then []
       else tail optwords

  extractOptionString []     = ""
  extractOptionString (c:cs) = case cs of
    []  -> ""
    [_] -> ""
    _   -> if (c : take 2 cs) == "#-}" then "" else c : extractOptionString cs

  readHeaderLines h = do
    eof <- hIsEOF h
    if eof then return []
           else do line <- hGetLine h
                   if isModuleStart line
                     then return []
                     else do lines <- readHeaderLines h
                             return (strip line : lines)
   where isModuleStart l = take 6 l `elem` ["module", "import"] ||
                           (let (w, _) = break isSpace l
                             in notNull w && all isAlphaNum w)

-- Terminate all open SourceProgGUIs
terminateSourceProgGUIs :: ReplState -> IO ReplState
terminateSourceProgGUIs rst
  | null sguis = return rst
  | otherwise  = do
      writeVerboseInfo rst 1 "Terminating source program GUIs..."
      catch (mapIO_ (\ (_,(_,h)) -> hPutStrLn h "q" >> hFlush h >> hClose h) sguis)
            (\_ -> done)
      return { sourceguis := [] | rst }
 where sguis = rst :> sourceguis
