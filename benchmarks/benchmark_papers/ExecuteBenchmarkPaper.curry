----------------------------------------------------------------
--- Executes Curry code snippets occurring in a latex file so that they
--- are replaced by their results computed by the Curry code.
---
--- The code snippets are marked by \runcurry{...} where
--- the argument must not contain any curly brackets or backslashes.
--- Furthermore, it must be a valid Curry expression of type `IO String`.
--- Moreover, the latex file can also contain program lines enclosed by
--- \begin{curryprog}
--- ...
--- \end{curryprog}
--- These program lines are included in the Curry program before the
--- code snippets are executed.
---
--- @author Michael Hanus
--- @version October 2014
----------------------------------------------------------------

import ReadShowTerm(readQTerm)
import Char(isDigit)
import List(intercalate)
import IO
import System
import Directory
import FileGoodies
import Distribution(installDir,curryCompiler)
import Time

-- Curry system executable to be used to execute Curry code snippets.
currySystem = installDir++"/bin/"++curryCompiler

-- The cleancurry command of the Curry system
cleanCurry = installDir++"/bin/cleancurry"

-- The LaTeX macro file containing definitions for Curry macros:
currycodeFile = "currycode.sty"

main bminstalldir = do
  args <- getArgs
  processArgs bminstalldir False args

processArgs bminstalldir runlatex args = case args of
  [] -> showError
  ["-h"]      -> showHelp
  ["-?"]      -> showHelp
  ["--help"]  -> showHelp
  "-f":rargs  -> processArgs bminstalldir True rargs
  [infile]    -> if head infile == '-'
                 then showError
                 else let texfile = if fileSuffix infile == "tex"
                                    then infile
                                    else infile++".tex"
                       in mainExec bminstalldir texfile runlatex
  _ -> showError
 where
  showError =
    error ("Illegal arguments (use '--help' for description):\n"++unwords args)

showHelp = putStrLn $
  "Usage: <prog> <options> <texfile>\n" ++
  "where <options> can contain:\n"++
  "-h     : show help info\n"++
  "-?     : show help info\n"++
  "--help : show help info\n"++
  "-f     : format generated LaTeX file with pdflatex and show it with evince\n"

mainExec bminstalldir textfile runlatex = do
  hascode <- extractCode bminstalldir textfile
  when (hascode && runlatex) $ do
    st <- system $ "pdflatex "++textfile
    when (st==0) (system ("evince "++stripSuffix textfile++".pdf") >> done)
    exitWith st

-- Extract Curry code snippets from a latex file.
-- The code snippets are stored in a temporary Curry program as
-- pairs of the code snippet text and the code snippet.
-- Then the temporary Curry program is executed to generate
-- a latex macro file (with suffix `.currycode.tex`) defining
-- for each code snippet its execution result.
extractCode :: String -> String -> IO Bool
extractCode bminstalldir textfile = do
  pid <- getPID
  let tmpname   = "tmpxxx"++show pid
      curryfile = tmpname++".curry"
      macrofile = stripSuffix textfile ++ ".currycode.tex"
  absmacrofile <- getAbsoluteFileName macrofile
  cnts <- readFile textfile
  hc <- openFile curryfile WriteMode
  hPutStrLn hc "import ExecuteBenchmarkPaper\n"
  codesnippets <- extractCurryCode hc cnts
  if null codesnippets
   then do
     hClose hc
     removeFile curryfile
     putStrLn "No code snippets found, nothing done"
     return False
   else do
     saveOldFile absmacrofile
     copyFile (bminstalldir++'/':currycodeFile)
              (dirName absmacrofile++'/':currycodeFile)
     hPutStrLn hc $
       concatMap genMacro (zip [1..] codesnippets) ++
       "\nmain = genMacroFile [" ++
                 intercalate "," (map (\i->macroOpName i)
                                      [1 .. length codesnippets]) ++
                 "] \""++absmacrofile++"\"\n"
     hClose hc
     putStrLn "Computing results for Curry code snippets..."
     ec <- system $ unwords [currySystem,":set path",bminstalldir,
                             ":load",curryfile,":eval main",":quit"]
     system $ cleanCurry ++ " " ++ tmpname
     if ec==0
      then removeFile curryfile >> return True
      else error $ "Something went wrong when executing Curry code snippets\n"++
                   "Inspect generated Curry program in \""++curryfile++"\"\n"
 where
   macroOpName i = "runCurryMacro" ++ show i

   genMacro (i,m) =
     '\n':macroOpName i++" :: (String, IO String)\n"++
          macroOpName i++" = ("++show m++","++m++")\n"

--- If a file with the given name `fname` exists, it is moved
--- to the file `fname.date`, where `date` is the modification date
--- of this file.
saveOldFile :: String -> IO ()
saveOldFile fname = do
  exfname <- doesFileExist fname
  when exfname $ do
    mdate <- getModificationTime fname
    ctime <- toCalendarTime mdate
    let savename = fname ++ "." ++
                   intercalate "_"
                     (map (\f -> show (f ctime))
                          [ctYear,ctMonth,ctDay,ctHour,ctMin,ctSec])
    renameFile fname savename

--- Extract \begin{curryprog}...\end{curryprog} and \runcurry{...} from a text.
--- The "{...}" must not contain any further curly brackets.
--- The extracted `curryprog` code is written to the handle.
extractCurryCode :: Handle -> String -> IO [String]
extractCurryCode _ [] = return []
extractCurryCode hc (c:cs) = case c of
  '\\' -> if take 9 cs == "runcurry{"
          then let (s,ds) = break (=='}') (drop 9 cs)
                in if null ds
                   then error ("UNCLOSED MACRO: "++c:take 50 cs++"...")
                   else do xs <- extractCurryCode hc (tail ds)
                           return (s:xs)
          else extractCurryCode hc cs
  '\n' -> if not (null cs) && head cs == '%'
          then skipLine cs
          else if take 17 cs == "\\begin{curryprog}"
               then copyCurryCode hc (drop 17 cs) >>=
                    extractCurryCode hc
               else extractCurryCode hc cs
  _    -> extractCurryCode hc cs
 where
  skipLine [] = return []
  skipLine (d:ds) = if d=='\n'
                    then extractCurryCode hc (d:ds)
                    else skipLine ds

copyCurryCode _ [] = error "\\end{curryprog} missing!"
copyCurryCode hc (d:ds) = case d of
  '\n' -> if take 15 ds == "\\end{curryprog}"
          then hPutChar hc '\n' >> return (drop 15 ds)
          else hPutChar hc d >> copyCurryCode hc ds
  _    -> hPutChar hc d >> copyCurryCode hc ds


-- Generate a LaTeX file containing results of macro execution.
genMacroFile :: [(String,IO String)] -> String -> IO ()
genMacroFile macros outfile = do
  s <- mapIO showMacro macros
  writeFile outfile ("\\newcommand{\\curryresult}[1]\n"++genResultMacro s++"\n")
  putStrLn $ "Execution results written into file '"++outfile++"'"
 where
  showMacro (m,act) = do
    bmReport m
    s <- act
    return ("\\ifthenelse{\\equal{#1}{" ++m ++ "}}{" ++ s ++"}\n")

  genResultMacro [] = "{\\{\\texttt{#1}\\}}\n"
  genResultMacro (t:ts) = "{"++t++"{"++genResultMacro ts++"}}"

-- Report execution of a code snippet:
bmReport c = putStrLn $ "Running benchmark code: " ++ c

----------------------------------------------------------------
-- Auxiliaries

-- Transforms a file name into an absolute file name.
getAbsoluteFileName fname = do
  let (fdir,bname) = splitDirectoryBaseName fname
  if fdir == "."
    then do
      currentDir <- getCurrentDirectory
      return (currentDir++"/"++bname)
    else
     if head fdir /= '/' -- TODO: does not work for Windows
     then do -- is relative path name
       currentDir <- getCurrentDirectory
       return (currentDir++"/"++fname)
     else return fname

----------------------------------------------------------------