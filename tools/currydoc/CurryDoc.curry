----------------------------------------------------------------------
--- Implementation of CurryDoc, a utility for the automatic
--- generation of HTML documentation from Curry programs.
---
--- @author Michael Hanus
----------------------------------------------------------------------

-- * All comments to be put into the HTML documentation must be
--   prefixed by "--- " (also in literate programs!).
--
-- * The comment of a module must occur before the first "module" or
--   "import" line of this module.
--
-- * The comment of a function or datatype must occur before the
--   first definition of this function or datatype.
--
-- * The comments can contain at the end several special comments:
--   @cons id comment   --> a comment for a constructor of a datatype
--   @param id comment  --> comment for function parameter id
--                          (list all parameters in left-to-right order)
--   @return comment    --> comments for the return value of a function
--   @author comment    --> the author of a module (only in module comments)
--   @version comment   --> the version of a module (only in module comments)
--
-- * Current restriction: doesn't properly work for infix operator definitions
--   without a type definition (so it should be always included)

module CurryDoc where

import CurryDocRead
import CurryDocHtml
import CurryDocTeX
import FlatCurry
import System
import Time
import List
import Directory
import FileGoodies
import AnaOverlapping
import AnaCompleteness
import AnaIndeterminism
import AnaOpComplete
import Distribution

--------------------------------------------------------------------------
-- Global definitions:

-- Version of currydoc
currydocVersion = "Version 0.4.2 of May 18, 2010"

-- Directory where include files for generated documention (e.g., icons,
-- css, tex includes) are stored:
includeDir = installDir++"/include"

--------------------------------------------------------------------------
-- Check arguments and call main function:
main = do
  args <- getArgs
  case args of
    ["--html",modname] -> makeCompleteDoc HtmlDoc True True
                                 ("DOC_"++stripSuffix modname)
                                 (stripSuffix modname)
    ["-html",modname] -> makeCompleteDoc HtmlDoc True True
                                 ("DOC_"++stripSuffix modname)
                                 (stripSuffix modname)
    ["--tex",modname] -> makeCompleteDoc TexDoc False False
                                 ("DOC_"++stripSuffix modname)
                                 (stripSuffix modname)
    ["-tex",modname] -> makeCompleteDoc TexDoc False False
                                 ("DOC_"++stripSuffix modname)
                                 (stripSuffix modname)
    ["--html",docdir,modname] ->
       makeCompleteDoc HtmlDoc True True docdir (stripSuffix modname)
    ["-html",docdir,modname] ->
       makeCompleteDoc HtmlDoc True True docdir (stripSuffix modname)
    ["--tex",docdir,modname] ->
       makeCompleteDoc TexDoc False False docdir (stripSuffix modname)
    ["--noindexhtml",docdir,modname] ->
       makeCompleteDoc HtmlDoc False True docdir (stripSuffix modname)
    ["-noindexhtml",docdir,modname] ->
       makeCompleteDoc HtmlDoc False True docdir (stripSuffix modname)
    ("--onlyindexhtml":docdir:modnames) ->
                        makeIndexPages docdir (map stripSuffix modnames)
    ("-onlyindexhtml":docdir:modnames) ->
                        makeIndexPages docdir (map stripSuffix modnames)
    _ -> putStrLn $
           "ERROR: Illegal arguments for currydoc: " ++
           concat (intersperse " " args) ++ "\n" ++
           "Usage: currydoc [--html|--tex] <module_name>\n" ++
           "       currydoc [--html|--tex] <doc directory> <module_name>\n" ++
           "       currydoc --noindexhtml <doc directory> <module_name>\n" ++
           "       currydoc --onlyindexhtml <doc directory> <module_names>\n"

-- create directory if not existent:
createDir :: String -> IO ()
createDir dir = do
  exdir <- doesDirectoryExist dir
  if exdir then done else system ("mkdir "++dir) >> done

--------------------------------------------------------------------------
-- The type of documentations which can be generated.
data DocType = HtmlDoc | TexDoc

--------------------------------------------------------------------------
--- The main function of the CurryDoc utility.
--- @param withindex - True if the index pages should also be generated
--- @param recursive - True if the documentation for the imported modules
---                    should be also generated (if necessary)
--- @param docdir - the directory name containing all documentation files
--- @param modname - the name of the main module to be documented
makeCompleteDoc :: DocType -> Bool -> Bool -> String -> String -> IO ()
makeCompleteDoc doctype withindex recursive docdir modname = do
  putStrLn("CurryDoc ("++currydocVersion++") - the Curry Documentation Tool\n")
  prepareDocDir doctype docdir
  -- parsing source program:
  callFrontend FCY modname
  (alltypes,allfuns,allops) <- readFlatCurryWithImports [modname]
  progname <- findSourceFileInLoadPath modname
  makeDocIfNecessary doctype recursive docdir
                     (genAnaInfo (Prog modname [] alltypes allfuns allops))
                     progname
  time <- getLocalTime
  if withindex
   then do genMainIndexPage     currydocVersion time docdir [modname]
           genFunctionIndexPage currydocVersion time docdir allfuns
           genConsIndexPage     currydocVersion time docdir alltypes
   else done
  -- change access rights to readable for everybody:
  system ("chmod -R go+rX "++docdir)
  done

--- Generate only the index pages for a list of (already compiled!) modules:
makeIndexPages :: String -> [String] -> IO ()
makeIndexPages docdir modnames = do
  putStrLn("CurryDoc ("++currydocVersion++") - the Curry Documentation Tool\n")
  prepareDocDir HtmlDoc docdir
  (alltypes,allfuns,_) <- readFlatCurryWithImports modnames
  time <- getLocalTime
  genMainIndexPage     currydocVersion time docdir modnames
  genFunctionIndexPage currydocVersion time docdir allfuns
  genConsIndexPage     currydocVersion time docdir alltypes
  -- change access rights to readable for everybody:
  system ("chmod -R go+rX "++docdir)
  done

-- create documentation directory (if necessary) with gifs and stylesheets:
prepareDocDir :: DocType -> String -> IO ()
prepareDocDir HtmlDoc docdir = do
  createDir docdir
  putStrLn ("Copying icons into documentation directory \""++docdir++"\"...")
  -- copying all icons:
  copyIncludeIfPresent docdir "currydocicons/*.gif"
  -- copy style sheet:
  copyIncludeIfPresent docdir "currydoc.css"
prepareDocDir TexDoc docdir = do
  createDir docdir
  putStrLn $ "Copy macros into documentation directory \""++docdir++"\"..."
  copyIncludeIfPresent docdir "currydoc.tex"

copyIncludeIfPresent docdir inclfile = do
  existIDir <- doesDirectoryExist includeDir
  if existIDir
   then system ("cp "++includeDir++"/"++inclfile++" "++docdir) >> done
   else done

-- generate all analysis infos:
genAnaInfo prog =
  AnaInfo (getFunctionInfo (analyseOverlappings prog))
          (getFunctionInfo (analyseCompleteness prog))
          (getFunctionInfo (analyseIndeterminism prog))
          (getFunctionInfo (analyseOpCompleteness prog))

-- generate documentation for a single module:
makeDoc :: DocType -> Bool -> String -> AnaInfo -> String -> IO ()
makeDoc doctype recursive docdir anainfo progname = do
  putStrLn ("Reading comments from file \""++progname++".curry\"...")
  (modcmts,progcmts) <- readComments (progname++".curry")
  makeDocWithComments doctype recursive docdir anainfo progname
                      modcmts progcmts

makeDocWithComments HtmlDoc recursive docdir anainfo progname
                    modcmts progcmts = do
  time <- getLocalTime
  (imports,hexps) <- generateHtmlDocs currydocVersion time anainfo progname
                                      modcmts progcmts
  let outfile = docdir++"/"++getLastName progname++".html"
  putStrLn $ "Writing documentation to \""++outfile++"\"..."
  writeFile outfile (showDocCSS ("Module "++getLastName progname) hexps)
  translateSource2ColoredHtml docdir progname
  if recursive
   then mapIO_ (makeDocIfNecessary HtmlDoc recursive docdir anainfo) imports
   else done

makeDocWithComments TexDoc recursive docdir anainfo progname
                    modcmts progcmts = do
  (imports,textxt) <- generateTexDocs anainfo progname modcmts progcmts
  let outfile = docdir++"/"++getLastName progname++".tex"
  putStrLn $ "Writing documentation to \""++outfile++"\"..."
  writeFile outfile textxt
  if recursive
   then mapIO_ (makeDocIfNecessary TexDoc recursive docdir anainfo) imports
   else done


--- Generates the documentation for a module if it is necessary.
--- I.e., the documentation is generated if no previous documentation
--- file exists or if the existing documentation file is older than
--- the FlatCurry file.
makeDocIfNecessary :: DocType -> Bool -> String -> AnaInfo -> String -> IO ()
makeDocIfNecessary doctype recursive docdir anainfo modname = do
  progname <- findSourceFileInLoadPath modname
  let docfile = docdir ++ "/" ++ getLastName progname ++
                (if doctype==HtmlDoc then ".html" else ".tex")
  docexists <- doesFileExist docfile
  if not docexists
   then copyOrMakeDoc doctype recursive docdir anainfo progname 
   else do ctime  <- getModificationTime (flatCurryFileName progname)
           dftime <- getModificationTime docfile
           if compareClockTime ctime dftime == GT
            then copyOrMakeDoc doctype recursive docdir anainfo progname
            else if recursive
                 then do imports <- getImports progname
                         mapIO_ (makeDocIfNecessary doctype recursive docdir
                                                    anainfo)
                                imports
                 else done

-- get imports of a program by reading the interface, if possible:
getImports progname = do
  let fintname = flatCurryIntName progname
  fintexists <- doesFileExist fintname
  (Prog _ imports _ _ _) <- if fintexists
                            then readFlatCurryFile fintname
                            else readFlatCurryFile (flatCurryFileName progname)
  return imports

copyOrMakeDoc :: DocType -> Bool -> String -> AnaInfo -> String -> IO ()
copyOrMakeDoc doctype recursive docdir anainfo progname = do
  hasCopied <- copyDocIfPossible doctype docdir progname
  if hasCopied then done
               else makeDoc doctype recursive docdir anainfo progname

--- Copy the documentation file from standard documentation directoy "CDOC"
--- (used for documentation of system libraries) if possible.
--- Returns true if the copy was possible.
copyDocIfPossible :: DocType -> String -> String -> IO Bool
copyDocIfPossible TexDoc _ _ = return False -- ignore copying for TeX docs
copyDocIfPossible HtmlDoc docdir progname =
  let docprogname = getDirName progname++"/CDOC/"++getLastName progname in
  do docexists <- doesFileExist (docprogname++".html")
     if not docexists
      then return False
      else
       do ctime <- getModificationTime (flatCurryFileName progname)
          htime <- getModificationTime (docprogname++".html")
          if compareClockTime ctime htime == GT
           then return False
           else
            do putStrLn ("Copying doc file from "++docprogname++".html")
               system ("cp "++docprogname++".html "++docdir)
               system ("cp "++docprogname++"_curry.html "++docdir)
               return True

-----------------------------------------------------------------------
-- auxiliaries:

-- extract directory name from a path name:
getDirName n =
  let revdirname = dropWhile (/='/') (reverse n)
   in if revdirname=="" then "."
                        else reverse (tail revdirname)

-- read a list of FlatCurry modules together with all their imported modules
-- and return the lists of type, function, and operator declarations:
readFlatCurryWithImports :: [String] -> IO ([TypeDecl],[FuncDecl],[OpDecl])
readFlatCurryWithImports modules = collectMods modules []
 where
  collectMods [] _ = return ([],[],[])
  collectMods (m:ms) implist =
    if m `elem` implist
    then collectMods ms implist
    else
      do filename <- findFileInLoadPath (m++".fcy")
         (Prog _ imps types funs ops) <- readFlatCurryFile filename
         (ts,fs,os) <- collectMods (ms++imps) (m:implist)
         return (types++ts, funs++fs, ops++os)


-- add a directory name for a Curry source file by looking up the
-- current load path (CURRYPATH):
findSourceFileInLoadPath modname = do
  loadpath <- getLoadPathForFile modname
  mbfname <- lookupFileInPath (baseName modname) [".lcurry",".curry"] loadpath
  maybe (error ("Curry file for module \""++modname++"\" not found!"))
        (return . stripSuffix)
        mbfname

-----------------------------------------------------------------------
