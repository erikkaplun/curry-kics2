-----------------------------------------------------------------
--- A tool to create a simple makefile for compiling
--- a Curry application with kics2.
---
--- @author Michael Hanus
--- @version June 2011
-----------------------------------------------------------------

module CreateMakefile where

import FlatCurry
import FlatCurryRead
import List
import System
import Distribution
import FileGoodies

main = do
  args <- getArgs
  case args of
   [mainmod] -> createMake (stripSuffix mainmod) Nothing
   [mainmod,target] -> createMake (stripSuffix mainmod) (Just target)
   _ -> putStrLn $
    "ERROR: Illegal arguments: " ++
    concat (intersperse " " args) ++ "\n" ++
    "Usage: currycreatemake <main_module_name>\n" ++
    "   or: currycreatemake <main_module_name> <makefile>"

-- Create a simple makefile for a main module:
createMake :: String -> Maybe String -> IO ()
createMake mainmod target = do
  allints <- readFlatCurryIntWithImports mainmod
  let allmods = (foldl union [mainmod]
                       (map (\ (Prog _ imps _ _ _) -> imps) allints))
  allsources <- mapIO findSourceFileInLoadPath (filter (/="Prelude") allmods)
  (maybe putStr writeFile target)
     (showMake mainmod (map replacePakcsLib allsources))

showMake mainmod sourcefiles =
  "# Makefile for main module \""++mainmod++"\":\n\n"++
  "KICS2HOME="++installDir++"\n"++
  "KICS2LIB=${KICS2HOME}/lib\n\n"++
  ".PHONY: all\n"++
  "all: "++mainmod++"\n\n"++
  mainmod++": " ++ concat (intersperse " \\\n\t  " sourcefiles) ++"\n"++
  "\t# create saved state for top-level function \"main\":\n"++
  "\t${KICS2HOME}/bin/kics2 :l "++mainmod++" :save :q\n\n"++
  ".PHONY: clean\n"++
  "clean:\n\t${KICS2HOME}/bin/cleancurry\n"

-- add a directory name for a Curry source file by looking up the
-- current load path (CURRYPATH):
findSourceFileInLoadPath modname = do
  loadpath <- getLoadPathForFile modname
  mbfname <- lookupFileInPath (baseName modname) [".lcurry",".curry"] loadpath
  maybe (error ("Curry file for module \""++modname++"\" not found!"))
        (return . dropLocal)
        mbfname
 where
  dropLocal f = if take 2 f == "./" then drop 2 f else f

-- replace KICS2 lib directory prefix in a filename by ${KICS2LIB}:
replacePakcsLib filename =
  let pakcslib = installDir++"/lib"
      pllength = length pakcslib
   in if take pllength filename == pakcslib
      then "${KICS2LIB}" ++ drop pllength filename
      else filename
