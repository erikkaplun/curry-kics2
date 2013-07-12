--- --------------------------------------------------------------------------
--- The main module for KiCS2c the ID based Curry to Haskell compiler
---
--- @author  Bernd Brassel, Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version July 2012
--- --------------------------------------------------------------------------
module Compile where

import Char             (isSpace)
import Maybe            (fromJust)
import List             (isPrefixOf)
import Directory        (doesFileExist)
import FilePath         ((</>), dropExtension, normalise)
import FileGoodies      (lookupFileInPath)
import FiniteMap
import FlatCurry
import FlatCurryGoodies (updQNamesInProg)
import ReadShowTerm     (readQTermFile)

import AnnotatedFlatCurryGoodies (unAnnProg)

import qualified AbstractHaskell as AH
import qualified AbstractHaskellGoodies as AHG (funcName, typeOf)
import AbstractHaskellPrinter (showModuleHeader, showDecls)
import CompilerOpts
import Files
  ( withBaseName, withDirectory, withExtension
  , writeFileInDir, writeQTermFileInDir
  )
import FlatCurry2AbstractHaskell (fcy2abs)
import LiftCase (liftCases)
import EliminateCond (eliminateCond)
import DefaultPolymorphic (defaultPolymorphic)
import Inference (inferProgFromProgEnv)
import Message (putErrLn, showStatus, showDetail) --, showAnalysis)
import ModuleDeps (ModuleIdent, Source, deps)
import Names
import SimpleMake
-- import Splits (mkSplits)
import TransFunctions
import TransTypes
import Utils (when, foldIO, notNull)

--- Parse the command-line arguments and build the specified files
main :: IO ()
main = do
  (opts, files) <- compilerOpts
  mapIO_ (build opts) files

--- Load the module, resolve the dependencies and compile the source files
--- if necessary
build :: Options -> String -> IO ()
build opts fn = do
  mbFn <- locateCurryFile fn
  case mbFn of
    Nothing -> putErrLn $ "Could not find file " ++ fn
    Just f -> do
      (mods, errs) <- deps opts f
      if null errs
        then Utils.foldIO (makeModule mods) initState (zip mods [1 .. ]) >> done
        else mapIO_ putErrLn errs
        where initState = { compOptions := opts | defaultState }


--- Checks if the given string corresponds to a Curry-File and
--- returns the actual file path
--- @param fn - the (relative) path to the Curry file with or without extension
--- @return `Just path` if the module was found, `Nothing` if not
locateCurryFile :: String -> IO (Maybe String)
locateCurryFile fn = do
  exists <- doesFileExist fn
  if exists
    then return (Just fn)
    else lookupFileInPath fn [".curry", ".lcurry"] ["."]


makeModule :: [(ModuleIdent, Source)] -> State -> ((ModuleIdent, Source), Int)
           -> IO State
makeModule mods state mod@((_, (fn, fcy)), _)
  | opts :> optForce  = compileModule progs modCnt state mod
  | otherwise         = do
                        depFiles <- getDepFiles
                        smake (destFile (opts :> optOutputSubdir) fn)
                              depFiles
                              (compileModule progs modCnt state mod)
                              (loadAnalysis modCnt state mod)
  where
    getDepFiles = do
      hasExternals <- doesFileExist extFile
      let ownModule = fn : if hasExternals then [extFile] else []
      let imported  = map (\i -> destFile (opts :> optOutputSubdir)
                               $ fst $ fromJust $ lookup i mods) imps
      return $ ownModule ++ imported
    extFile = externalFile fn
    (Prog _ imps _ _ _) = fcy
    modCnt = length mods
    progs = [ (m, p) | (m, (_, p)) <- mods]
    opts = state :> compOptions

storeAnalysis :: State -> AnalysisResult -> String -> IO ()
storeAnalysis state (types, ndAna, hoFunAna, hoConsAna) fn = do
  showDetail opts $ "Writing Analysis file " ++ ndaFile
  writeQTermFileInDir ndaFile (ndAnaStr, hoFuncAnaStr, hoConsAnaStr, typesStr)
    where
      opts         = state :> compOptions
      ndaFile      = analysisFile (opts :> optOutputSubdir) fn
      ndAnaStr     = showFM ndAna
      hoFuncAnaStr = showFM hoFunAna
      hoConsAnaStr = showFM hoConsAna
      typesStr     = showFM types

loadAnalysis :: Int -> State -> ((ModuleIdent, Source), Int) -> IO State
loadAnalysis total state ((mid, (fn, _)), current) = do
  showStatus opts $ compMessage current total ("Analyzing " ++ mid) fn ndaFile
  (ndAnalysis, hoFuncAnalysis, hoConsAnalysis, types) <- readQTermFile ndaFile
  return { ndResult     := (state :> ndResult    ) `plusFM` readFM (<) ndAnalysis
         , hoResultFun  := (state :> hoResultFun ) `plusFM` readFM (<) hoFuncAnalysis
         , hoResultCons := (state :> hoResultCons) `plusFM` readFM (<) hoConsAnalysis
         , typeMap      := (state :> typeMap     ) `plusFM` readFM (<) types
         | state }
    where
      ndaFile = analysisFile (opts :> optOutputSubdir) fn
      opts = state :> compOptions

compileModule :: [(ModuleIdent, Prog)] -> Int -> State
              -> ((ModuleIdent, Source), Int) -> IO State
compileModule progs total state ((mid, (fn, fcy)), current) = do
  showStatus opts $ compMessage current total ("Compiling " ++ mid) fn destination

  let fcy' = filterPrelude opts fcy
  dump DumpFlat opts fcyName (show fcy')

  showDetail opts "Inferring types"
  let afcy = either error id (inferProgFromProgEnv progs fcy)
  dump DumpTypedFlat opts typedName (show fcy')

  showDetail opts "Lifting case expressions"
  let pLifted = liftCases True afcy
  dump DumpLifted opts liftedName (show pLifted)

  showDetail opts "Eliminate calls to cond"
  let pElim = eliminateCond pLifted
  dump DumpEliminated opts elimName (show pElim)

  showDetail opts "Default locally polymorphic sub-expressions"
  let pDefaulted = defaultPolymorphic pElim
  dump DumpDefaulted opts defaultedName (show pDefaulted)

  showDetail opts "Renaming symbols"
  let renamed@(Prog _ _ ts _ _)  = rename (unAnnProg pDefaulted)
  dump DumpRenamed opts renamedName (show renamed)

  showDetail opts "Transforming functions"
  ((tProg,modAnalysisResult), state') <- unM (transProg renamed) state
  storeAnalysis state' modAnalysisResult fn
  let ahsFun@(AH.Prog n imps _ funs ops) = fcy2abs tProg
  dump DumpFunDecls opts funDeclName (show ahsFun)

  showDetail opts "Transforming type declarations"
  let typeDecls = transTypes (state' :> hoResultCons) ts
  dump DumpTypeDecls opts typeDeclName (show typeDecls)

  showDetail opts "Combining to Abstract Haskell"
  let ahs = (AH.Prog n (defaultModules ++ imps) typeDecls funs ops)

  -- TODO: HACK: manually patch export of type class curry into Prelude
  let ahsPatched = patchCurryTypeClassIntoPrelude ahs
  dump DumpAbstractHs opts abstractHsName (show ahsPatched)

  showDetail opts "Integrating external declarations"
  integrated <- integrateExternals opts ahsPatched fn

  showDetail opts $ "Generating Haskell module " ++ destination
  writeFileInDir destination integrated

  showDetail opts $ "Writing auxiliary info file " ++ funcInfo
  writeQTermFileInDir funcInfo (extractFuncInfos funs)

  showDetail opts $ "Done"
  return state'

    where
    fcyName        = fcyFile $ withBaseName (++ "Dump")      mid
    typedName      = fcyFile $ withBaseName (++ "Typed")     mid
    liftedName     = fcyFile $ withBaseName (++ "Lifted")    mid
    elimName       = fcyFile $ withBaseName (++ "ElimCond")  mid
    defaultedName  = fcyFile $ withBaseName (++ "Defaulted") mid
    renamedName    = fcyFile $ withBaseName (++ "Renamed")   mid
    funDeclName    = ahsFile $ withBaseName (++ "FunDecls")  mid
    typeDeclName   = ahsFile $ withBaseName (++ "TypeDecls") mid
    abstractHsName = ahsFile mid
    destination    = destFile (opts :> optOutputSubdir) fn
    funcInfo       = funcInfoFile (opts :> optOutputSubdir) fn
    opts           = state :> compOptions
    fcyFile f = withExtension (const ".fcy") f
    ahsFile f = withExtension (const ".ahs") f

-- Extract some basic information (deterministic, IO) about all functions
extractFuncInfos funs =
  map (\fd -> (AHG.funcName fd, isIO (AHG.typeOf fd))) funs
 where
  isIO AH.Untyped = False
  isIO (AH.FType texp) = withIOResult texp
  isIO (AH.CType _ texp) = withIOResult texp

  withIOResult (AH.TVar _) = False
  withIOResult (AH.FuncType _ texp) = withIOResult texp
  withIOResult (AH.TCons tc _) = tc == (curryPrelude, "C_IO")

-- Patch Prelude in order to add some exports for predefined items
patchCurryTypeClassIntoPrelude :: AH.Prog -> AH.Prog
patchCurryTypeClassIntoPrelude p@(AH.Prog m imps td fd od)
  | m == curryPrelude = AH.Prog m imps (curryDecl:td) fd od
  | otherwise         = p
 where
  curryDecl = AH.Type (curryPrelude, "Curry") AH.Public [] []

compMessage :: Int -> Int -> String -> String -> String -> String
compMessage curNum maxNum msg fn dest
  =  '[' : fill max sCurNum ++ " of " ++ sMaxNum  ++ "]"
  ++ ' ' : msg  ++ " ( " ++ normalise fn ++ ", " ++ normalise dest ++ " )"
    where
      sCurNum = show curNum
      sMaxNum = show maxNum
      max = length $ sMaxNum
      fill n s = replicate (n - length s) ' ' ++ s

filterPrelude :: Options -> Prog -> Prog
filterPrelude opts p@(Prog m imps td fd od)
  | noPrelude = Prog m (filter (/= prelude) imps) td fd od
  | otherwise = p
  where noPrelude = ExtNoImplicitPrelude `elem` opts :> optExtensions

--
integrateExternals :: Options -> AH.Prog -> String -> IO String
integrateExternals opts (AH.Prog m imps td fd od) fn = do
  exts <- lookupExternals opts (dropExtension fn)
  let (pragmas, extimps, extdecls) = splitExternals exts
  return $ unlines $ filter notNull
    [ unlines (defaultPragmas : pragmas)
    , showModuleHeader m td fd imps
    , unlines extimps
    , showDecls m od td fd
    , unlines extdecls
    ]
 where
  defaultPragmas :: String
  defaultPragmas =
      "{-# LANGUAGE MagicHash #-}\n"
   ++ "{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}"


-- lookup an external file for a module and return either the content or an
-- empty String
lookupExternals :: Options -> String -> IO String
lookupExternals opts fn = do
  exists <- doesFileExist extName
  if exists
    then showDetail opts    "External file found" >> readFile extName
    else showDetail opts "No External file found" >> return ""
    where extName = externalFile fn

-- Split an external file into a pragma String, a list of imports and the rest
-- TODO: This is a bloody hack
splitExternals :: String -> ([String], [String], [String])
splitExternals content = (pragmas, imports, decls)
  where
  (pragmas, rest ) = span isPragma (lines content)
  (imports, decls) = span isImport rest
  isPragma line    = all isSpace line || "{-#"    `isPrefixOf` line
  isImport line    = all isSpace line || "import" `isPrefixOf` line
                                      || "#"      `isPrefixOf` line
                                      || isComment line
  isComment line   = "-- " `isPrefixOf` line
                     && not ("-- #endimport" `isPrefixOf` line)

-- Dump an intermediate result to a file
dump :: DumpFormat -> Options -> String -> String -> IO ()
dump format opts file src = when (format `elem` opts :> optDump) $ do
  showDetail opts $ "Dumping " ++ file
  writeFileInDir (withDirectory (</> opts :> optOutputSubdir) file) src

rename :: Prog -> Prog
rename p@(Prog name imports _ _ _) =
  Prog (renameModule name) (map renameModule imports) td fd od where
  (Prog _ _ td fd od) = updQNamesInProg renameQName p
