----------------------------------------------------------------------
--- Some operations to handle the KiCS2 resource configuration file
--- that is stored in $HOME/.kics2rc
---
--- @author  Michael Hanus
--- @version January 2013
----------------------------------------------------------------------

module RCFile (readRC, rcValue, setRCProperty) where

import Char         (toLower, isSpace)
import Directory    (getHomeDirectory, doesFileExist, copyFile, renameFile)
import FilePath     (FilePath, (</>), (<.>))
import Function     (first)
import Installation (installDir)
import PropertyFile
import Sort         (mergeSort)

import Utils        (strip)

defaultRC :: FilePath
defaultRC = installDir </> "kics2rc.default"

--- Location of the rc file of a user.
--- After bootstrapping, one can also use Distribution.rcFileName
--- The name of the file specifying configuration parameters of the
--- current distribution. This file must have the usual format of
--- property files (see description in module PropertyFile).
rcFileName :: IO FilePath
rcFileName = (</> ".kics2rc") `liftIO` getHomeDirectory

--- Reads the rc file. If it is not present, the standard file
--- from the distribution will be copied.
readRC :: IO [(String, String)]
readRC = do
  rcName   <- rcFileName
  rcExists <- doesFileExist rcName
  if rcExists then updateRC else copyFile defaultRC rcName
  readPropertyFile rcName

rcKeys :: [(String, String)] -> [String]
rcKeys = mergeSort (<=) . map fst

--- Reads the rc file (which must be present) and compares the definitions
--- with the distribution rc file. If the set of variables is different,
--- update the rc file with the distribution but keep the user's definitions.
updateRC :: IO ()
updateRC = do
  rcName    <- rcFileName
  userprops <- readPropertyFile rcName
  distprops <- readPropertyFile defaultRC
  unless (rcKeys userprops == rcKeys distprops) $ do
    putStrLn $ "Updating \"" ++ rcName ++ "\"..."
    renameFile rcName $ rcName <.> "bak"
    copyFile defaultRC rcName
    mapIO_ (\ (n, v) -> maybe done
              (\uv -> unless (uv == v) $ updatePropertyFile rcName n uv)
              (lookup n userprops))
           distprops

--- Sets a property in the rc file.
setRCProperty :: String -> String -> IO ()
setRCProperty pname pval = do
  readRC -- just be to sure that rc file exists and is up-to-date
  rcName <- rcFileName
  updatePropertyFile rcName pname pval

--- Look up a configuration variable in the list of variables from the rc file.
--- Uppercase/lowercase is ignored for the variable names and the empty
--- string is returned for an undefined variable.
rcValue :: [(String, String)] -> String -> String
rcValue rcdefs var = strip $ maybe "" id $
  lookup (map toLower var) (map (first (map toLower)) rcdefs)
