----------------------------------------------------------------------
--- Some operations to handle to KiCS2 configuration file
--- that is stored in $HOME/.kics2rc
----------------------------------------------------------------------

module RCFile(readRC,rcValue,setRCProperty,strip) where

import PropertyFile
import Directory(doesFileExist,renameFile)
import Char(toLower,isSpace)
import System(system,getEnviron)
import Installation(installDir)
import Sort(mergeSort)

--- Location of the rc file of a user.
--- After bootstrapping, one can also use Distribution.rcFileName
--- The name of the file specifying configuration parameters of the
--- current distribution. This file must have the usual format of
--- property files (see description in module PropertyFile).
rcFileName :: IO String
rcFileName = getEnviron "HOME" >>= return . (++"/.kics2rc")

--- Reads the rc file. If it is not present, the standard file
--- from the distribution will be copied.
readRC :: IO [(String,String)]
readRC = do
  rcname   <- rcFileName
  rcexists <- doesFileExist rcname
  if rcexists
   then updateRC >> readPropertyFile rcname
   else do system ("cp "++installDir++"/kics2rc.default "++rcname)
           readPropertyFile rcname

--- Reads the rc file (which must be present) and compares the definitions
--- with the distribution rc file. If the set of variables is different,
--- update the rc file with the distribution but keep the user's definitions.
updateRC :: IO ()
updateRC = do
  let distrcname = installDir++"/kics2rc.default"
  rcname   <- rcFileName
  userprops <- readPropertyFile rcname  
  distprops <- readPropertyFile distrcname
  if mergeSort (<=) (map fst userprops) == mergeSort (<=) (map fst distprops)
   then done
   else do putStrLn $ "Updating \""++rcname++"\"..."
           renameFile rcname (rcname++".bak")
           system ("cp "++installDir++"/kics2rc.default "++rcname)
           mapIO_ (\ (n,v) -> maybe done
                                    (\uv -> if uv==v then done else
                                            updatePropertyFile rcname n uv)
                                    (lookup n userprops))
                  distprops

--- Sets a property in the rc file.
setRCProperty :: String -> String -> IO ()
setRCProperty pname pval = do
  readRC -- just be to sure that rc file exists and is up-to-date
  rcname   <- rcFileName
  updatePropertyFile rcname pname pval

--- Look up a configuration variable in the list of variables from the rc file.
--- Uppercase/lowercase is ignored for the variable names and the empty
--- string is returned for an undefined variable.
rcValue :: [(String,String)] -> String -> String
rcValue rcdefs var = strip $
   maybe "" id (lookup (map toLower var)
                       (map (\ (x,y) -> (map toLower x,y)) rcdefs))

strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
