----------------------------------------------------------------------
--- Some operations to handle to KiCS2 configuration file
--- that is stored in $HOME/.kics2rc
----------------------------------------------------------------------

module RCFile(readRC,rcValue,strip) where

import PropertyFile
import Directory(doesFileExist)
import Char(toLower,isSpace)
import System(system,getEnviron)
import Installation(installDir)

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
   then readPropertyFile rcname
   else do system ("cp "++installDir++"/kics2rc.default "++rcname)
           readPropertyFile rcname

--- Look up a configuration variable in the list of variables from the rc file.
--- Uppercase/lowercase is ignored for the variable names and the empty
--- string is returned for an undefined variable.
rcValue :: [(String,String)] -> String -> String
rcValue rcdefs var = strip $
   maybe "" id (lookup (map toLower var)
                       (map (\ (x,y) -> (map toLower x,y)) rcdefs))

strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
