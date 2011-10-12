-----------------------------------------------------------------
-- Get and set properties of the browser

module BrowserPropertyFile(getDotViewCmd,setDotViewCmd) where

import RCFile
import PropertyFile
import System
import Directory

-- Read dot view command from kics2rc file:
getDotViewCmd :: IO String
getDotViewCmd = do
  rcdefs <- readRC
  return (rcValue rcdefs "dotviewcommand")

-- Set dot view command in kics2rc file:
setDotViewCmd :: String -> IO ()
setDotViewCmd = setRCProperty "dotviewcommand"
