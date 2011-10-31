------------------------------------------------------------------------------
--- Library to process markdown documents with pandoc.
---
--- @author Michael Hanus
--- @version October 2011
------------------------------------------------------------------------------

module Pandoc
 where

import IO
import IOExts
import System(system,getPID)
import Directory(removeFile)

-- The executable of pandoc:
pandocCmd = "pandoc"

--- Transforms a string in markdown format into a string in HTML format.
markdown2html :: String -> IO String
markdown2html = transformMarkdown "html"

--- Transforms a string in markdown format into a string in LaTeX format.
markdown2latex :: String -> IO String
markdown2latex = transformMarkdown "latex"

--- Transforms a string in markdown format into a string in a target format.
transformMarkdown :: String -> String -> IO String
transformMarkdown targetformat mks = do
  pid <- getPID
  let tmpmd   = "tmp_pandoc_"++show pid++".txt"
      tmphtml = "tmp_pandoc_"++show pid++".html"
  writeFile tmpmd mks
  system $ pandocCmd++" -t "++targetformat++" "++tmpmd++" > "++tmphtml
  output <- readCompleteFile tmphtml
  removeFile tmpmd
  removeFile tmphtml
  return output
{-
transformMarkdown targetformat mks = do
  (hin,hout,herr) <- execCmd (pandocCmd++" -t "++targetformat)
  hPutStr hin mks
  hFlush hin
  hClose hin
  output <- hGetContents hout
  hClose hout
  hClose herr
  return output
-}
