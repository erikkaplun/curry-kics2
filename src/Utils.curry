--- --------------------------------------------------------------------------
--- Various utility functions
---
--- This module should be further divided if it contains too much unrelated
--- things.
---
--- @author  Bjoern Peemoeller
--- @version July 2013
--- --------------------------------------------------------------------------
module Utils where

import Char (isSpace)

notNull :: [a] -> Bool
notNull = not . null

--- Remove leading and trailing whitespace
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
