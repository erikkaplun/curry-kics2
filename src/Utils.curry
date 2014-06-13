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

--- Extend a String to a given minimal length by adding *leading* spaces.
lpad :: Int -> String -> String
lpad n s = replicate (n - length s) ' ' ++ s

--- Extend a String to a given minimal length by adding *trailing* spaces.
rpad :: Int -> String -> String
rpad n s = s ++ replicate (n - length s) ' '
