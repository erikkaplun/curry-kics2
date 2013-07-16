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

unless :: Bool -> IO () -> IO ()
unless flag act = if flag then done else act

notNull :: [a] -> Bool
notNull = not . null

when :: Bool -> IO () -> IO ()
when flag act = if flag then act else done

--- Remove leading and trailing whitespace
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
