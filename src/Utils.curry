--- --------------------------------------------------------------------------
--- Various utility functions
---
--- This module should be further divided if it contains too much unrelated
--- things.
---
--- @author  Bjoern Peemoeller
--- @version December 2012
--- --------------------------------------------------------------------------
module Utils where

import Char (isSpace)

foldIO :: (a -> b -> IO a) -> a -> [b] -> IO a
foldIO _ a []      =  return a
foldIO f a (x:xs)  =  f a x >>= \fax -> foldIO f fax xs

liftIO :: (a -> b) -> IO a -> IO b
liftIO f m = m >>= return . f

unless :: Bool -> IO () -> IO ()
unless flag act = if flag then done else act

notNull :: [a] -> Bool
notNull = not . null

when :: Bool -> IO () -> IO ()
when flag act = if flag then act else done

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

--- Remove leading and trailing whitespace
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace
