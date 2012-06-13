--- --------------------------------------------------------------------------
--- Various utility functions
---
--- This module should be further divided if it contains too much unrelated
--- things.
---
--- @author  Bjoen Peemoeller
--- @version April 2011
--- --------------------------------------------------------------------------
module Utils where

import List (intersperse)

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

intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, c) = (f a, c)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (c, a) = (c, f a)

scanl :: (a -> b -> a) -> a -> [b] -> [a]
scanl f q ls =  q : (case ls of
  []   -> []
  x:xs -> scanl f (f q x) xs)

scanl1 :: (a -> a -> a) -> [a] -> [a]
scanl1 f (x:xs) =  scanl f x xs
scanl1 _ []     =  []
