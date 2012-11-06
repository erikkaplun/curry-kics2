-- A few auxiliary functions to formulate tests with random numbers.
module RandomTest where

import Random
import List(nub)
import Assertion

--- generate a list of at most n random numbers (without duplicated elements)
rndList :: Int -> IO [Int]
rndList n = getRandomSeed >>= return . take n . nub . (flip nextIntRange 100000)

--- Tests a given predicate on a list of random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
test :: String -> ([Int]->Bool) -> Assertion (Maybe [Int])
test s f = assertIO s (rndList lenRnds >>= \xs -> return (if f xs then Nothing else Just xs)) Nothing

--- test equality on random list
eq :: String -> ([Int]->a) -> ([Int]->a) -> Assertion (Maybe [Int])
eq s f g = test s (\x -> (f x)==(g x))

--- length of test lists
lenRnds = 1000
