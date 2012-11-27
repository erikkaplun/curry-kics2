-- A few auxiliary functions to formulate tests with random numbers.
module RandomTest(test,eq) where

import Random
import List(nub)
import Assertion

--- Tests a given predicate on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
test :: String -> ([Int]->Bool) -> Assertion (Maybe [Int])
test s f = assertIO s (rndList lenRnds >>= \xs -> return (if f xs then Nothing else Just xs)) Nothing

--- Tests whether two operations return equal results
--- on a list of distinct random numbers.
--- In case of a failure, the list of random numbers is returned
--- in order to see the test cases in the CurryTest tool.
eq :: String -> ([Int]->a) -> ([Int]->a) -> Assertion (Maybe [Int])
eq s f g = test s (\x -> (f x)==(g x))

--- generate a list of at most n random numbers (without duplicated elements)
rndList :: Int -> IO [Int]
rndList n = getRandomSeed >>= return . nub . take n . (flip nextIntRange 100000)

--- maximal length of test lists
lenRnds = 1000
