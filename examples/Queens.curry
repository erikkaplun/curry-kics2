-- import Benchmark

nsoln :: Int -> Int
nsoln x1 = length (gen x1 x1)

safe :: Int -> Int -> [Int] -> Bool
safe x1 x2 [] = True
safe x1 x2 (x4:x5) =
     (x1 /= x4) &&
     ((x1 /= (x4 + x2)) &&
     ((x1 /= (x4 - x2)) && (safe x1 (x2 + 1) x5)))

gen :: Int -> Int -> [[Int]]
gen x1 x2
 = case x2 == 0 of
    True -> [[]]
    False -> concatMap (gen'' x1) (gen x1 (x2 - 1))

gen'' :: Int -> [Int] -> [[Int]]
gen'' x1 x2
 = concatMap (gen' x2) (enumFromTo 1 x1)

gen' :: [Int] -> Int -> [[Int]]
gen' x1 x2
 = case safe x2 1 x1 of
    True -> [x2:x1]
    False -> []



goal0 = nsoln 10

{-
goal1 = nsoln 12


main_ = bench [mgoal0,mgoal1]

mgoal0 x = x=:=goal0
mgoal1 x = x=:=goal1

cmgoal0 = goal0
cmgoal1 = goal1
-}