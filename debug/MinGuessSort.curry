-- some element of a list:
someOf (x:xs) = x ? someOf xs

-- min sort via guessing of some minimal element
sort :: [Int] -> [Int]
sort [] = []
sort (x:xs) | all (>=m) (x:xs) = m : sort rest
  where m = someOf (x:xs)
        rest = del m (x:xs)

del x (y:ys) = if x==y then ys
                       else y : del x ys

main n = length (sort [n, n-1 .. 1])

-- IDC run times:
-- n=7: 0.08
-- n=8: 0.72
-- n=9: 6.80
-- n=10: out of memory!
