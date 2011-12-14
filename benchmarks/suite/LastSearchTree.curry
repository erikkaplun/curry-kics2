import SearchTree

last :: [a] -> a
last l | xs ++ [x] =:= l = x where xs,x free

goal0 = last (replicate 10000 True)
goal1 = last (replicate 100000 True)

main = goal1

mainDFS = allValuesDFS (someSearchTree main)

mainBFS = allValuesBFS (someSearchTree main)

mainIDS = allValuesIDS (someSearchTree main)
