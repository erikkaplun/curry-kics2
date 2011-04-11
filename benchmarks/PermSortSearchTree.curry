import SearchTree

insert x [] = [x]
insert x (y:ys) = x:y:ys ? y : (insert x ys)

perm [] = []
perm (x:xs) = insert x (perm xs)

sorted :: [Int] -> [Int]
sorted []       = []
sorted [x]      = [x]
sorted (x:y:ys) | x <= y = x : sorted (y:ys)

psort xs = sorted (perm xs)

sortmain n = psort (2:[n,n-1 .. 3]++[1])

--main = sortmain 15

goal n = searchTree (sortmain n) >>= return . allValuesDFS

--main = goal 15

-- print search tree for perm or permsort:
prpermst n = searchTree (perm [1..n]) >>= putStrLn . showSearchTree 
prsortst n = searchTree (psort [n,n-1 .. 1]) >>= putStrLn  . showSearchTree

