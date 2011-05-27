
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
main = sortmain 10

isSorted :: [Int] -> Success
isSorted [] = success
isSorted [_] = success
isSorted (x:y:ys) = (x <= y) =:= True & isSorted (y:ys)

psort2 xs | isSorted xs' = xs'
  where xs' = perm xs

sortmain2 n = psort2 (2:[n,n-1 .. 3]++[1])

main2 = sortmain2 10
