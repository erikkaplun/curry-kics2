-- Solve the 24-puzle:

data Exp = Num Int
         | Add Exp Exp
         | Mul Exp Exp
         | Sub Exp Exp
         | Div Exp Exp

test :: Exp -> [Int] -> Int
test (Num y) [z] | y =:= z = y
test (Add x y) l | split u v l = test x u + test y v where u, v free
test (Sub x y) l | split u v l = test x u - test y v where u, v free
test (Mul x y) l | split u v l = test x u * test y v where u, v free
test (Div x y) l | split u v l = opdvv (test x u) (test y v)
  where
    u, v free
    opdvv a b = if b == 0 || a `mod` b /= 0 then failed else a `div` b

-- split a list into two non-empty sublists
split (u:us) (v:vs) l | (u:us)++(v:vs) =:= l = success

-- compute (non-deterministically) a permutation
permute [] = []
permute (x:xs) | u++v =:= permute xs = u++[x]++v where u, v free

-- these are the digits which can be chosen:
problem = [2,3,6,8]


-- main call:
solve | test x (permute problem) =:= 24 = x where x free


-- show all solutions to the puzzle:
--allSolutions = getAllValues solve >>= putStr . unlines . map show

