type Graph a = a -> a

succ :: Graph Int
succ 1 = 2
succ 1 = 3
succ 2 = 4
succ 3 = 4

inv :: Graph a -> Graph a
inv g a | a =:= g b = b where b free

undir :: Graph a -> Graph a
undir g a = g a
undir g a = inv g a

path :: Graph a -> a -> a -> [a]
path = path' [] where
  path' :: [a] -> Graph a -> a -> a -> [a]
  path' p g a b | a == b          = reverse (a:p)
                | (a `notElem` p) = path' (a:p) g (g a) b

main = print $ path succ 1 4
