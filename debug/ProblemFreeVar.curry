{-
nub [] [] = success
nub (x:xs) ys = nub xs zs & if elem x zs then ys =:= zs else ys =:= x:zs where zs free

idList2 [] [] = success
idList2 (x:xs) ys = ys =:= x:l & idList2 xs l -- where zs free
    where l = lists

idList4 [] [] = success
idList4 (x:xs) ys =  idList4 xs l & ys =:= x:l -- where zs free
    where l = lists

idList3 xs ys = xs =:= ys

lists = [] ? x:lists
  where x free
-}

-- idList []     ys = ys =:= []
idList [] [] = success
idList (x:xs) ys = ys =:= x:zs & idList xs zs where zs free
