--- Change in a binary tree all leaves to the maximum value of all leaves
--- (in one pass!)


max :: Int -> Int -> Int
max x y = if x>y then x else y


data BTree a = Leaf a | Node (BTree a) (BTree a)

passTree :: BTree Int -> Int -> (BTree Int, Int)
passTree (Leaf n) mx = (Leaf mx, n)
passTree (Node t1 t2) mx
 | (mt1,m1) =:= passTree t1 mx  &  (mt2,m2) =:= passTree t2 mx
  = (Node mt1 mt2, max m1 m2)
 where mt1,mt2,m1,m2 free

maxTree :: BTree Int -> BTree Int
maxTree t | (mt,mx) =:= passTree t mx  = mt  where mt,mx free

--- goals:
goal1 = maxTree (Node (Leaf 0)
                      (Node (Leaf 1)
                            (Leaf 2)))
--> Node (Leaf 2) (Node (Leaf 2) (Leaf 2))

goal2 = maxTree (Node (Node (Leaf 1)
                            (Leaf 0))
                      (Node (Leaf 3)
                            (Leaf 2)))
--> Node (Node (Leaf 3) (Leaf 3)) (Node (Leaf 3) (Leaf 3))
