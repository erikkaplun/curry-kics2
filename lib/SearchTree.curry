------------------------------------------------------------------
--- This library defines a representation of a search space as
--- a tree and various search strategies on this tree.
------------------------------------------------------------------

module SearchTree(SearchTree(..),someSearchTree, getSearchTree,
                  isDefined, showSearchTree, searchTreeSize,
                  allValuesDFS, allValuesBFS, allValuesIDS, allValuesIDSwith)
  where

--- A search tree is a value, a failure, or a choice between to search trees.
data SearchTree a = Value a
                  | Fail
                  | Or (SearchTree a) (SearchTree a)

--- Returns the search tree for some expression.
getSearchTree :: a -> IO (SearchTree a)
getSearchTree x = return (someSearchTree x)

--- Internal operation to return the search tree for some expression.
--- Note that this operation is not purely declarative since
--- the ordering in the resulting search tree depends on the
--- ordering of the program rules.
someSearchTree :: a -> SearchTree a
someSearchTree external

--- Returns True iff the argument is is defined, i.e., has a value.
isDefined :: a -> Bool
isDefined x = hasValue (someSearchTree x)
 where hasValue y = case y of Value _  -> True
                              Fail     -> False
                              Or t1 t2 -> hasValue t1 || hasValue t2

--- Shows the search tree as an intended line structure
showSearchTree :: SearchTree _ -> String
showSearchTree = showST 0
 where
   showST _ (Value a)  = "Value: " ++ show a ++ "\n"
   showST _ Fail       = "Fail\n"
   showST i (Or t1 t2) = "Or " ++ showST (i+3) t1 ++
                         tab (i+3) ++ showST (i+3) t2

   tab i = take i (repeat ' ')

--- Return the size (number of Value/Fail/Or nodes) of the search tree
searchTreeSize :: SearchTree _ -> (Int,Int,Int)
searchTreeSize (Value _)  = (1,0,0)
searchTreeSize Fail       = (0,1,0)
searchTreeSize (Or t1 t2) = let (v1,f1,o1) = searchTreeSize t1
                                (v2,f2,o2) = searchTreeSize t2
                             in (v1+v2,f1+f2,o1+o2+1)

--- Return all values in a search tree via depth-first search
allValuesDFS :: SearchTree a -> [a]
allValuesDFS Fail      = []
allValuesDFS (Value x) = [x]
allValuesDFS (Or x y)  = allValuesDFS x ++ allValuesDFS y

--- Return all values in a search tree via breadth-first search
allValuesBFS :: SearchTree a -> [a]
allValuesBFS t = allBFS [t]

children :: [SearchTree a] -> [SearchTree a]
children []           = []
children (Fail:ts)    = children ts
children (Value _:ts) = children ts
children (Or x y:ts)  = x:y:children ts

values :: [SearchTree a] -> [a]
values []           = []
values (Fail:ts)    = values ts
values (Value x:ts) = x : values ts
values (Or _ _:ts)  = values ts

allBFS :: [SearchTree a] -> [a]
allBFS []     = []
allBFS (t:ts) = values (t:ts) ++ allBFS (children (t:ts))


--- Return all values in a search tree via iterative-deepening search.
allValuesIDS :: SearchTree a -> [a]
allValuesIDS t = allValuesIDSwith 100 (2*) t

--- Return all values in a search tree via iterative-deepening search.
--- The first argument is the initial depth bound and
--- the second argument is a function to increase the depth in each
--- iteration.
allValuesIDSwith :: Int -> (Int -> Int) -> SearchTree a -> [a]
allValuesIDSwith initdepth incrdepth st =
  iterIDS initdepth (collectInBounds 0 initdepth st)
 where
  iterIDS _ Nil = []
  iterIDS n (Cons x xs) = x : iterIDS n xs
  iterIDS n Abort = let newdepth = incrdepth n
                     in iterIDS newdepth (collectInBounds n newdepth st)

-- Collect solutions within some level bounds in a tree.
collectInBounds :: Int -> Int -> SearchTree a -> AbortList a
collectInBounds oldbound newbound st = collectLevel newbound st
 where
  collectLevel _ Fail      = Nil
  collectLevel d (Value x) = if d<=newbound-oldbound then Cons x Nil else Nil
  collectLevel d (Or x y)  =
    if d>0 then concA (collectLevel (d-1) x) (collectLevel (d-1) y)
           else Abort

-- List containing "aborts" are used to implement the iterative
-- depeening strategy:

data AbortList a = Nil | Cons a (AbortList a) | Abort

-- Concatenation on abort lists where aborts are moved to the right.
concA :: AbortList a -> AbortList a -> AbortList a
concA Abort       Abort = Abort
concA Abort       Nil = Abort
concA Abort       (Cons x xs) = Cons x (concA Abort xs)
concA Nil         ys = ys
concA (Cons x xs) ys = Cons x (concA xs ys)
