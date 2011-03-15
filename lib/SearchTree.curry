------------------------------------------------------------------
--- This library defines a representation of a search space as
--- a tree and search strategies on this tree.
------------------------------------------------------------------

module SearchTree(SearchTree(..),searchTree,showSearchTree,
                  allValuesDFS,allValuesBFS)
  where

--- A search tree is a value, a failure, or a choice between to search trees.
data SearchTree a = Value a
                  | Fail
                  | Or (SearchTree a) (SearchTree a)

--- Returns the search tree for some expression.
searchTree :: a -> SearchTree a
searchTree external

--- Shows the search tree as an intended line structure
showSearchTree :: SearchTree a -> String
showSearchTree = showST 0
 where
   showST i (Value a)  = "Value: " ++ show a ++ "\n"
   showST i Fail       = "Fail\n"
   showST i (Or t1 t2) = "Or " ++ showST (i+3) t1 ++
                         tab (i+3) ++ showST (i+3) t2
   
   tab i = take i (repeat ' ')

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
