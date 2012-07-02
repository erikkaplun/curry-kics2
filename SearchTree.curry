------------------------------------------------------------------------------
--- This library defines a representation of a search space as
--- a tree and various search strategies on this tree.
---
--- @author  Michael Hanus, Bjoern Peemoeller, Fabian Reck
--- @version March 2012
------------------------------------------------------------------------------

module SearchTree
  ( SearchTree (..), someSearchTree, getSearchTree
  , isDefined, showSearchTree, searchTreeSize
  , allValuesDFS, allValuesBFS, allValuesIDS, allValuesIDSwith
  ) where

--- A search tree is a value, a failure, or a choice between to search trees.
data SearchTree a = Value a
                  | Fail Int
                  | Or (SearchTree a) (SearchTree a)

-- A value sequence is a sequence of values that
-- implements the semantics of set functions w.r.t. failures
data ValueSequence _ -- external

emptyVS :: ValueSequence a
emptyVS external

addVS :: a -> ValueSequence a -> ValueSequence a
addVS external

failVS :: Int -> ValueSequence a
failVS external

vsToList :: ValueSequence a -> [a]
vsToList external

(|++|) :: ValueSequence a -> ValueSequence a -> ValueSequence a
(|++|) external

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
                              Fail _   -> False
                              Or t1 t2 -> hasValue t1 || hasValue t2

--- Shows the search tree as an intended line structure
showSearchTree :: SearchTree _ -> String
showSearchTree st = showsST [] st ""
 where
  -- `showsST ctxt <SearchTree>`, where `ctxt` is a stack of boolean flags
  -- indicating whether we show the last alternative of the respective
  -- level to enable drawing aesthetical corners
  showsST ctxt (Value  a) = indent ctxt . shows a      . nl
  showsST ctxt (Fail _)   = indent ctxt . showChar '!' . nl
  showsST ctxt (Or t1 t2) = indent ctxt . showChar '?' . nl
                          . showsST (False : ctxt) t1
                          . showsST (True  : ctxt) t2

  indent []     = id
  indent (i:is) = showString (concatMap showIndent $ reverse is)
                . showChar   (if i then llc else lmc)
                . showString (hbar : " ")
    where showIndent isLast = (if isLast then ' ' else vbar) : "  "

  vbar = '\x2502' -- vertical bar
  hbar = '\x2500' -- horizontal bar
  llc  = '\x2514' -- left lower corner
  lmc  = '\x251c' -- left middle corner

  nl           = showChar '\n'
  shows x      = showString (show x)
  showChar c   = (c:)
  showString s = (s++)

-- showSearchTree st = showST 0 st ""
--  where
--   showST _ (Value a)  = showString "Value: " . shows a . nl
--   showST _ Fail       = showString "Fail"    . nl
--   showST i (Or t1 t2) = showString "Or "
--                       . showST i' t1 . tab i' . showST i' t2
--     where i'    = i + 1
--           tab j = showString $ replicate (3 * j) ' '


--- Return the size (number of Value/Fail/Or nodes) of the search tree
searchTreeSize :: SearchTree _ -> (Int, Int, Int)
searchTreeSize (Value _)  = (1, 0, 0)
searchTreeSize (Fail _)   = (0, 1, 0)
searchTreeSize (Or t1 t2) = let (v1, f1, o1) = searchTreeSize t1
                                (v2, f2, o2) = searchTreeSize t2
                             in (v1 + v2, f1 + f2, o1 + o2 + 1)

--- Return all values in a search tree via depth-first search
allValuesDFS :: SearchTree a -> [a]
allValuesDFS = vsToList . allValuesDFS' 

allValuesDFS' :: SearchTree a -> ValueSequence a
allValuesDFS' (Fail d)  = failVS d
allValuesDFS' (Value x) = addVS x emptyVS
allValuesDFS' (Or x y)  = allValuesDFS' x |++| allValuesDFS' y

--- Return all values in a search tree via breadth-first search
allValuesBFS :: SearchTree a -> [a]
allValuesBFS t = vsToList (allBFS [t])

children :: [SearchTree a] -> [SearchTree a]
children []             = []
children (Fail _:ts)    = children ts
children (Value _:ts)   = children ts
children (Or x y:ts)    = x:y:children ts

values :: [SearchTree a] -> ValueSequence a
values []           = emptyVS
values (Fail d:ts)  = failVS d |++| values ts
values (Value x:ts) = addVS x (values ts)
values (Or _ _:ts)  = values ts

allBFS :: [SearchTree a] -> ValueSequence a
allBFS []     = emptyVS
allBFS (t:ts) = values (t:ts) |++| allBFS (children (t:ts))


--- Return all values in a search tree via iterative-deepening search.
allValuesIDS :: SearchTree a -> [a]
allValuesIDS t = vsToList (allValuesIDSwith 100 (2*) t)

--- Return all values in a search tree via iterative-deepening search.
--- The first argument is the initial depth bound and
--- the second argument is a function to increase the depth in each
--- iteration.
allValuesIDSwith :: Int -> (Int -> Int) -> SearchTree a -> ValueSequence a
allValuesIDSwith initdepth incrdepth st =
  iterIDS initdepth (collectInBounds 0 initdepth st)
 where
  iterIDS _ Nil = emptyVS
  iterIDS n (Cons x xs) = addVS x (iterIDS n xs)
  iterIDS n (FCons fd xs) = failVS fd |++| iterIDS n xs
  iterIDS n Abort = let newdepth = incrdepth n
                     in iterIDS newdepth (collectInBounds n newdepth st)

-- Collect solutions within some level bounds in a tree.
collectInBounds :: Int -> Int -> SearchTree a -> AbortList a
collectInBounds oldbound newbound st = collectLevel newbound st
 where
  collectLevel d (Fail fd)  = if d <=newbound-oldbound then FCons fd Nil else Nil
  collectLevel d (Value x) = if d<=newbound-oldbound then Cons x Nil else Nil
  collectLevel d (Or x y)  =
    if d>0 then concA (collectLevel (d-1) x) (collectLevel (d-1) y)
           else Abort

-- List containing "aborts" are used to implement the iterative
-- depeening strategy:

data AbortList a = Nil | Cons a (AbortList a) | FCons Int (AbortList a) | Abort

-- Concatenation on abort lists where aborts are moved to the right.
concA :: AbortList a -> AbortList a -> AbortList a
concA Abort       Abort = Abort
concA Abort       Nil = Abort
concA Abort       (Cons x xs) = Cons x (concA Abort xs)
concA Abort       (FCons d xs) = FCons d (concA Abort xs)
concA Nil         ys = ys
concA (Cons x xs) ys = Cons x (concA xs ys)
concA (FCons d xs) ys = FCons d (concA xs ys)
