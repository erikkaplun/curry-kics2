--- Computing strongly connected components
---
--- Copyright (c) 2000 - 2003, Wolfgang Lux
--- See LICENSE for the full license.
---
--- At various places in the compiler we had to partition a list of
--- declarations into strongly connected components. The function
--- \texttt{scc} computes this relation in two steps. First, the list is
--- topologically sorted ``downwards'' using the \emph{defs} relation.
--- Then the resulting list is sorted ``upwards'' using the \emph{uses}
--- relation and partitioned into the connected components. Both relations
--- are computed within this module using the bound and free names of each
--- declaration.
---
--- In order to avoid useless recomputations, the code in the module first
--- decorates the declarations with their bound and free names and a
--- unique number. The latter is only used to provide a trivial ordering
--- so that the declarations can be used as set elements.

module SCC (scc) where

import SetRBT (emptySetRBT, elemRBT, insertRBT)

data Node a b = Node Int [b] [b] a

cmpNode :: Node a b -> Node a b -> Bool
cmpNode n1 n2 = key n1 < key n2

key :: Node a b -> Int
key (Node k _ _ _) = k

bvs :: Node a b -> [b]
bvs (Node _ bs _ _) = bs

fvs :: Node a b -> [b]
fvs (Node _ _ fs _) = fs

node :: Node a b -> a
node (Node _ _ _ n) = n

scc :: (a -> [b]) -- ^ entities defined by node
    -> (a -> [b]) -- ^ entities used by node
    -> [a]        -- ^ list of nodes
    -> [[a]]      -- ^ strongly connected components
scc bvs' fvs' = map (map node) . tsort' . tsort . zipWith wrap [0 ..]
  where wrap i n = Node i (bvs' n) (fvs' n) n

tsort :: [Node a b] -> [Node a b]
tsort xs = snd (dfs xs (emptySetRBT cmpNode) [])
  where
  dfs []        marks stack = (marks, stack)
  dfs (x : xs') marks stack
    | x `elemRBT` marks = dfs xs' marks stack
    | otherwise         = dfs xs' marks' (x : stack')
    where
    (marks', stack') = dfs (defs x) (x `insertRBT` marks) stack
    defs x1          = filter (any (`elem` fvs x1) . bvs) xs

tsort' :: [Node a b] -> [[Node a b]]
tsort' xs = snd (dfs xs (emptySetRBT cmpNode) [])
  where
  dfs []        marks stack = (marks, stack)
  dfs (x : xs') marks stack
    | x `elemRBT` marks = dfs xs' marks stack
    | otherwise         = dfs xs' marks' ((x : concat stack') : stack)
    where
    (marks', stack') = dfs (uses x) (x `insertRBT` marks) []
    uses x1          = filter (any (`elem` bvs x1) . fvs) xs
