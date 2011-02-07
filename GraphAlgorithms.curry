--- Basic Graph Algorithms
---
--- Porting of a haskell library (c) 1999 - 2002 by Martin Erwig
---
--- The library contains mainly of functions, based on depth first search (dfs).
---
---  Classification of all 32 dfs functions:
---
---       dfs-function ::= [direction]"df"structure["With"]["'"]
---
---       direction  -->  "x" | "u" | "r"
---
---       structure  -->  "s" | "f"
---
---  <table border="1" rules="groups">
---   <thead>
---    <tr><td></td>  <td colspan=4>structure</td></tr>
---    <tr><td>direction</td> <td align="center">"s"</td>
---        <td align="center">"f"</td>
---        <td> + optional With, e.g., </td><td>  + optional ', e.g.,</td>
---    </tr>
---   </thead>
---   <tbody>
---    <tr><td align="center">"x"</td><td align="right">xdfs</td>
---        <td align="right">xdff</td>
---        <td align="center">xdfsWith</td><td align="center">xdff'</td>
---    </tr>
---    <tr><td align="center">" "</td><td align="right">dfs</td>
---        <td align="right">dff</td>
---        <td align="center">dffWith</td><td align="center">dfs'</td>
---    </tr>
---    <tr><td align="center">"u"</td><td align="right">udfs</td>
---        <td align="right">udff</td>
---        <td align="center" colspan="2">udfsWith'</td>
---   </tr>
---    <tr><td align="center">"r"</td><td align="right">rdfs</td>
---        <td align="right">rdff</td>
---        <td align="center" colspan="2">rdfrWith'</td>
---    </tr>
---   </tbody>
--- </table>
---
---  <table border="1" rules="groups">
---   <thead>
---    <tr><td colspan="2" align="center">Direction Parameter</td></tr>
---   </thead>
---   <tbody>
---    <tr><td>"x"</td><td>parameterized by a function that specifies which nodes 
---          to be visited next,</td></tr>
---
---     <tr><td>" "</td><td>the "normal" case: just follow successors</td></tr>
---    
---     <tr><td>"u"</td><td>undirected, ie, follow predecesors and successors</td></tr>
---      
---      <tr><td>"r"</td><td>reverse, ie, follow predecesors</td></tr>
---   </tbody>
--- </table>
---
---
---     Structure Parameter
---     -------------------
---      s : result is a list of 
---           (a) objects computed from visited contexts  ("With"-version)
---           (b) nodes                                   (normal version)
---
---      f : result is a tree/forest of 
---           (a) objects computed from visited contexts  ("With"-version)
---           (b) nodes                                   (normal version)
---
---     Optional Suffixes
---     -----------------
---      With : objects to be put into list/tree are given by a function
---             on contexts, default for non-"With" versions: nodes
---
---      '    : parameter node list is given implicitly by the nodes of the 
---             graph to be traversed, default for non-"'" versions: nodes
---             must be provided explicitly
---
---
---     Defined are only the following 18 most important function versions:
---
---       xdfsWith
---        dfsWith,dfsWith',dfs,dfs'
---        udfs,udfs'
---        rdfs,rdfs'
---       xdffWith
---        dffWith,dffWith',dff,dff'
---        udff,udff'
---        rdff,rdff'
---       
---     Others can be added quite easily if needed.
--- @author Bernd Braßel
--- @version May 2005


module GraphAlgorithms (
    module GraphInductive,

    -- * Graph Operations
    grev,
    undir,unlab,
    gsel, --gfold,
    -- * Filter Operations
    efilter,elfilter,
    -- * Predicates and Classifications
    hasLoop,isSimple,

    -- * Tree Operations
    postorder, postorderF, preorder, preorderF,

    --- Depth First Search
    CFun,
    dfs,dfs',dff,dff',
    dfsWith, dfsWith',dffWith,dffWith',
    -- * Undirected DFS
    udfs,udfs',udff,udff',
    -- * Reverse DFS
    rdff,rdff',rdfs,rdfs',
    -- * Applications of DFS\/DFF
    topsort,topsort',
    scc,reachable,lscc,lreachable,
    -- * Applications of UDFS\/UDFF
    components,noComponents,isConnected

    ) where

import GraphInductive
--import Thread (threadMaybe,threadList)

import List (nub)
import Tree

------------------------------------------------------------------
-- some useful operations on graphs
------------------------------------------------------------------

--- Reverse the direction of all edges.
grev :: Graph a b -> Graph a b 
grev = gmap (\(p,v,l,s)->(s,v,l,p))

--- Make the graph undirected, i.e. for every edge from A to B, there
--- exists an edge from B to A.
---
--- This version of undir considers edge lables and keeps edges with
--- different labels.
---
--- An alternative is the definition below:
---
--- <pre>undir = gmap (\(p,v,l,s)->
---           let ps = nubBy (\x y->snd x==snd y) (p++s) in (ps,v,l,ps))</pre>

undir :: Graph a b -> Graph a b
undir = gmap (\(p,v,l,s)->let ps = nub (p++s) in (ps,v,l,ps))

--- Remove all labels.
--- alternative:
---    unlab = nmap (\_->()) . emap (\_->())

unlab :: Graph _ _ -> Graph () ()
unlab = gmap (\(p,v,_,s)->(unlabAdj p,v,(),unlabAdj s))
        where unlabAdj = map (\(_,v)->((),v))

--------------------------------------------------
-- Filter operations
--------------------------------------------------


--- Return all 'Context's for which the given function returns 'True'.
gsel :: (Context a b -> Bool) -> Graph a b -> [Context a b]
gsel p = ufold (\c cs->if p c then c:cs else cs) []


--- Filter based on edge property.
efilter :: (LEdge b -> Bool) -> Graph a b -> Graph a b
efilter f = ufold cfilter empty
            where cfilter (p,v,l,s) g = (p',v,l,s') :& g
                   where p' = filter (\(b,u)->f (u,v,b)) p
                         s' = filter (\(b,w)->f (v,w,b)) s

--- Filter based on edge label property.
elfilter :: (b -> Bool) -> Graph a b -> Graph a b
elfilter f = efilter (\(_,_,b)->f b)


-- some predicates and classifications
--

-- | 'True' if the graph has any edges of the form (A, A).
hasLoop :: Graph _ _ -> Bool
hasLoop = not . null . (gsel (\c->(node' c `elem` suc' c)))

-- | The inverse of 'hasLoop'.
isSimple :: Graph _ _ -> Bool
isSimple = not . hasLoop


-- | Flatten a 'Tree', returning the elements in post-order.
postorder :: Tree a -> [a]
postorder (Node v ts) = postorderF ts ++ [v]

-- | Flatten multiple 'Tree's in post-order.
postorderF :: [Tree a] -> [a]
postorderF = concatMap postorder

-- | Flatten a 'Tree', returning the elements in pre-order.  Equivalent to
--'flatten' in "Data.Tree".
preorder :: Tree a -> [a]
preorder = flatten

-- | Flatten multiple 'Tree's in pre-order.
preorderF :: [Tree a] -> [a]
preorderF = concatMap preorder

----------------------------------------------------------------------
-- Depth First Search (DFS) AND FRIENDS
----------------------------------------------------------------------


-- fixNodes fixes the nodes of the graph as a parameter
--
fixNodes ::  ([Node] -> Graph a b -> c) -> Graph a b -> c
fixNodes f g = f (nodes g) g


-- generalized depth-first search
--  (could also be simply defined as applying preorderF to the 
--   result of xdffWith)
--   
type CFun a b c = Context a b -> c

xdfsWith ::  CFun a b [Node] -> CFun a b c -> [Node] -> Graph a b -> [c]
xdfsWith d f vs g | null vs || isEmpty g = []
                  | otherwise 
                  = case match (head vs) g of
                         (Just c,g')  -> f c:xdfsWith d f (d c++(tail vs)) g'
                         (Nothing,g') -> xdfsWith d f (tail vs) g'  


-- dfs
--
dfsWith ::  CFun a b c -> [Node] -> Graph a b -> [c]
dfsWith = xdfsWith suc'

dfsWith' ::  CFun a b c -> Graph a b -> [c]
dfsWith' f = fixNodes (dfsWith f)

dfs ::  [Node] -> Graph _ _ -> [Node]
dfs = dfsWith node'

dfs' ::  Graph _ _ -> [Node]
dfs' = dfsWith' node'


-- undirected dfs, ie, ignore edge directions
--
udfs ::  [Node] -> Graph _ _ -> [Node]
udfs = xdfsWith neighbors' node'  

udfs' ::  Graph _ _ -> [Node]
udfs' = fixNodes udfs


-- reverse dfs, ie, follow predecessors
--
rdfs ::  [Node] -> Graph _ _ -> [Node]
rdfs = xdfsWith pre' node'  

rdfs' ::  Graph _ _ -> [Node]
rdfs' = fixNodes rdfs


-- generalized depth-first forest
-- 
xdfWith ::  CFun a b [Node] -> CFun a b c -> [Node] -> Graph a b -> ([Tree c],Graph a b)
xdfWith d f vs     g | null vs || isEmpty g = ([],g)
                     | otherwise 
                     = case match (head vs) g of
                        (Nothing,g1) -> xdfWith d f (tail vs) g1 
                        (Just c,g1)  -> aux c (xdfWith d f (d c) g1)

   where
     aux c (ts,g2) = let (ts',g3) = xdfWith d f (tail vs) g2 in (Node (f c) ts:ts',g3)


xdffWith ::  CFun a b [Node] -> CFun a b c -> [Node] -> Graph a b -> [Tree c]
xdffWith d f vs g = fst (xdfWith d f vs g)


-- dff
--
dffWith ::  CFun a b c -> [Node] -> Graph a b -> [Tree c]
dffWith = xdffWith suc'

dffWith' ::  CFun a b c -> Graph a b -> [Tree c]
dffWith' f = fixNodes (dffWith f)

dff ::  [Node] -> Graph _ _ -> [Tree Node]
dff = dffWith node'

ldff :: [Node] -> Graph a _ -> [Tree a]
ldff = dffWith lab'

dff' ::  Graph _ _ -> [Tree Node]
dff' = dffWith' node'


-- undirected dff
--
udff ::  [Node] -> Graph _ _ -> [Tree Node]
udff = xdffWith neighbors' node'

udff' ::  Graph _ _ -> [Tree Node]
udff' = fixNodes udff


-- reverse dff, ie, following predecessors
--
rdff ::  [Node] -> Graph _ _ -> [Tree Node]
rdff = xdffWith pre' node'

lrdff :: [Node] -> Graph a _ -> [Tree a]
lrdff = xdffWith pre' lab'

rdff' ::  Graph _ _ -> [Tree Node]
rdff' = fixNodes rdff


----------------------------------------------------------------------
-- ALGORITHMS BASED ON DFS
----------------------------------------------------------------------

components ::  Graph _ _ -> [[Node]]
components = (map preorder) . udff'

noComponents ::  Graph _ _ -> Int
noComponents = length . components

isConnected ::  Graph _ _ -> Bool
isConnected = (==1) . noComponents

postflatten :: Tree a -> [a]
postflatten (Node v ts) = postflattenF ts ++ [v]

postflattenF :: [Tree a] -> [a]
postflattenF = concatMap postflatten

topsort ::  Graph _ _ -> [Node]
topsort = reverse . postflattenF . dff'

topsort' ::  Graph a _ -> [a]
topsort' = reverse . postorderF . (dffWith' lab')

scc ::  Graph _ _ -> [[Node]]
scc g = map preorder (rdff (topsort g) g)            -- optimized, using rdff
-- sccOrig g = map preorder (dff (topsort g) (grev g))  -- original by Sharir

reachable ::  [Node] -> Graph _ _ -> [Node]
reachable vs g = preorderF (dff vs g)

lscc :: Graph a _ -> [[a]]
lscc g = map preorder (lrdff (topsort g) g)

lreachable :: [Node] -> Graph a _ -> [a]
lreachable vs g = preorderF (ldff vs g)

