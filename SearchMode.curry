module SearchMode (SearchMode (..)) where

data SearchMode
  = NoSearch -- no search
  | DFS      -- depth first search
  | BFS      -- bredth first search
  | IterDFS  -- iterative depth first search
  | PAR      -- parallel search
