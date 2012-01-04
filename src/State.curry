-------------------------------------------------------------------
-- This module provides an implementation of the state monad
-- @author fre
-- @version January 3, 2012
-------------------------------------------------------------------

module State ( State 
             , bindS
             , returnS
             , getS
             , setS
             , runState
             , evalState
             , execState
             , sequenceS
             , modifyS
             ) where

type State a s = s -> (a, s)

bindS :: State a s -> (a -> State b s) -> State b s
bindS state f s = let (x, newS) = state s
                  in f x newS 

returnS :: a -> State a s
returnS x s = (x,s)

sequenceS :: [State a s] -> State [a] s
sequenceS = 
 foldr (\s newS -> s    `bindS` \a  -> 
                   newS `bindS` \as -> 
                   returnS (a:as))
       (returnS [])
       
getS :: State s s
getS s = (s,s)

setS :: s -> State () s
setS newState _ = ((),newState)

modifyS :: (s -> s) -> State () s
modifyS f s = ((),f s)

runState :: State a s -> s -> (a,s)
runState state s = state s

evalState :: State a s -> s -> a
evalState state s = fst (runState state s)

execState :: State a s -> s -> s
execState state s = snd (runState state s)


