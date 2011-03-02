module InfList where

goal0 = [] == repeat 1

goal1 = [1,1,1] < repeat 1

goal2 = repeat 1 < [1,1,1]

goal3 = [1,1,1] == [1, coin, 1]

coin = 0 ? 1
