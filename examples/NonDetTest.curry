module NonDetTest where

coin = True ? False

goal0 = (\x -> (x,x)) coin
goal1 = (coin, coin)

goal2 = replicate 4 coin

goal3 = replicate 4 3