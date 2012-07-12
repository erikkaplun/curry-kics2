
data Peano = O | S Peano
data List a = Nil | Cons a (List a)

-- simple types
goal0 = True =:= True                      -- success
goal1 = True =:= False                     -- fail
goal2 = x =:= True where x free            -- success
goal3 = x =:= True &> x where x free       -- True
goal4 = x =:= True &> [x,x] where x free   -- [True,True]
goal5 = [x =:= True &> x,x] where x free   -- [True,True]
goal6 = [x,x =:= True &> x] where x free   -- [True,True]

uni :: Bool -> Bool -> Success
uni x y = x =:= y

goal7 = uni x y where x,y free             -- success
goal8 =  x =:= y                           -- [True,True]
      &> y=:= True
      &> [x,y]  where x,y free

goal9 =  x =:= y                           -- [True,True]
      &> x=:= True
      &> [x,y]  where x,y free

goal10 =  x =:= y                          -- [False,False]
       &> x =:= z
       &> y=:=False &> [x,y]
  where x,y,z free

goal11 =  x =:= y                          -- [False,False,False]
       &> x =:= z
       &> x=:=False
       &> [z,x,y]  where x,y,z free

goal12 = x =:= y                           -- [False,False,False]
       &> x =:= z
       &> z=:=False &> [x,z,y]
  where x,y,z free

goal13 =  x =:= y                          -- [False,False,False]
       &> x =:= z
       &> z=:=False
       &> y=:=False
       &> [x,y,z]  where x,y,z free

goal14 = x=:=y &> y=:=False &> x where x,y free            -- False
goal15 = x=:=(y?True) &> y=:=False &> x where x,y free     -- False?True

-- complex types

goal16 = x=:=[] &> True:x where x free                     -- [True]
goal17 = x=:=[True] &> x where x free                      -- [True]

goal18 = x=:=y &> (y=:= [True] &> x) where x,y free          -- [True]

f [False] = success
goal19 = x=:=y &> y=:= [True] &> f x &> x where x,y free   -- fail

g [True] = success
goal20 = x=:=y &> y=:= [True] &> g x &> x where x,y free   -- [True]


goal21 = x=:=(y?[False]) &> y=:=[True] &> x where x,y free -- [True]?[False]

uni2 :: [Bool] -> [Bool] -> Success
uni2 x y = x =:= y

goal22 = uni2 x [y] where x,y free                         -- success
goal23 = uni2 x [y] &> x where x,y free                    -- [y]
goal24 = x =:= [y] &> y=:=True &> x where x,y free         -- [True]
goal25 = x =:= (y:z) &> x=:=(False:z1:z2) &> z2 =:=[] &> x -- [False,z1]
  where x,y,z,z1,z2 free


goal26 = x =:= [y?True] &> y=:=False &> x where x,y free   -- [False]?[True]

goal27 = [x,True,z]=:=[False,y,y] &> [x,y,z]               -- [False,True,True]
  where x,y,z free

main =
  ( ((0, goal0), (2, goal2), (3, goal3), (4, goal4)) -- goal1 is fail
  , ((5, goal5), (6, goal6), (7, goal7), (8, goal8), (9, goal9))
  , ((10, goal10), (11, goal11), (12, goal12), (13, goal13), (14, goal14))
  , ((16, goal16), (17, goal17), (18, goal18)) -- goal15 is nd, goal19 is fail
  , ((20, goal20), (22, goal22), (23, goal23), (24, goal24)) -- goal21 is nd
  , ((25, goal25), (27, goal27)) -- goal26 is nd
  )
