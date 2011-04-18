goal1 = y =:<= True & x =:= True & x =:= y where x, y free -- Success

goal2 = y =:<= [True] &> x =:<= ([]?[False]) &> x=:=y &> x where x, y free -- fail
goal3 = y =:<= [True] &> x =:<= ([False]?[]) &> x=:=y &> x where x, y free -- fail