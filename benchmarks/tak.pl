
tak(X,Y,Z,A) :-
	X =< Y, !,
	Z = A.
tak(X,Y,Z,A) :-
	% X > Y,
	X1 is X - 1,
	tak(X1,Y,Z,A1),
	Y1 is Y - 1,
	tak(Y1,Z,X,A2),
	Z1 is Z - 1,
	tak(Z1,X,Y,A3),
	tak(A1,A2,A3,A).

goal0(R) :- tak(24,16,8,R).
goal1(R) :- tak(27,16,8,R).
goal2(R) :- tak(33,17,8,R).

main :- goal1(R), write(R), nl, halt.
