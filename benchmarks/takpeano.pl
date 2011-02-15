ifthenelse(true,x,_,x).
ifthenelse(false,_,y,y).

add(o,N,N).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

double(X,R) :- add(X,X,R).

dec(s(X),X).

leq(o,_,true).
leq(s(_),o,false).
leq(s(X),s(Y),R) :- leq(X,Y,R).

tak(X,Y,Z,A) :-
	leq(X,Y,true), !,
	Z = A.
tak(X,Y,Z,A) :-
	% X > Y,
	dec(X,X1),
	tak(X1,Y,Z,A1),
	dec(Y,Y1),
	tak(Y1,Z,X,A2),
	dec(Z,Z1),
	tak(Z1,X,Y,A3),
	tak(A1,A2,A3,A).

two(s(s(o))).
four(F) :- two(T), double(T,F).
n8(N) :- four(F), double(F,N).
n16(N) :- four(F), double(F,N).
n24(N) :- n16(N16), n8(N8), add(N8,N16,N).
n27(N) :- n16(N16), two(N2), add(s(N2),N24,N).

goal0(R) :- n24(N24), n16(N16), n8(N8), tak(N24,N16,N8,R).
goal1(R) :- n27(N27), n16(N16), n8(N8), tak(N27,N16,N8,R).
%goal2(R) :- tak(33,17,8,R).

main :- goal1(R), write(R), nl, halt.
