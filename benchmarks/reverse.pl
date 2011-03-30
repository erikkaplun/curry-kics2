% Prolog benchmark: naive reverse of a user-defined list

add(o,N,N).
add(s(X),Y,s(Z)) :- add(X,Y,Z).

double(X,R) :- add(X,X,R).

mult(o,_,o).
mult(s(X),Y,Z) :- mult(X,Y,XY), add(Y,XY,Z).

two(s(s(o))).
four(F) :- two(T), double(T,F).
nat16(N) :- four(F), mult(F,F,N).
nat256(N) :- nat16(M), mult(M,M,N).
nat4096(R) :- nat256(M), nat16(N), mult(M,N,R).

app([],Xs,Xs).
app([X|Xs],Ys,[X|Zs]) :- app(Xs,Ys,Zs).

reverse([],[]).
reverse([X|Xs],R) :- reverse(Xs,Zs), app(Zs,[X],R).

natList(o,[]).
natList(s(X),[s(X)|Z]) :- natList(X,Z).

isList([],true).
isList([_|Xs],R) :- isList(Xs,R).

goal3(R) :- nat4096(R1), natList(R1,R2), reverse(R2,R3), isList(R3,R).

main :- goal3(R), write(R), nl, halt.

