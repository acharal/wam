?- reverse([a,b],X).

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

reverse([],[]).
reverse([X|Xs],Zs) :- reverse(Xs, Ys), append(Ys, [X], Zs).


flatten(Xs, Ys) :- flattern(Xs, [], Ys).
flatten([X|Xs], As, Ys) :- flatten(Xs,As,As1), flatten(X,As1, Ys).
flatten([], Ys, Ys).
