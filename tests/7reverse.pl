?- reverse([a,b,c,d,e,f,g], X).

append([], Ys, Ys).
append([X|Xs], Ys, [X|Zs]) :- append(Xs, Ys, Zs).

reverse([],[]).
reverse([X|Xs],Zs) :- reverse(Xs, Ys), append(Ys, [X], Zs).

