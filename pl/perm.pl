?- perm([a,b,c],X).

perm(L, [H|P]) :- delete(H,L,R),perm(R,P).
perm([],[]).


delete(X, [X|T], T).
delete(X, [H|T], [H|NT]):-delete(X,T,NT).
