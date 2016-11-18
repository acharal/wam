?- goal(m(X, Y), g(a, W)).

m(X, Y, c, a).
g(a, b, c, d).

goal(A, B) :- A(c, a), A(c, b).
goal(A, B) :- B(c, d), A(c, a).
