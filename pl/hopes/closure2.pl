?- closure(friends, mike, Y).

closure(R, X, Y) :- R(X, Y).
closure(R, X, Y) :- R(X, Z), closure(R, Z, Y).

friends(john, mike).
friends(mike, lina).
friends(serena, rosalie).
