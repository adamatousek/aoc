answer(X) :- answer(20, X).

answer(1, 1) :- !.
answer(N, R) :- M is N - 1, answer(M, P), R is lcm(N, P).
