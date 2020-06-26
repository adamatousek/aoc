% Sike, you thought!

answer(Sum) :- answer(999, 0, Sum).

answer(0, Acc, Acc) :- !.
answer(N, Acc, Sum) :- M is N - 1, condadd(N, Acc, NextAcc), answer(M, NextAcc, Sum).

condadd(N, Addend, Sum) :- cond(N), !, Sum is N + Addend.
condadd(_, Addend, Addend).

cond(N) :- N mod 3 =:= 0, !.
cond(N) :- N mod 5 =:= 0.
