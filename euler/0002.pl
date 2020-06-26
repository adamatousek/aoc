% Nope.

answer(N) :- answer(1, 1, 0, N).

answer(_, B, S, S) :- B > 4000000, !.
answer(A, B, ES, Res) :- C is A + B, condadd(B, ES, NextES), answer(B, C, NextES, Res).

condadd(Addend, SubSum, Sum) :- Addend mod 2 =:= 0, !, Sum is Addend + SubSum.
condadd(_, Sum, Sum).
