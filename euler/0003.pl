% Haha

answer(Res) :- answer(600851475143, 2, Res).

answer(1, Now, Now) :- !.
answer(N, Now, Res) :- N mod Now =:= 0, !, M is N div Now, answer(M, Now, Res).
answer(N, Now, Res) :- Next is Now + 1, answer(N, Next, Res).
