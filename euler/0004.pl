% Nope.

answer(N) :- answer(100, 100, 0, N).

answer(1000, _, Max, Max) :- !.
answer(A, B, Max, N) :- B > A, !, A2 is A + 1, answer(A2, 100, Max, N).
answer(A, B, Max, N) :- update_max(A*B, Max, Max2), B2 is B + 1, answer(A, B2, Max2, N).

update_max(Prod, Old, Max) :- P is Prod, palindrome(P), !, Max is max(P,Old).
update_max(_, Old, Old).

palindrome(P) :- number_to_chars(P, DS), reverse(DS, RDS), DS == RDS.
