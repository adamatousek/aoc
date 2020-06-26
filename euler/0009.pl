% Finally something Prolog-worthy again

answer(Prod) :- between(1, 1000, C), AB is 1000 - C, between(1, AB, A), B is AB - A,
                C * C =:= A * A + B * B, Prod is A * B * C.
