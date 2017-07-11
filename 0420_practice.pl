% 2017.04.20 class practice


% first_pos(L, P): P is the first positive integer in L

first_pos([X,Xs], P) :-
    (X #> 0) #<=> B,
    B #=>   P #= X
    #\ B # =>
    first_pos(Xs, P).


first_pos(L, P, B).


first_pos([], _P, 0).
first_pos([X|T], P, B1) :-
    (X #> 0) #<=> B,
    B #=> P #= X,
    #\B #=> (B1 #= B2),
    first_pos(T, P, B2).

