:- use_module(library(clpfd)).

optnum(L, K) :-
    all_distinct(L),
    count_opt(L, K).

count_opt([A,B,C|R], K) :-
    (B #> A #/\ B #> C) #<=> X,
    (B #< A #/\ B #< C) #<=> N,

    K1 #= K-X-N,
    count_opt([B,C|R], K1).
count_opt([], 0).
count_opt([_], 0).
count_opt([_,_], 0).


visnum([H|T], K) :-
    count_vis(H, T, K).

count_vis(H, [X|T], K) :-
    (X #> H) #<=> R,
    R #=> A #= X,
    #\ R #=> A #= H,
    K1 #= K-R,
    count_vis(A, T, K1).
count_vis(_, [], 1).


draw(G, L) :-
    length(G, N),
    length(L, N),
    line(L),
    split(L, LA, LB).

line([A-B,B-C|R]) :-
    line([B-C|R]).
line([_-_]).
line([]).


/*
consult('~/constraint_2.pl').


L=[1,_,_,_], domain(L, 1, 4), optnum(L, 2), labeling([], L).

L=[_,_,2,_], domain(L, 1, 4), all_distinct(L), visnum(L, 3), labeling([], L).

L=[A,B,C,D],domain(L,1,4),visnum(L,3),visnum([D,C,B,A],2),all_distinct(L),D=1
*/