% split(+N, +List, -Front, -Back): Front is the list containing the first
% N elements of List, and Back is the list of the remaining elements of
% List. N can be assumed to be a given integer.

split(0, [], [], []).
split(N, [H|T], Front, Back) :-
    (   N = 0 -> Front = [], Back = [H|T]
    ;   N > 0,
        N0 is N - 1,
        split(N0, T, F1, Back),
        Front = [H|F1]
    ).


% chop(+N, +List, -LofLists): LofLists is a list whose elements are
% nonempty lists, such that the concatenation of these results in List.
% All elements of LofLists, except for the last, have length N, the
% length of the last should be between 1 and N (inclusive).

chop(_N, [], []).
chop(N, [H|T], LL) :-
    sub(N, 0, [H|T], [], [], LL).

% sub(N, Incr, List, Innerlist, Outerlist, FinalList). Incr is a counter for
% how many elements have been added into the InnerList. InnerLists are added
% to the OuterList. Final solution is stored in FinalList.
sub(_N, _I, [], IL, OL, LL) :-
    app(OL, [IL], LL).
sub(N, I, [H|T], IL, OL, LL) :-
    (   I is N ->  app(OL, [IL], INT), sub(N, 0, [H|T], [], INT, LL)
    ;   I1 is I + 1, app(IL, [H], IL1), sub(N, I1, T, IL1, OL, LL)
    ).

% app(A, B, C). Append A and B into C.
app([], L2, L2).
app([X|L1], L2, [X|L3]) :-
    app(L1, L2, L3).


% insert_ord(+RL0, +Element, ?RL):
% RL0 is a proper list of numbers, which is strictly increasing. The
% strictly increasing RL is obtained from RL0 by inserting the number
% Element, if Element is not already in RL0. Otherwise RL = RL0.

insert_ord([], E, [E]).
insert_ord([H|T], E, L) :-
    (   E < H -> insert_ord(T, H, List), L = [E|List]
    ;   E > H -> insert_ord(T, E, List), L = [H|List]
    ;   insert_ord(T, H, List), L = List
    ).


% nth1(+N, ?L, ?E): The N-th element of the (possibly open ended) list L
% is E. The head of a list is considered its 1st element.

nth1(N, [H|T], E) :-
    (   N = 1 ->  E = H
    ;   N0 is N - 1,
        nth1(N0, T, E)
    ).


% visible_count(+L, ?N): N is the number of left-visible elements in
% the proper list L of positive integers

visible_count([], 0).
visible_count([H|T], C) :-
    incre([H|T], H, 1, C).

% incre(+L, +V, ?C, ?F): L is the list of integers, V is the largest
% value observed so far, C is the starting count, F is the final count
incre([], _V, C, C).
incre([H|T], V, C, F) :-
    (   H > V -> I is C + 1, incre(T, H, I, F)
    ;   incre(T, V, C, F)
    ).