:- use_module(library(clpfd)).
:- use_module(library(lists)).


q(A,B,C,D) :-
    A #> 0, B #> 0, C #> 0, D #> 0,
    A #= B + C + D,
    2 * B #< C,
    2 * C #< D.


sudoku_simple(Matrix, N) :-
    process_row(Matrix, N),
    transpose(Matrix, Trans),
    process_row(Trans, N).

process_row(Rows, N) :-
    length(Rows, N),
    maplist(lengthA(N), Rows),
    maplist(domainA(N), Rows),
    maplist(all_distinct, Rows).

lengthA(N, Row) :-
    length(Row, N).

domainA(N, Row) :-
    domain(Row, 1, N).


fibonacci(N, Limit, L) :-
    length(L, N),
    domain(L, 1, Limit),
    fib_constraint(L).
fib_constraint([]).
fib_constraint(List) :-
    length(List, 1);
    length(List, 2).
fib_constraint([A,B,C|Rest]) :-
    A + B #= C,
    fib_constraint([B,C|Rest]).


p(A, B) :-
    abs(mod(A,3) - 1) #= mod(B,2).


knapsack(Items, MaxWeight, MinValue, Selected) :-
    length(Items, N),
    length(Vars, N),
    domain(Vars, 0, 1),
    maplist(get_weight, Items, Weights),
    maplist(get_value, Items, Values),
    maplist(get_id, Items, IDs),
    scalar_product(Weights, Vars, #=, WeightSum),
    scalar_product(Values, Vars, #=, ValueSum),
    WeightSum #=< MaxWeight,
    ValueSum #>= MinValue,
    labeling([], Vars),
    create_output(IDs, Vars, Selected).

get_id(item(I,_,_), I).
get_weight(item(_,W,_), W).
get_value(item(_,_,V), V).

create_output([], [], []).
create_output([X|L1], [1|Vars], [X|L2]) :-
    create_output(L1, Vars, L2).
create_output([_|L1], [0|Vars], L2) :-
    create_output(L1, Vars, L2).