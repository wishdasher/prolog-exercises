:- use_module(library(clpfd)).
:- use_module(library(lists)).

visnum(List, K) :-
    [H|T] = List,
    count_vis(H, T, K),
    maplist(shave_all, List).

count_vis(H, [X|T], K) :-
    (X #> H) #<=> R,
    A #= max(X, H),
    K1 #= K-R,
    count_vis(A, T, K1).
count_vis(_, [], 1).

chop(_N, [], []).
chop(N, [H|T], LL) :-
    sub_chop(N, 0, [H|T], [], [], LL).
sub_chop(_N, _I, [], IL, OL, LL) :-
    append(OL, [IL], LL).
sub_chop(N, I, [H|T], IL, OL, LL) :-
    (   I is N ->  append(OL, [IL], INT), sub_chop(N, 0, [H|T], [], INT, LL)
    ;   I1 is I + 1, append(IL, [H], IL1), sub_chop(N, I1, T, IL1, OL, LL)
    ).

sudoku(N, Puzzle, Flat) :-
    N2 is N * N,
    length(Puzzle, N2),
    maplist(lengthA(N2), Puzzle),
    maplist(domainA(N2), Puzzle),
    maplist(all_distinct, Puzzle),
    transpose(Puzzle, Columns),
    maplist(all_distinct, Columns),
    make_blocks(Puzzle, N2, N, Blocks, Flat),
    maplist(all_distinct, Blocks).

make_blocks(List, Length, SubLength, Sections, Flat) :-
    chop(SubLength, List, Chopped),
    maplist(transpose, Chopped, Transposed),
    append(Transposed, F1),
    maplist(chop(SubLength), F1, Subgrids),
    append(Subgrids, F2),
    append(F2, Flat),
    chop(Length, Flat, Sections).
    %maplist(shave_all, Flat).

lengthA(N2, Row) :-
    length(Row, N2).

domainA(N2, Row) :-
    domain(Row, 1, N2).

parse_clues([H|T], SS) :-
    parse_clue(H, SS),
    parse_clues(T, SS).
parse_clues([], _SS).

parse_clue(g(N,R,C), SS) :-
    nth1(R, SS, Row),
    nth1(C, Row, N).
parse_clue(v(V,n,RC), SS) :-
    transpose(SS, Trans),
    nth1(RC, Trans, View),
    visnum(View, V).
parse_clue(v(V,e,RC), SS) :-
    nth1(RC, SS, Rev),
    reverse(Rev, View),
    visnum(View, V).
parse_clue(v(V,s,RC), SS) :-
    transpose(SS, Trans),
    nth1(RC, Trans, Rev),
    reverse(Rev, View),
    visnum(View, V).
parse_clue(v(V,w,RC), SS) :-
    nth1(RC, SS, View),
    visnum(View, V).

skysudoku(ss(K, Clues), SS) :-
    sudoku(K, SS, Flat),
    parse_clues(Clues, SS),
    maplist(shave_all, Flat),
    labeling([ffc,bisect], Flat).

shave_all(X) :- fd_set(X, FD), fdset_to_list(FD, L),
    findall(X, member(X,L), Vs),
    list_to_fdset(Vs, FD1), X in_set FD1.

%% test(V1, V2) :-
%%     statistics(runtime, [T0|_]),
%%     skysudoku(V1, V2),
%%     statistics(runtime, [T1|_]),
%%     T is T1 - T0,
%%     format('p/0 took ~3d sec.~n', [T]).
