% necessary for transpose function in SWI-PROLOG
:- use_module(library(lists)).
:- use_module(library(between)).

% fails if there are duplicates
check_duplicates(List) :-
    exclude(zero, List, Filtered),
    sort(Filtered, Sorted),
    length(Filtered, O),
    length(Sorted, S),
    O = S.

zero(E) :-
    E == 0.

check_duplicates_lists(LL) :-
    maplist(check_duplicates, LL).


consistent(Input) :-
    transpose(Input, Transposed),
    check_duplicates_lists(Input),
    check_duplicates_lists(Transposed),
    create_subgrids(Input, Sections),
    check_duplicates_lists(Sections).


% chop(+N, +List, -LofLists): LofLists is a list whose elements are
% nonempty lists, such that the concatenation of these results in List.
% All elements of LofLists, except for the last, have length N, the
% length of the last should be between 1 and N (inclusive).
chop(_N, [], []).
chop(N, [H|T], LL) :-
    sub_chop(N, 0, [H|T], [], [], LL).
% sub(N, Incr, List, Innerlist, Outerlist, FinalList). Incr is a counter for
% how many elements have been added into the InnerList. InnerLists are added
% to the OuterList. Final solution is stored in FinalList.
sub_chop(_N, _I, [], IL, OL, LL) :-
    append(OL, [IL], LL).
sub_chop(N, I, [H|T], IL, OL, LL) :-
    (   I is N ->  append(OL, [IL], INT), sub_chop(N, 0, [H|T], [], INT, LL)
    ;   I1 is I + 1, append(IL, [H], IL1), sub_chop(N, I1, T, IL1, OL, LL)
    ).

create_subgrids(List, Sections) :-
    length(List, Length),
    SubLength is integer(sqrt(Length)),
    chop(SubLength, List, Chopped),
    maplist(transpose, Chopped, Transposed),
    maplist(chop(SubLength), Transposed, Subgrids),
    flatten(Subgrids, Flattened),
    chop(Length, Flattened, Sections).


sudoku0(Puzzle, Solution) :-
    length(Puzzle, Length),
    flatten(Puzzle, Flattened),
    fill_and_check([], Flattened, Length, Solution).

fill_and_check(Head, Remaining, Length, Solution) :-
    [H|T] = Remaining,
    (   H == 0 ->  between(1, Length, X),
        append(Head, [X], NewHead),
        append(NewHead, T, FlatPotential),
        chop(Length, FlatPotential, Potential),
        consistent(Potential),
        (   \+(has_zero(Potential)) ->  Solution = Potential
        ;   fill_and_check(NewHead, T, Length, Solution)
        )
    ;   append(Head, [H], NewHead),
        fill_and_check(NewHead, T, Length, Solution)
    ).

has_zero(LL) :-
    %maplist(memberchk(0), LL).
    flatten(LL, F),
    memberchk(0, F).

flatten(List, Flattened):-
  flatten(List, [], Flattened).
flatten([], Flattened, Flattened).
flatten([Item|Tail], L, Flattened):-
  flatten(Item, L1, Flattened),
  flatten(Tail, L, L1).
flatten(Item, Flattened, [Item|Flattened]):-
  \+ is_list(Item).