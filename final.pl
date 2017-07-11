:- use_module(library(clpfd)).
:- use_module(library(lists)).


counter_list(L) :-
    shorter_list(LL, L),
    append(LL, L),
    maplist(counter_segment, LL).
counter_segment([Cnt|Zs]) :-
    length(Zs, Cnt),
    Cnt > 0,
    maplist(=(0), Zs).


%% clist(L) :-
%%     shorter_list(LL, L),
%%     append(LL, L),
%%     maplist(cs, LL),
%%     maplist(shave_all, L).
%% cs([Cnt|Zs]) :-
%%     length(Zs, Cnt),
%%     Cnt #> 0,
%%     maplist(#=(0), Zs),
%%     maplist(shave_all, Zs).

clist(L) :-
    [H|T] = L,
    length(L, Length),
    domain(L, 0, Length),
    H #\= 0,
    nth1(Length, L, 0),
    %% H1 #= H + 1,
    %% sublist(L, SL, 0, H1, After),
    %% sum(SL,#=,H),
    helper(T, H).

helper([H|T], Left) :-
    H #= 0 #<=> Left #\= 0,
    Left1 #= Left - 1,
    Pass #= max(H, Left1),
    helper(T, Pass).
helper([], 0).


shave_all(X) :- fd_set(X, FD), fdset_to_list(FD, L),
    findall(X, member(X,L), Vs),
    list_to_fdset(Vs, FD1), X in_set FD1.

/*

test_all(final).
consult('~/final.pl').



ADVANCED
| ?- L = [_,_,_C,_,_,_], clist(L), _C #> 0.
L = [1,0,_C,0,_A,0],
_A in 0..1,
_C in{1}\/{3} ? p;
no
| ?- length(L, 13), clist_adv(L), nth1(5, L, _X), _X #>0,
nth1(8, L, 0), _X #>0, nth1(11, L, _Y), _Y #>0.
L = [_A,0,_B,0,_X,0,_C,0,_D,0,2,0,0],
_D in 0..1,
_C in(0..1)\/{3},
_X in{1}\/{3}\/{5},
_B in 0..1,
_A in{1}\/{3} ? ;
no
| ?-
*/

:- use_module(library(samsort)).
:- use_module(library(timeout)).

% test_case(N, Sol, Goal, Sols): Test case N involves goal Goal which, when
% called, returns in Sol the solution. The expected list of solutions for
% test case N is Sols.

test_case(01,[],(length(L, 13), clist(L), nth1(5, L, _X), _X #>0, nth1(8, L, 0), _X #>0, nth1(11, L, _Y), _Y #>0),[[]]).
test_case(02,L ,(length(L, 3), clist(L), labeling([], L)),[[2,0,0]]).
test_case(03,L ,(L = [_A,_,_,_A,_,_], clist(L), labeling([], L)),[[2,0,0,2,0,0]]).
test_case(04,L ,(L = [_,_,_C,_,_,_], clist(L), _C #> 0, labeling([], L)),[[1,0,1,0,1,0],[1,0,3,0,0,0]]).
test_case(05,L ,(length(L, 13), clist(L), nth1(5, L, _X), _X #>0, nth1(8, L, 0), _X #>0, nth1(11, L, _Y), _Y #>0, labeling([], L)),[[1,0,1,0,1,0,1,0,1,0,2,0,0],[1,0,1,0,1,0,3,0,0,0,2,0,0],[1,0,1,0,3,0,0,0,1,0,2,0,0],[1,0,1,0,5,0,0,0,0,0,2,0,0],[3,0,0,0,1,0,1,0,1,0,2,0,0],[3,0,0,0,1,0,3,0,0,0,2,0,0],[3,0,0,0,3,0,0,0,1,0,2,0,0],[3,0,0,0,5,0,0,0,0,0,2,0,0]]).
test_case(06,L ,(N = 26, N2 is N+2, length(L, N2), domain(L, 0, N2), sum(L, #=, N), L=[_,_,C|_], C #>0, clist(L), labeling([down], L)),[[1,0,25,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]]).
test_case(07,L ,(N = 250, length(L1, N), length(L2, N), append(L1, [B|L2], L), clist([A|L]), A + B #= 2*N+2, labeling([down], L)),[]).
test_case(08,L ,(N = 31, length(L, N), clist(L), (foreach(X,L) do X mod 2 #= 1 #\/ X #= 0), labeling([down], L)),[]).
test_case(09,L ,(length(L, 3), clist(L)),[[2,0,0]]).
test_case(10,L ,(L = [_A,_,_,_A,_,_], clist(L)),[[2,0,0,2,0,0]]).
test_case(11,DL,(L = [_,_,_C,_,_,_], clist(L), _C #> 0, (foreach(X,L),foreach(D,DL) do fd_set(X,S),fdset_to_list(S,D))),[[[1],[0],[1,3],[0],[0,1],[0]]]).
test_case(12,DL,(length(L, 13), clist(L), nth1(5, L, _X), _X #>0, nth1(8, L, 0), _X #>0, nth1(11, L, _Y), _Y #>0, (foreach(X,L),foreach(D,DL) do fd_set(X,S),fdset_to_list(S,D))),[[[1,3],[0],[0,1],[0],[1,3,5],[0],[0,1,3],[0],[0,1],[0],[2],[0],[0]]]).


% If you want to test multiple variants, put a fact:
%
%              variant('Variant name').
%
% in each program, and call test_variant.
test_variant :-
    variant(V),
    test_all(V).

% timeout(TO): TO is the default time out limit, in msec.
timeout(20000).

% Run all test cases for variant V, with the default timeout limit
test_all(V) :-
    timeout(TO),
    test_all(V, TO).

% Run all test cases for variant V, with the timeout limit of TimeOut,
% given in msec.
test_all(V, TimeOut) :-
    test_case(N, Sol, Goal, ExpectedSols),
    statistics(runtime, [T0,_]),  % T0 is the CPU time since the start
                                  % of the Prolog system, in msec
    time_out(findall(Sol, Goal, UnsortedSols), TimeOut, Res),
                                  % Runs the goal in the first arg with a timeout,
                                  % Res will be the atom time_out, if timed out
    statistics(runtime, [T1,_]),
    T is T1-T0,
    (   Res == time_out -> format('~w: ~|test case ~t~w~12+ timed out', [V,N])
    ;   samsort(UnsortedSols, Sols), % sorts without removing duplicates
        (   Sols == ExpectedSols -> format('~w: ~|test case ~t~w~12+ OK       ', [V,N])
        ;   format('~w: ~|test case ~t~w~12+: Expected ~w, got ~w', [V,N,ExpectedSols,Sols])
        )
    ),
    format(' CPU time = ~|~t~3d~8+ sec\n', [T]),
    fail.
test_all(_,_).