% sublist(+Whole, ?Part, ?Before, ?Length, ?After): Part is a
% sublist of Whole such that there are Before number of elements in
% Whole before Part, After number of elements in Whole after Part
% and the length of Part is Length.
sublist(Whole, Part, Before, Length, After) :-
  append(Divider, End, Whole),
  append(Front, Part, Divider),
  length(Part, Length),
  length(Front, Before),
  length(End, After).


% plateau(L, I, Len): There is a plateau of length Len starting at the
% I-th position of list L.

plateau([H|T], I, Length) :-
  plateau(T, 1, 1, H, I, Length).

% [H|T] is the current list we work with
% Start keeps track of the starting index of a plateau
% Lcounter keeps track of the current plateau length
% Current is the value of the plateau
% I is the starting index of a plateau
% Length is the length of the plateau
plateau([H|T], Start, Lcounter, Current, I, Length) :-
  (   H == Current ->  (   L1 is Lcounter + 1,
                           plateau(T, Start, L1, Current, I, Length))
  ;   (   Lcounter > 1, Length = Lcounter, I = Start
      ;   I1 is Start + Lcounter,
          plateau(T, I1, 1, H, I, Length)
      )
  ).

plateau([], Start, Lcounter, _Current, I, Length) :-
  Lcounter > 1, Length = Lcounter, I = Start.

% select and permutation as discussed in class
select(E, [E|Rest], Rest).
select(E, [X|Tail], [X|Rest]):-
	select(E, Tail, Rest).
perm([], []).
perm(List, [First|Perm]) :-
	select(First, List, Rest),
	perm(Rest, Perm).

% draw(+G, -L): Graph G and line L describe the same graph.
draw([], []).
draw(Graph, Line) :-
  perm(Graph, Perm),
  [H|T] = Perm,
  X-Y = H,
  (   check(T, [X-Y], Y, Line) ; check(T, [Y-X], X, Line)).

% check(Remaining, List, Next, Line)
% checks to see if Remaining list can be extended from current List
% this happens if an element of the H of remaining list matches Next
% the other element of the H then becomes Next
% if there are no elements left in the remaining list, then the list is a Line
check([X1-Y1|T], List, Next, Line) :-
  (   X1 == Next ->  append(List, [X1-Y1], Builder), check(T, Builder, Y1, Line)
  ;   Y1 == Next, append(List, [Y1-X1], Builder), check(T, Builder, X1, Line)
  ).
check([], List, _, List).