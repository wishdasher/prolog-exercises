% increment_tree(+Tree0, ?Tree): Tree is obtained from binary tree
% Tree0 by incrementing each leaf value by 1.

increment_tree(leaf(L), T) :-
    S is L + 1,
    T = leaf(S).
increment_tree(node(Left, Right), T) :-
    increment_tree(Left, LT),
    increment_tree(Right, RT),
    T = node(LT, RT).


% tree_rightmost_value(+Tree, ?Value): Tree is a binary tree and
% the integer in its rightmost leaf is Value.

tree_rightmost_value(leaf(L), V) :-
    V = L.
tree_rightmost_value(node(_Left, Right), V) :-
    tree_rightmost_value(Right, V).


% tree_leaf_value(+Tree, +V): V is present as a leaf value in Tree.

tree_leaf_value(leaf(L), V) :-
    V == L;
    V = L.
tree_leaf_value(node(Left, Right), V) :-
    tree_leaf_value(Left, V);
    tree_leaf_value(Right, V).


% increment_list(+L0, ?L): L is a list of numbers obtained
% from L0 by incrementing each element by 1.
% You can assume that L0 is given and is a list of numbers.

increment_list([], []).
increment_list(E, L) :-
    number(E),
    E1 is E + 1,
    L = E1.
increment_list([H|T], L) :-
    increment_list(H, H1),
    increment_list(T, T1),
    L = [H1|T1].


% list_last(+List, ?V): List is a list whose last element is V.

list_last([E], V) :-
    V = E.
list_last([_H|T], V) :-
    list_last(T, V).


% list_element(+List, +V): V is present as an element in the list List.

list_element([], V) :-
    V = false.
list_element(E, V) :-
    E == V.
list_element([H|T], V) :-
    list_element(H, V);
    list_element(T, V).
