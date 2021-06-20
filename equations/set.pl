%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Set operations %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

% Creates a set.
% make_set(List, Set)
make_set(List, Set) :-
	sort(List, Set).

?- make_set([c, b, a, b], [a, b, c]). 

% Checks if value is in set.
in_set(V, [V | _]).

in_set(V, [_ | Tail]) :-
	in_set(V, Tail).

?- make_set([a, b, c], S), in_set(b, S).

?- make_set([a, b, c], S), \+ in_set(d, S).

log_in_set(Value, Set, log_true) :-
	in_set(Value, Set),
	!.       

log_in_set(_, _, log_false).

?- make_set([a, b, c], S), log_in_set(b, S, log_true).

?- make_set([a, b, c], S), log_in_set(d, S, log_false).


% Checks if value is in set of natural numbers
log_in_naturals(X, log_true) :-
	integer(X),
	X >= 1,
	!.

log_in_naturals(_, log_false).

?- log_in_naturals(42, log_true).
?- log_in_naturals(0, log_false).
?- log_in_naturals(-25, log_false).
?- log_in_naturals(1.25, log_false).
?- log_in_naturals(something, log_false).

% Checks if value is in set of integers
log_in_integers(X, log_true) :-
	integer(X),
	!.

log_in_integers(_, log_false).


% Checks if value is in set of real numbers
log_in_reals(X, log_true) :-
	number(X),
	!.

log_in_reals(_, log_false).

% Checks if value is in set of complex numbers
log_in_complex(X, log_true) :-
	as_complex(X, _),
	!.

log_in_complex(_, log_false).


?- log_in_integers(42, log_true).
?- log_in_integers(0, log_true).
?- log_in_integers(-25, log_true).
?- log_in_integers(1.25, log_false).
?- log_in_integers(something, log_false).

% Calculates set intersection.
% set_intersection(A, B, Intersection)
set_intersection([V | Tail], B, [V | IntersectionTail]) :-
	in_set(V, B),
	set_intersection(Tail, B, IntersectionTail),
	!.

set_intersection([_ | Tail], B, IntersectionTail) :-
	set_intersection(Tail, B, IntersectionTail).

set_intersection([], _, []).


?- 	make_set([a, b, c], A),
	make_set([b, c, d], B),
	make_set([b, c], Y),
	set_intersection(A, B, Y).

% Calculates set union.
% set_union(A, B, Union)
set_union_impl([V | Tail], B, UnionTail) :-
	in_set(V, B),
	set_union_impl(Tail, B, UnionTail),
	!.

set_union_impl([V | Tail], B, [V | UnionTail]) :-
	set_union_impl(Tail, B, UnionTail).

set_union_impl([], B, B).

set_union(A, B, Union) :-
	set_union_impl(A, B, Merged),
	sort(Merged, Union).

?-	make_set([b, c, d], A),
	make_set([a, b, c], B),
	make_set([a, b, c, d], Y),
	set_union(A, B, Y).

% Calculates set difference.
% set_union(A, B, Difference)
set_difference([V | Tail], B, DifferenceTail) :-
	in_set(V, B),
	!,
	set_difference(Tail, B, DifferenceTail).

set_difference([V | Tail], B, [V | DifferenceTail]) :-
	set_difference(Tail, B, DifferenceTail).

set_difference([], _, []).

?-	make_set([b, c, d, e], A),
	make_set([a, b, c], B),
	make_set([d, e], Y),
	set_difference(A, B, Y).


set_equal(X, X, log_true) :-
	!.

set_equal(_, _, log_false).

?-	make_set([a, b, c], A),
	make_set([b, c, a], B),
	set_equal(A, B, log_true).

?-	make_set([a, b, c], A),
	make_set([b, c, d], B),
	set_equal(A, B, log_false).


subset([], _).

subset([V | Tail], SuperSet) :-
	in_set(V, SuperSet),
	subset(Tail, SuperSet).

?-	make_set([a, b, c], A),
	make_set([a, b, c, d, e], B),
	subset(A, B).

?-	make_set([a, b, c], A),
	make_set([a, b, d, e], B),
	\+ subset(A, B).


log_subset(A, B, Value) :-
	subset(A, B),
	!,
	Value = log_true.

log_subset(_, _, log_false).

?-	make_set([a, b, c], A),
	make_set([a, b, c, d, e], B),
	log_subset(A, B, log_true).

?-	make_set([a, b, c], A),
	make_set([a, b, d, e], B),
	log_subset(A, B, log_false).


