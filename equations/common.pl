
% Calculates path to the file for given label
calc_file_path(Prefix, Label, Path) :-
	atomic_list_concat(['../out/', Prefix, '_', Label, '.tex'], Path).

% Prints label
print_label(Stream, Prefix, Label) :-
	write(Stream, '\\label{'),
	write(Stream, Prefix),
	write(Stream, Label),
	writeln(Stream, '}').

% Prints horizontal line
print_hline(Stream) :-
	writeln(Stream, '\\hline').


% Prints given text in case that list is not empty.
% print_if_not_empty(Stream, Text, List)
print_if_not_empty(_, _, []).

print_if_not_empty(Stream, Text, [_ | _]) :-
	write(Stream, Text).


% Calculates minimal value of the values in the list.
% min_list(List, Min)
min_list([V], V) :-
	number(V).

min_list([V | Tail], Min) :-
	number(V),
	min_list(Tail, TailMin),
	Min is min(V, TailMin).

?-	min_list([1, -3, 5, 8], -3).
?-	min_list([42], 42).
?-	\+ min_list([], _).


% Calculates maximal value of the values in the list.
% max_list(List, Max)
max_list([V], V) :-
	number(V).

max_list([V | Tail], Max) :-
	number(V),
	max_list(Tail, TailMax),
	Max is max(V, TailMax).

?-	max_list([1, -3, 5, 8, 3], 8).
?-	max_list([42], 42).
?-	\+ max_list([], _).


% Evaluates A <= B.
% less_or_equal(A, B, IsLessOrEqual)
less_or_equal(A, B, log_true) :-
	A =< B,
	!.

less_or_equal(_, _, log_false).

?-	less_or_equal(5 + 3, 9, log_true).
?-	less_or_equal(5 + 3, 8, log_true).
?-	less_or_equal(5 + 3, 7, log_false).


% Evaluates A < B.
% less_than(A, B, IsLessOrEqual)
less_than(A, B, log_true) :-
	A < B,
	!.

less_than(_, _, log_false).

?-	less_than(5 + 3, 9, log_true).
?-	less_than(5 + 3, 8, log_false).
?-	less_than(5 + 3, 7, log_false).

