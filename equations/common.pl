:- ensure_loaded(math).


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
min_list([V], M) :-
	as_real(V, M).

min_list([V | Tail], Min) :-
	as_real(V, ReV),
	min_list(Tail, TailMin),
	Min is min(ReV, TailMin).

?-	min_list([1, -3, 5, 8], -3).
?-	min_list([42], 42).
?-	\+ min_list([], _).


% Calculates maximal value of the values in the list.
% max_list(List, Max)
max_list([V], M) :-
	as_real(V, M).

max_list([V | Tail], Max) :-
	as_real(V, ReV),
	max_list(Tail, TailMax),
	Max is max(ReV, TailMax).

?-	max_list([1, -3, 5, 8, 3], 8).
?-	max_list([42], 42).
?-	\+ max_list([], _).


% Evaluates A <= B.
% less_or_equal(A, B, IsLessOrEqual)
less_or_equal(A, B, Value) :-
	as_real(A, ReA),
	as_real(B, ReB),
	ReA =< ReB,
	!,
	Value = log_true.

less_or_equal(_, _, log_false).

?-	less_or_equal(8, 9, log_true).
?-	less_or_equal(8, 8, log_true).
?-	less_or_equal(8, 7, log_false).


% Evaluates A < B.
% less_than(A, B, IsLessOrEqual)
less_than(A, B, Value) :-
	as_real(A, ReA),
	as_real(B, ReB),
	ReA < ReB,
	!,
	Value = log_true.

less_than(_, _, log_false).

?-	less_than(8, 9, log_true).
?-	less_than(8, 8, log_false).
?-	less_than(8, 7, log_false).


% For each element of the list n-th element of sublist is obtained. These elements are collected int Result list.
% all_nth(Index, ListOfLists, Result)
all_nth0(_, [], []).

all_nth0(Index, [List | TailList], [Element | TailResult]) :-
	nth0(Index, List, Element),
	all_nth0(Index, TailList, TailResult).

?-	all_nth0(1, [[x, y, z], [a, b, c]], [y, b]).


write_list(Stream, [A, B | Tail]) :-
	write(Stream, A),
	write(Stream, ", "),
	write_list(Stream, B | Tail).

write_list(Stream, [A]) :-
	write(Stream, A).

write_list(_, []).



copy_variable(OrigVar, OrigTerm, CopiedVar, CopiedTerm) :-
	term_variables(OrigTerm, OrigVariableList),
	copy_term([OrigVar, OrigVariableList, OrigTerm], [CopiedVar, CopiedVariableList, CopiedTerm]),
	unify_other_vars(OrigVar, OrigVariableList, CopiedVariableList).


unify_other_vars(_, [], []).


unify_other_vars(OrigVar, [HeadOrigVar | TailOrigVar], [_ | TailCopiedVar]) :-
	OrigVar == HeadOrigVar,
	!,
	unify_other_vars(OrigVar, TailOrigVar, TailCopiedVar).

unify_other_vars(OrigVar, [HeadOrigVar | TailOrigVar], [HeadCopiedVar | TailCopiedVar]) :-
	HeadOrigVar = HeadCopiedVar,
	unify_other_vars(OrigVar, TailOrigVar, TailCopiedVar).


verify_variable_free(Variable, _) :-
	var(Variable),
	!.

verify_variable_free(Variable, Label) :-
	write("Variable "),
	write(Label),
	write(" is already bound to "),
	writeln(Variable),
	fail.



verify_free_variable_or_list(VariableOrList, LabelOrList) :-
	verify_variable_free(VariableOrList, LabelOrList).

verify_free_variable_or_list([], []).

verify_free_variable_or_list([Variable | VariableTail], [Label | LabelTail]) :-
	verify_variable_free(Variable, Label),
	verify_free_variable_or_list(VariableTail, LabelTail).


