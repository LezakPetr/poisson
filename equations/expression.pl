:- ensure_loaded(common).


% Predicate succeeds for boolean values.
boolean(log_false).
boolean(log_true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Logical operations %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Negation
% Y = not(X)
% log_not(X, Y)
log_not(log_false, log_true).
log_not(log_true, log_false).

% Logical or
% Y = A or B
% log_or(A, B, Y)
log_or(log_false, log_false, log_false).
log_or(log_false, log_true,  log_true).
log_or(log_true,  log_false, log_true).
log_or(log_true,  log_true,  log_true).

% Logical and
% Y = A and B
% log_and(A, B, Y)
log_and(log_false, log_false, log_false).
log_and(log_false, log_true,  log_false).
log_and(log_true,  log_false, log_false).
log_and(log_true,  log_true,  log_true).

% Implication
% Y = A => B
% log_impl(A, B, Y)
log_impl(log_false, log_false, log_true).
log_impl(log_false, log_true,  log_true).
log_impl(log_true,  log_false, log_false).
log_impl(log_true,  log_true,  log_true).

% Logical equivalence
% Y = A <=> B
% log_equiv(A, B, Y)
log_equiv(log_false, log_false, log_true).
log_equiv(log_false, log_true,  log_false).
log_equiv(log_true,  log_false, log_false).
log_equiv(log_true,  log_true,  log_true).

% Logical and over list
log_and_all([V | Tail], Value) :-
	log_and_all(Tail, TailValue),
	log_and(V, TailValue, Value).

log_and_all([], log_true).

?- log_and_all([], log_true).
?- log_and_all([log_true, log_true], log_true).
?- log_and_all([log_false, log_true], log_false).
?- log_and_all([log_true, log_false], log_false).


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


% Evaluates function. Arguments must be evaluated.
evaluate_function(X, X) :-
	boolean(X).

evaluate_function(par(X), X).

evaluate_function(not(X), Value) :-
	log_not(X, Value).

evaluate_function(or(A, B), Value) :-
	log_or(A, B, Value).

evaluate_function(and(A, B), Value) :-
	log_and(A, B, Value).

evaluate_function(impl(A, B), Value) :-
	log_impl(A, B, Value).

evaluate_function(equiv(A, B), Value) :-
	log_equiv(A, B, Value).

evaluate_function(X, X) :-
	number(X).

evaluate_function(List, EvaluatedList) :-
	is_list(List),
	evaluate_list(List, EvaluatedList).

evaluate_function(intersection(set(A), set(B)), set(Value)) :-
	set_intersection(A, B, Value).

evaluate_function(union(set(A), set(B)), set(Value)) :-
	set_union(A, B, Value).

evaluate_function(difference(set(A), set(B)), set(Value)) :-
	set_difference(A, B, Value).

evaluate_function(set_equal(set(A), set(B)), Value) :-
	set_equal(A, B, Value).

evaluate_function(A + B, Y) :-
	number(A),
	number(B),
	Y is A + B.

evaluate_function(A - B, Y) :-
	number(A),
	number(B),
	Y is A - B.

evaluate_function(A * B, Y) :-
	number(A),
	number(B),
	Y is A * B.

evaluate_function(A / B, Y) :-
	number(A),
	number(B),
	Y is A / B.

evaluate_function(num_equal(List, Tolerance), Value) :-
	min_list(List, Min),
	max_list(List, Max),
	less_or_equal(Max - Min, Tolerance, Value).


?-	make_set([a, b, c], A),
	make_set([b, c, d], B),
	make_set([b, c], Y),
	evaluate_function(intersection(set(A), set(B)), set(Y)).

?-	make_set([b, c, d], A),
	make_set([a, b, c], B),
	make_set([a, b, c, d], Y),
	evaluate_function(union(set(A), set(B)), set(Y)).

?-	make_set([b, c, d, e], A),
	make_set([a, b, c], B),
	make_set([d, e], Y),
	evaluate_function(difference(set(A), set(B)), set(Y)).

?-	evaluate_function(2 + 3, 5).
?-	evaluate_function(2.0 + 3.0, 5.0).

?-	evaluate_function(2 - 7, -5).
?-	evaluate_function(2.0 - 7.0, -5.0).

?-	evaluate_function(2 * 3, 6).
?-	evaluate_function(2.0 * 3.0, 6.0).

?-	evaluate_function(6 / 2, 3).
?-	evaluate_function(6.0 / 2.0, 3.0).
?-	evaluate_function(1 / 2, 0.5).

?-	evaluate_function(num_equal([0.35, 0.35, 0.36], 0.011), log_true).
?-	evaluate_function(num_equal([0.35, 0.35, 0.36], 0.009), log_false).

evaluate_list([], []).

evaluate_list([Expression | Tail], [EvaluatedExpression | EvaluatedTail]) :-
	evaluate_expression(Expression, EvaluatedExpression),
	evaluate_list(Tail, EvaluatedTail).

evaluate_expression(declare_statement(StatementVariable, _, SubFormula), Value) :-
	!,
	findall(SubFormulaValue, (boolean(StatementVariable), evaluate_expression(SubFormula, SubFormulaValue)), [EF1, EF2]),
	log_and(EF1, EF2, Value).

evaluate_expression(declare_set(SetVariable, _, TestedSets, SubFormula), Value) :-
	!,
	findall(SubFormulaValue, (member(SetVariable, TestedSets), evaluate_expression(SubFormula, SubFormulaValue)), Results),
	log_and_all(Results, Value).

evaluate_expression(Expression, Value) :-
	Expression =.. [Functor | Args],
	evaluate_list(Args, EvaluatedArgs),
	SubEvaluatedExpression =.. [Functor | EvaluatedArgs],
	evaluate_function(SubEvaluatedExpression, Value).


?-	evaluate_expression(3*4 - 2*5, 2).
?-	evaluate_expression(3 * (4 - 2) * 5, 30).

?-	evaluate_expression(num_equal([1/(2+1), 2 / 6, 1 - (2/3)], 1e-14), log_true).
?-	evaluate_expression(num_equal([1, 2], 1e-14), log_false).

?-	make_set([a, b, c], S1),
	make_set([b, c, d], S2),
	make_set([d, e], S3),
	evaluate_expression(
		declare_set(A, 'A', [set(S1), set(S2), set(S3)], declare_set(B, 'B', [set(S1), set(S2), set(S3)],
			set_equal(
				intersection(A, B),
				intersection(B, A)
			)
		)),
		log_true
	).

?-	make_set([a, b, c], S1),
	make_set([b, c, d], S2),
	make_set([d, e], S3),
	evaluate_expression(
		declare_set(A, 'A', [set(S1), set(S2), set(S3)], declare_set(B, 'B', [set(S1), set(S2), set(S3)],
			set_equal(
				intersection(A, B),
				union(A, B)
			)
		)),
		log_true
	).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Formula print %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

% Prints formula with given label to file calculated 
print_expression(Label, Formula) :-
	calc_file_path('eq', Label, Path),
	open(Path, write, Stream),
	writeln(Stream, '\\begin{equation}'),
	print_label(Stream, 'eq:', Label),
	print_expression_term(Stream, Formula),
	writeln(Stream, ''),
	writeln(Stream, '\\end{equation}'),
	close(Stream).


% Prints boolean value into stream
print_boolean(Stream, log_true) :-
	write(Stream, '\\true').
		
print_boolean(Stream, log_false) :-
	write(Stream, '\\false').


% Prints boolean value enclosed in math mode into stream
print_boolean_in_math_mode(Stream, B) :-
	write(Stream, '\\('),
	print_boolean(Stream, B),
	write(Stream, '\\)').


% Prints term of formula into stream
print_expression_term(Stream, F) :-
	print_expression_term(Stream, F, root).

print_expression_term(Stream, F, _) :-
	atom(F),
	write(Stream, F).

print_expression_term(Stream, declare_statement(V, L, F), PR) :-
	atomic_list_concat(['\\predicate{', L, '}'], V),
	print_expression_term(Stream, F, PR).

print_expression_term(Stream, impl(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, impl), 
	print_expression_term(Stream, A, impl),
	write(Stream, ' \\impl '),
	print_expression_term(Stream, B, impl),
	print_bracket_if_needed(Stream, ')', PR, impl).

print_expression_term(Stream, equiv(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, equiv), 
	print_expression_term(Stream, A, equiv),
	write(Stream, ' \\equivalent '),
	print_expression_term(Stream, B, equiv),
	print_bracket_if_needed(Stream, ')', PR, equiv).

print_expression_term(Stream, or(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, or), 
	print_expression_term(Stream, A, or),
	write(Stream, ' \\lor '),
	print_expression_term(Stream, B, or),
	print_bracket_if_needed(Stream, ')', PR, or).

print_expression_term(Stream, and(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, and), 
	print_expression_term(Stream, A, and),
	write(Stream, ' \\land '),
	print_expression_term(Stream, B, and),
	print_bracket_if_needed(Stream, ')', PR, and).

print_expression_term(Stream, par(F), _) :-
	write(Stream, '('),
	print_expression_term(Stream, F, root),
	write(Stream, ')').

print_expression_term(Stream, not(F), _) :-
	write(Stream, '\\overline{'),
	print_expression_term(Stream, F, root),
	write(Stream, '}').


% Prints bracket if it is needed.
% print_bracket_if_needed(Stream, Bracket, SuperOperator, SubOperator)
print_bracket_if_needed(_, _, SuperOperator, SubOperator) :-
	bracket_not_needed(SuperOperator, SubOperator),
	!.

print_bracket_if_needed(Stream, Bracket, _, _) :-
	write(Stream, Bracket).


% Succeeds if bracket is not needed.
% bracket_not_needed(SuperOperator, SubOperator)
bracket_not_needed(root, _).
bracket_not_needed(or, or).
bracket_not_needed(and, and).
bracket_not_needed(or, and).
bracket_not_needed(impl, and).
bracket_not_needed(equiv, and).
bracket_not_needed(impl, or).
bracket_not_needed(equiv, or).
bracket_not_needed(equiv, impl).

