:- ensure_loaded(common).


% Predicate succeeds for boolean values.
boolean(log_false).
boolean(log_true).

hint(linebreak).


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

% Logical or over list
log_or_all([V | Tail], Value) :-
	log_or_all(Tail, TailValue),
	log_or(V, TailValue, Value).

log_or_all([], log_false).

?- log_or_all([], log_false).
?- log_or_all([log_false, log_false], log_false).
?- log_or_all([log_false, log_true], log_true).
?- log_or_all([log_true, log_false], log_true).

% Logical and over list
log_and_all([V | Tail], Value) :-
	log_and_all(Tail, TailValue),
	log_and(V, TailValue, Value).

log_and_all([], log_true).

?- log_and_all([], log_true).
?- log_and_all([log_true, log_true], log_true).
?- log_and_all([log_false, log_true], log_false).
?- log_and_all([log_true, log_false], log_false).

log_equiv_all([A, B | Tail], Value) :-
	log_equiv(A, B, ABEquiv),
	log_equiv_all([B | Tail], BTailEquiv),
	log_and(ABEquiv, BTailEquiv, Value).

log_equiv_all([_], log_true).

?- log_equiv_all([log_true, log_true, log_true], log_true).
?- log_equiv_all([log_false, log_false, log_false], log_true).
?- log_equiv_all([log_true, log_false, log_true], log_false).


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


% Evaluates function. Arguments must be evaluated.
evaluate_function(X, X) :-
	boolean(X).

evaluate_function(X, X) :-
	hint(X).

evaluate_function(par(X), X).

evaluate_function(set(S), set(S)).

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

evaluate_function(equiv([A | Tail]), Value) :-
	log_equiv_all([A | Tail], Value).

evaluate_function(X, X) :-
	number(X).

evaluate_function(set_of(Values), set(Set)) :-
	make_set(Values, Set).

evaluate_function(empty_set, set(Value)) :-
	make_set([], Value).

evaluate_function(natural_numbers, natural_numbers).
evaluate_function(integers, integers).

evaluate_function(in(X, set(S)), Value) :-
	log_in_set(X, S, Value).

evaluate_function(in(X, natural_numbers), Value) :-
	log_in_naturals(X, Value).

evaluate_function(in(X, integers), Value) :-
	log_in_integers(X, Value).

evaluate_function(not_in(X, S), Value) :-
	evaluate_function(in(X, S), InSet),
	log_not(InSet, Value).

evaluate_function(intersection(set(A), set(B)), set(Value)) :-
	set_intersection(A, B, Value).

evaluate_function(union(set(A), set(B)), set(Value)) :-
	set_union(A, B, Value).

evaluate_function(difference(set(A), set(B)), set(Value)) :-
	set_difference(A, B, Value).

evaluate_function(set_equal([set(A), set(B) | Tail]), Value) :-
	set_equal(A, B, SetsEqual),
	evaluate_function(set_equal([set(B) | Tail]), RestEqual),
	log_and(SetsEqual, RestEqual, Value).

evaluate_function(set_equal([set(_)]), log_true).

evaluate_function(set_equal(set(A), set(B)), Value) :-
	set_equal(A, B, Value).

evaluate_function(set_not_equal(set(A), set(B)), Value) :-
	set_equal(A, B, SetsEqual),
	log_not(SetsEqual, Value).

evaluate_function(subset(set(A), set(B)), Value) :-
	log_subset(A, B, Value).

evaluate_function(superset(set(A), set(B)), Value) :-
	log_subset(B, A, Value).

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

evaluate_list([Expression | Tail], EvaluatedTail) :-
	hint(Expression),
	!,
	evaluate_list(Tail, EvaluatedTail).

evaluate_list([Expression | Tail], [EvaluatedExpression | EvaluatedTail]) :-
	evaluate_expression(Expression, EvaluatedExpression),
	evaluate_list(Tail, EvaluatedTail).


evaluate_expression_or_fail(Expression, Value) :-
	evaluate_expression(Expression, Value),
	!.

evaluate_expression_or_fail(_, failed).


evaluate_expression(declare_statement(StatementVariable, _, SubFormula), Value) :-
	!,
	findall(SubFormulaValue, (boolean(StatementVariable), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), [EF1, EF2]),
	log_and(EF1, EF2, Value).

evaluate_expression(declare_set(SetVariable, _, TestedSets, SubFormula), Value) :-
	!,
	findall(SubFormulaValue, (member(SetVariable, TestedSets), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_and_all(Results, Value).

evaluate_expression(declare_predicate(PredicateVariable, _, TestedPredicates, SubFormula), Value) :-
	!,
	findall(SubFormulaValue, (member(PredicateVariable, TestedPredicates), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_and_all(Results, Value).

evaluate_expression(apply(Predicate, Args, ArgValues), Value) :-
	!,
	findall(SubFormulaValue, (Args = ArgValues, evaluate_expression_or_fail(Predicate, SubFormulaValue)), [Value]).

evaluate_expression(declare_variable(Variable, _, TestedValues, SubFormula), Value) :-
	!,
	findall(SubFormulaValue, (member(Variable, TestedValues), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_and_all(Results, Value).

evaluate_expression(forall(Variable, _, TestedValues, SubFormula), Value) :-
	!,
	findall(SubFormulaValue, (member(Variable, TestedValues), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_and_all(Results, Value).

evaluate_expression(forall_in(Variable, _, Set, TestedValues, SubFormula), Value) :-
	evaluate_expression(forall(Variable, _, TestedValues, impl(in(Variable, Set), SubFormula)), Value).

evaluate_expression(exists(Variable, _, TestedValues, SubFormula), Value) :-
	!,
	findall(SubFormulaValue, (member(Variable, TestedValues), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_or_all(Results, Value).

evaluate_expression(exists_in(Variable, _, Set, TestedValues, SubFormula), Value) :-
	evaluate_expression(exists(Variable, _, TestedValues, and(in(Variable, Set), SubFormula)), Value).

evaluate_expression(set_by(Variable, _, Values, Formula), Value) :-
	!,
	findall(Variable, (member(Variable, Values), evaluate_expression_or_fail(Formula, log_true)), Results),
	make_set(Results, ResultsSet),
	Value = set(ResultsSet).

evaluate_expression(List, EvaluatedList) :-
	is_list(List),
	!,
	evaluate_list(List, EvaluatedList).

evaluate_expression(Expression, Value) :-
	Expression =.. [Functor | Args],
	evaluate_list(Args, EvaluatedArgs),
	SubEvaluatedExpression =.. [Functor | EvaluatedArgs],
	evaluate_function(SubEvaluatedExpression, Value).


?-	evaluate_list([log_true, linebreak, log_false], [log_true, log_false]).
?-	evaluate_expression([log_true, linebreak, log_false], [log_true, log_false]).

?-	evaluate_expression(3*4 - 2*5, 2).
?-	evaluate_expression(3 * (4 - 2) * 5, 30).

?-	evaluate_expression(num_equal([1/(2+1), 2 / 6, 1 - (2/3)], 1e-14), log_true).
?-	evaluate_expression(num_equal([1, 2], 1e-14), log_false).

?-	make_set([1, 2], S1),
	make_set([1, 2], S2),
	evaluate_expression(set_equal([set(S1), set(S2)]), log_true).

?-	make_set([1, 2], S1),
	make_set([1, 2], S2),
	evaluate_expression(set_equal(set(S1), set(S2)), log_true).

?-	make_set([1, 2], S1),
	make_set([1, 2], S2),
	make_set([1, 2], S3),
	evaluate_expression(set_equal([set(S1), set(S2), set(S3)]), log_true).

?-	make_set([1, 2], S1),
	make_set([1, 3], S2),
	make_set([1, 2], S3),
	evaluate_expression(set_equal([set(S1), set(S2), set(S3)]), log_false).

?-	evaluate_expression(set_equal([empty_set, empty_set]), log_true).

?-	make_set([1, 2], S1),
	make_set([1, 2], S2),
	evaluate_expression(set_not_equal(set(S1), set(S2)), log_false).

?-	make_set([1, 2], S1),
	make_set([1, 3], S2),
	evaluate_expression(set_not_equal(set(S1), set(S2)), log_true).

?-	evaluate_expression(set_not_equal(empty_set, empty_set), log_false).

?-	make_set([1, 2, 3], S1),
	make_set([2, 3, 4], S2),
	make_set([4, 5], S3),
	evaluate_expression(
		declare_set(A, 'A', [set(S1), set(S2), set(S3)], declare_set(B, 'B', [set(S1), set(S2), set(S3)],
			set_equal([
				intersection(A, B),
				intersection(B, A)
			])
		)),
		log_true
	).

?-	make_set([1, 2, 3], S1),
	make_set([2, 3, 4], S2),
	make_set([4, 5], S3),
	evaluate_expression(
		declare_set(A, 'A', [set(S1), set(S2), set(S3)], declare_set(B, 'B', [set(S1), set(S2), set(S3)],
			set_equal([
				intersection(A, B),
				union(A, B)
			])
		)),
		log_false
	).

?-	P = X * Y,
	evaluate_expression(
		apply(P, [X, Y], [3, 4]) + apply(P, [X, Y], [5, 6]),
		42
	).

?-	evaluate_expression(
		forall(X, 'X', [1, 2], num_equal([X, 1], 0)),
		log_false
	).

?-	evaluate_expression(
		forall(X, 'X', [1, 2], or(num_equal([X, 1], 0), num_equal([X, 2], 0))),
		log_true
	).

?-	evaluate_expression(
		exists(X, 'X', [1, 2], num_equal([X, 3], 0)),
		log_false
	).

?-	evaluate_expression(
		exists(X, 'X', [1, 2], num_equal([X, 2], 0)),
		log_true
	).

?-	make_set([1, 2], Value),
	evaluate_expression(
		set_by(X, 'x', [1, 2, 3, 4], or(num_equal([X, 1], 0), num_equal([X, 2], 0))),
		set(Value)
	).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Formula print %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

% Prints formula with given label to file calculated 
print_expression(Label, Formula) :-
	calc_file_path('eq', Label, Path),
	open(Path, write, Stream),
	print_expression_term(Stream, Formula),
	writeln(Stream, ''),
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

print_expression_term(Stream, B, _) :-
	boolean(B),
	!,
	print_boolean(Stream, B).

print_expression_term(Stream, N, _) :-
	number(N),
	!,
	write(Stream, N).

print_expression_term(Stream, empty_set, _) :-
	!,
	write(Stream, ' \\emptyset ').

print_expression_term(Stream, natural_numbers, _) :-
	!,
	write(Stream, ' \\naturalnumbers ').

print_expression_term(Stream, integers, _) :-
	!,
	write(Stream, ' \\integers ').

print_expression_term(Stream, set_of(Values), _) :-
	!,
	write(Stream, ' \\{ '),
	print_expression_list(Stream, Values),
	write(Stream, ' \\} ').

print_expression_term(Stream, F, _) :-
	atom(F),
	write(Stream, F).

print_expression_term(Stream, declare_statement(Value, Label, SubFormula), PR) :-
	atomic_list_concat(['\\predicate{', Label, '}'], Value),
	print_expression_term(Stream, SubFormula, PR).

print_expression_term(Stream, declare_predicate(Variable, Label, _, SubFormula), PR) :-
	Variable = Label,
	print_expression_term(Stream, SubFormula, PR).

print_expression_term(Stream, declare_set(Variable, Label, _, SubFormula), PR) :-
	Variable = Label,
	print_expression_term(Stream, SubFormula, PR).

print_expression_term(Stream, impl(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, impl, ' \\impl ').

print_expression_term(Stream, equiv(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, equiv, ' \\equivalent ').

print_expression_term(Stream, equiv([A | Tail]), PR) :-
	print_bracket_if_needed(Stream, '(', PR, equiv),
	print_chain(Stream, [A | Tail], ' \\equivalent ', equiv), 
	print_bracket_if_needed(Stream, ')', PR, equiv).

print_expression_term(Stream, or(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, or, ' \\lor ').

print_expression_term(Stream, and(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, and, ' \\land ').

print_expression_term(Stream, par(F), _) :-
	write(Stream, '('),
	print_expression_term(Stream, F, root),
	write(Stream, ')').

print_expression_term(Stream, not(F), _) :-
	write(Stream, '\\overline{'),
	print_expression_term(Stream, F, root),
	write(Stream, '}').

print_expression_term(Stream, declare_variable(Variable, Label, _, SubFormula), PR) :-
	Variable = Label,
	print_expression_term(Stream, SubFormula, PR).

print_expression_term(Stream, forall(Variable, Label, _, SubFormula), PR) :-
	Variable = Label,
	print_bracket_if_needed(Stream, '(', PR, forall), 
	write(Stream, ' \\forall '),
	write(Stream, Variable),
	write(Stream, ' \\ '),
	print_expression_term(Stream, SubFormula, forall),
	print_bracket_if_needed(Stream, ')', PR, forall).

print_expression_term(Stream, forall_in(Variable, Label, Set, _, SubFormula), PR) :-
	Variable = Label,
	print_bracket_if_needed(Stream, '(', PR, forall), 
	write(Stream, ' \\forall '),
	write(Stream, Variable),
	write(Stream, ' \\in '),
	print_expression_term(Stream, Set, in),
	write(Stream, ' \\ '),
	print_expression_term(Stream, SubFormula, forall),
	print_bracket_if_needed(Stream, ')', PR, forall).

print_expression_term(Stream, exists(Variable, Label, _, SubFormula), PR) :-
	Variable = Label,
	print_bracket_if_needed(Stream, '(', PR, exists), 
	write(Stream, ' \\exists '),
	write(Stream, Variable),
	write(Stream, ' \\ '),
	print_expression_term(Stream, SubFormula, exists),
	print_bracket_if_needed(Stream, ')', PR, exists).

print_expression_term(Stream, exists_in(Variable, Label, Set, _, SubFormula), PR) :-
	Variable = Label,
	print_bracket_if_needed(Stream, '(', PR, exists), 
	write(Stream, ' \\exists '),
	write(Stream, Variable),
	write(Stream, ' \\in '),
	print_expression_term(Stream, Set, in),
	write(Stream, ' \\ '),
	print_expression_term(Stream, SubFormula, exists),
	print_bracket_if_needed(Stream, ')', PR, exists).

print_expression_term(Stream, apply(Predicate, _, ArgValues), _) :-
	write(Stream, '\\predicate{'),
	write(Stream, Predicate),
	write(Stream, '}('),
	print_predicate_args(Stream, ArgValues),
	write(Stream, ')').

print_expression_term(Stream, set_by(Variable, Label, _, Formula), _) :-
	Variable = Label,
	write(Stream, '\\{'),
	write(Stream, Label),
	write(Stream, ' : '),
	print_expression_term(Stream, Formula, root),
	write(Stream, '\\}').

print_expression_term(Stream, set_equal(List), PR) :-
	is_list(List),
	print_bracket_if_needed(Stream, '(', PR, equal),
	print_chain(Stream, List, ' = ', equal), 
	print_bracket_if_needed(Stream, ')', PR, equal).

print_expression_term(Stream, set_equal([A]), _) :-
	print_expression_term(Stream, A, eq).

print_expression_term(Stream, set_equal(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, equal, ' = ').

print_expression_term(Stream, set_not_equal(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, plus, ' \\neq ').

print_expression_term(Stream, subset(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, subset, ' \\subset ').

print_expression_term(Stream, superset(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, subset, ' \\supset ').

print_expression_term(Stream, in(X, S), PR) :-
	print_binary_operator(Stream, X, S, PR, in, ' \\in ').

print_expression_term(Stream, not_in(X, S), PR) :-
	print_binary_operator(Stream, X, S, PR, in, ' \\notin ').

print_expression_term(Stream, union(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, union, ' \\cup ').

print_expression_term(Stream, intersection(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, intersection, ' \\cap ').

print_expression_term(Stream, difference(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, difference, ' \\setminus ').

print_expression_term(Stream, A + B, PR) :-
	print_binary_operator(Stream, A, B, PR, plus, ' + ').

print_expression_term(Stream, A - B, PR) :-
	print_binary_operator(Stream, A, B, PR, minus, ' - ').

print_expression_term(Stream, A * B, PR) :-
	print_binary_operator(Stream, A, B, PR, multiply, ' \\cdot ').

print_expression_term(Stream, num_equal(List, _), PR) :-
	print_bracket_if_needed(Stream, '(', PR, equal),
	print_chain(Stream, List, ' = ', equal),
	print_bracket_if_needed(Stream, ')', PR, equal).


print_binary_operator(Stream, A, B, SuperOperator, SubOperator, Label) :-
	print_bracket_if_needed(Stream, '(', SuperOperator, SubOperator),
	print_expression_term(Stream, A, SubOperator),
	write(Stream, Label),
	print_expression_term(Stream, B, SubOperator),
	print_bracket_if_needed(Stream, ')', SuperOperator, SubOperator).



print_predicate_args(Stream, [Arg, Next | Tail]) :-
	write(Stream, Arg),
	write(Stream, ', '),
	print_predicate_args(Stream, [Next | Tail]).

print_predicate_args(Stream, [Arg]) :-
	write(Stream, Arg).

print_predicate_args(_, []).


print_expression_list(Stream, [Arg, Next | Tail]) :-
	print_expression_term(Stream, Arg, root),
	write(Stream, ', '),
	print_expression_list(Stream, [Next | Tail]).

print_expression_list(Stream, [Arg]) :-
	print_expression_term(Stream, Arg, root).

print_expression_list(_, []).


print_chain(Stream, [A | Tail], Delim, Operator) :-
	hint(A),
	!,
	print_hint(Stream, A),
	print_chain(Stream, Tail, Delim, Operator).

print_chain(Stream, [A, B | Tail], Delim, Operator) :-
	print_expression_term(Stream, A, Operator),
	write(Stream,  Delim),
	print_chain(Stream, [B | Tail], Delim, Operator).

print_chain(Stream, [A], _, Operator) :-
	print_expression_term(Stream, A, Operator).


% Prints bracket if it is needed.
% print_bracket_if_needed(Stream, Bracket, SuperOperator, SubOperator)
print_bracket_if_needed(_, _, SuperOperator, SubOperator) :-
	bracket_not_needed(SuperOperator, SubOperator),
	!.

print_bracket_if_needed(Stream, Bracket, _, _) :-
	write(Stream, Bracket).



operator_priority(multiply, 201).
operator_priority(plus, 200).
operator_priority(minus, 200).

operator_priority(in, 102).
operator_priority(difference, 101).
operator_priority(union, 101).
operator_priority(intersection, 101).
operator_priority(equal, 100).

operator_priority(not, 6).
operator_priority(forall, 5).
operator_priority(exists, 4).
operator_priority(and, 3).
operator_priority(or, 2).
operator_priority(impl, 1).
operator_priority(equiv, 0).

operator_priority(root, -1).

% Succeeds if bracket is not needed.
% bracket_not_needed(SuperOperator, SubOperator)
bracket_not_needed(SuperOperator, SubOperator) :-
	operator_priority(SuperOperator, SuperOperatorPriority),
	operator_priority(SubOperator, SubOperatorPriority),
	SuperOperatorPriority < SubOperatorPriority.

bracket_not_needed(or, or).
bracket_not_needed(and, and).
bracket_not_needed(plus, plus).
bracket_not_needed(multiply, multiply).

bracket_not_needed(SuperOperator, SubOperator) :-
	quantifier(SuperOperator),
	quantifier(SubOperator).

quantifier(forall).
quantifier(exists).

print_hint(Stream, linebreak) :-
	writeln(Stream, ' \\\\').

