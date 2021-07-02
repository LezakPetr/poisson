:- ensure_loaded(common).
:- ensure_loaded(math).
:- ensure_loaded(expression_common).
:- ensure_loaded(logic).
:- ensure_loaded(set).


log_complex_equal(A, B, Equal) :-
	complex_equal(A, B),
	!,
	Equal = log_true.

log_complex_equal(_, _, log_false).


% Evaluates function. Arguments must be evaluated.
evaluate_function(X, X) :-
	boolean(X).

evaluate_function(X, X) :-
	hint(X).

evaluate_function(par(X), X).
evaluate_function(nopar(X), X).
evaluate_function(line(X), X).

evaluate_function(set(S), set(S)).

evaluate_function(not(X), Value) :-
	log_not(X, Value).

evaluate_function(or(A, B), Value) :-
	log_or(A, B, Value).

evaluate_function(and(A, B), Value) :-
	log_and(A, B, Value).

evaluate_function(equiv(A, B), Value) :-
	log_equiv(A, B, Value).

evaluate_function(equiv([A | Tail]), Value) :-
	log_equiv_all([A | Tail], Value).

evaluate_function(proof(Axioms, Conclusions), Value) :-
	log_and_all(Axioms, AxiomsTrue),
	log_and_all(Conclusions, ConclusionsTrue),
	log_impl(AxiomsTrue, ConclusionsTrue, Value).

evaluate_function(X, X) :-
	number(X).

evaluate_function(imag, complex(0, 1)).

evaluate_function(e, Value) :-
	Value is e.

evaluate_function(pi, Value) :-
	Value is pi.

evaluate_function(complex(Re, Im), complex(Re, Im)).

evaluate_function(set_of(Values), set(Set)) :-
	make_set(Values, Set).

evaluate_function(empty_set, set(Value)) :-
	make_set([], Value).

evaluate_function(natural_numbers, natural_numbers).
evaluate_function(integers, integers).
evaluate_function(nonnegative_integers, nonnegative_integers).
evaluate_function(rational_numbers, rational_numbers).
evaluate_function(real_numbers, real_numbers).
evaluate_function(positive_real_numbers, positive_real_numbers).
evaluate_function(complex_numbers, complex_numbers).

evaluate_function(in(X, set(S)), Value) :-
	log_in_set(X, S, Value).

evaluate_function(in(X, natural_numbers), Value) :-
	log_in_naturals(X, Value).

evaluate_function(in(X, integers), Value) :-
	log_in_integers(X, Value).

evaluate_function(in(X, nonnegative_integers), Value) :-
	log_in_nonnegative_integers(X, Value).

evaluate_function(in(X, rational_numbers), Value) :-
	log_in_reals(X, Value).

evaluate_function(in(X, real_numbers), Value) :-
	log_in_reals(X, Value).

evaluate_function(in(X, positive_real_numbers), Value) :-
	log_in_reals(X, IsReal),
	less_than(0, X, IsPositive),
	log_and(IsReal, IsPositive, Value).

evaluate_function(in(X, complex_numbers), Value) :-
	log_in_complex(X, Value).

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
	complex_add(A, B, Y).

evaluate_function(A - B, Y) :-
	complex_subtract(A, B, Y).

evaluate_function(plus_minus(A, B, PM), Y) :-
	complex_multiply(B, PM, C),
	complex_add(A, C, Y).

evaluate_function(-A, Y) :-
	complex_negate(A, Y).

evaluate_function(A * B, Y) :-
	complex_multiply(A, B, Y).

evaluate_function(A / B, Y) :-
	complex_divide(A, B, Y).

evaluate_function(A^B, Y) :-
	complex_pow(A, B, Y).

evaluate_function(sqrt(X), Y) :-
	number(X),
	evaluate_function(sqrt(2, X), Y).

evaluate_function(sqrt(A, B), Y) :-
	number(A),
	number(B),
	sqrt(A, B, Y).

evaluate_function(log(A, B), Y) :-
	number(A),
	number(B),
	Y is log(B) / log(A).

evaluate_function(ln(X), Y) :-
	number(X),
	Y is log(X).

evaluate_function(abs(Z), Y) :-
	complex_abs(Z, Y).

evaluate_function(arg(Z), Y) :-
	complex_arg(Z, Y).

evaluate_function(real_part(Z), X) :-
	as_complex(Z, complex(X, _)).

evaluate_function(imag_part(Z), Y) :-
	as_complex(Z, complex(_, Y)).

evaluate_function(sin(X), Y) :-
	Y is sin(X).

evaluate_function(cos(X), Y) :-
	Y is cos(X).

evaluate_function(tg(X), Y) :-
	Y is tan(X).

evaluate_function(equal([_]), log_true).

evaluate_function(equal([A, B | Tail]), Value) :-
	log_complex_equal(A, B, Equal),
	evaluate_function(equal([B | Tail]), TailEqual),
	log_and(Equal, TailEqual, Value).

evaluate_function(equal(A, B), Value) :-
	log_complex_equal(A, B, Value).

evaluate_function(not_equal(A, B), Value) :-
	evaluate_function(equal([A, B]), IsEqual),
	log_not(IsEqual, Value).

evaluate_function(less_than(A, B), Value) :-
	num_tolerance(Tolerance),
	Treshold is B - Tolerance,
	less_than(A, Treshold, Value).

evaluate_function(greater_than(A, B), Value) :-
	num_tolerance(Tolerance),
	Treshold is A - Tolerance,
	less_than(B, Treshold, Value).

evaluate_function(less_or_equal(A, B), Value) :-
	num_tolerance(Tolerance),
	Treshold is B + Tolerance,
	less_or_equal(A, Treshold, Value).

evaluate_function(greater_or_equal(A, B), Value) :-
	num_tolerance(Tolerance),
	Treshold is A + Tolerance,
	less_or_equal(B, Treshold, Value).

evaluate_function(min(set(S)), Value) :-
	min_list(S, Value).

evaluate_function(max(set(S)), Value) :-
	max_list(S, Value).

evaluate_function(inf(set(S)), Value) :-
	min_list(S, Value).

evaluate_function(sup(set(S)), Value) :-
	max_list(S, Value).

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

?-	evaluate_function(equal([0.35, 0.35, 0.35]), log_true).
?-	evaluate_function(equal([0.35, 0.35, 0.36]), log_false).

?-	evaluate_function(not_equal(0.35, 0.35), log_false).
?-	evaluate_function(not_equal(0.35, 0.36), log_true).

?-	evaluate_function(less_than(0.35, 0.36), log_true).
?-	evaluate_function(less_than(0.36, 0.36), log_false).
?-	evaluate_function(less_than(0.37, 0.36), log_false).

?-	evaluate_function(greater_than(0.35, 0.36), log_false).
?-	evaluate_function(greater_than(0.36, 0.36), log_false).
?-	evaluate_function(greater_than(0.36, 0.36), log_false).

?-	evaluate_function(less_or_equal(0.35, 0.36), log_true).
?-	evaluate_function(less_or_equal(0.36, 0.36), log_true).
?-	evaluate_function(less_or_equal(0.37, 0.36), log_false).

?-	evaluate_function(greater_or_equal(0.35, 0.36), log_false).
?-	evaluate_function(greater_or_equal(0.36, 0.36), log_true).
?-	evaluate_function(greater_or_equal(0.37, 0.36), log_true).

?-	evaluate_function(min(set([1, 5, 7, -3, 4])), -3).
?-	evaluate_function(max(set([1, 5, 7, -3, 4])), 7).

?-	evaluate_function(inf(set([1, 5, 7, -3, 4])), -3).
?-	evaluate_function(sup(set([1, 5, 7, -3, 4])), 7).

evaluate_list([], []).

evaluate_list([Expression | Tail], EvaluatedTail) :-
	hint(Expression),
	!,
	evaluate_list(Tail, EvaluatedTail).

evaluate_list([Expression | Tail], [EvaluatedExpression | EvaluatedTail]) :-
	evaluate_sub_expression(Expression, EvaluatedExpression),
	evaluate_list(Tail, EvaluatedTail).


evaluate_expression_or_fail(Expression, Value) :-
	evaluate_sub_expression(Expression, Value),
	!.

evaluate_expression_or_fail(_, failed).


log_choice(Variable, Label) :-
	verbose,
	write("Trying "),
	write(Label),
	write(" = "),
	writeln(Variable),
	fail.

log_choice(_, _).


evaluate_declaration(statement(StatementVariable, Label), SubFormula, Value) :-
	!,
	verify_variable_free(StatementVariable, Label),
	findall(SubFormulaValue, (boolean(StatementVariable), log_choice(StatementVariable, Label), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), [EF1, EF2]),
	log_and(EF1, EF2, Value).

evaluate_declaration(set(SetVariable, Label, TestedSets), SubFormula, Value) :-
	!,
	verify_variable_free(SetVariable, Label),
	findall(SubFormulaValue, (member(SetVariable, TestedSets), log_choice(SetVariable, Label), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_and_all(Results, Value).

evaluate_declaration(predicate(PredicateVariable, Label, TestedPredicates), SubFormula, Value) :-
	!,
	verify_variable_free(PredicateVariable, Label),
	findall(SubFormulaValue, (member(PredicateVariable, TestedPredicates), log_choice(PredicateVariable, Label), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_and_all(Results, Value).

evaluate_declaration(variable(Variable, Label, TestedValues), SubFormula, Value) :-
	!,
	verify_free_variable_or_list(Variable, Label),
	findall(SubFormulaValue, (member(Variable, TestedValues), log_choice(Variable, Label), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_and_all(Results, Value).

evaluate_declaration(function(Variable, Label, TestedFunctions), SubFormula, Value) :-
	!,
	verify_free_variable_or_list(Variable, Label),
	findall(
		SubFormulaValue,
		(member(Variable, TestedFunctions), log_choice(Variable, Label), rewrite_expression_or_exception(SubFormula, RewrittenSubFormula), evaluate_expression_or_fail(RewrittenSubFormula, SubFormulaValue)),
		Results
	),
	log_and_all(Results, Value).

evaluate_declaration(plus_minus(Variable), SubFormula, Value) :-
	!,
	verify_variable_free(Variable, "+-"),
	findall(SubFormulaValue, (member(Variable, [+1, -1]), log_choice(Variable, "+-"), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), [EF1, EF2]),
	log_and(EF1, EF2, Value).

evaluate_declaration(substitution(Variable, Label, Substitution), SubFormula, Value) :-
	!,
	verify_variable_free(Variable, Label),
	findall(SubFormulaValue, (Variable = Substitution, log_choice(Variable, Label), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), [Value]).


evaluate_sub_expression(Expression, Value) :-
	evaluate_expression(Expression, SubValue),
	!,
	Value = SubValue.

evaluate_sub_expression(Expression, _) :-
	write("Expression evaluation failed: "),
	writeln(Expression),
	fail.


evaluate_expression(Expression, _) :-
	verbose,
	write("Evaluating expression: "),
	writeln(Expression),
	fail.

evaluate_expression(Variable, _) :-
	var(Variable),
	writeln("Cannot evaluate free variable"),
	!,
	fail.

evaluate_expression(declare([], SubFormula), Value) :-
	!,
	evaluate_sub_expression(SubFormula, Value).

evaluate_expression(declare([Declaration | Tail], SubFormula), Value) :-
	!,
	evaluate_declaration(Declaration, declare(Tail, SubFormula), Value).

evaluate_expression(apply(Predicate, Args, ArgValues), Value) :-
	!,
	findall(SubFormulaValue, (Args = ArgValues, evaluate_expression_or_fail(Predicate, SubFormulaValue)), [Value]).

evaluate_expression(hidden_apply(Predicate, Args, ArgValues), Value) :-
	!,
	evaluate_expression(apply(Predicate, Args, ArgValues), Value).

evaluate_expression(forall(Variable, _, TestedValues, SubFormula), Value) :-
	!,
	var(Variable),
	findall(SubFormulaValue, (member(Variable, TestedValues), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_and_all(Results, Value).

evaluate_expression(forall_in(Variable, _, Set, TestedValues, SubFormula), Value) :-
	var(Variable),
	!,
	findall(
		SubFormulaValue,
		(
			member(Variable, TestedValues),
			evaluate_sub_expression(in(Variable, Set), log_true),
			evaluate_expression_or_fail(SubFormula, SubFormulaValue)
		),
		Results
	),
	log_and_all(Results, Value).

evaluate_expression(forall_in([Variable], [Label], Set, TestedValues, SubFormula), Value) :-
	!,
	var(Variable),
	evaluate_sub_expression(
		forall_in(Variable, Label, Set, TestedValues, SubFormula),
		Value
	).

evaluate_expression(forall_in([Variable | VariableTail], [Label | LabelTail], Set, TestedValues, SubFormula), Value) :-
	!,
	var(Variable),
	evaluate_sub_expression(
		forall_in(Variable, Label, Set, TestedValues,
			forall_in(VariableTail, LabelTail, Set, TestedValues, SubFormula)
		),
		Value
	).

evaluate_expression(exists(Variable, _, TestedValues, SubFormula), Value) :-
	!,
	var(Variable),
	findall(SubFormulaValue, (member(Variable, TestedValues), evaluate_expression_or_fail(SubFormula, SubFormulaValue)), Results),
	log_or_all(Results, Value).

evaluate_expression(exists_in(Variable, _, Set, TestedValues, SubFormula), Value) :-
	!,
	var(Variable),
	evaluate_sub_expression(exists(Variable, _, TestedValues, and(in(Variable, Set), SubFormula)), Value).

evaluate_expression(set_by(Variable, _, Values, Formula), Value) :-
	!,
	var(Variable),
	findall(Variable, (member(Variable, Values), evaluate_expression_or_fail(Formula, log_true)), Results),
	make_set(Results, ResultsSet),
	Value = set(ResultsSet).

evaluate_expression(List, EvaluatedList) :-
	is_list(List),
	!,
	evaluate_list(List, EvaluatedList).

evaluate_expression(in(X, difference(A, B)), Value) :-
	evaluate_sub_expression(and(in(X, A), not_in(X, B)), Value),
	!.

evaluate_expression(lim(VariableOrList, _, Domain, Point, Function), Value) :-
	!,
	evaluate_sub_expression(Point, EvaluatedPoint),
	calculate_lim(VariableOrList, Domain, EvaluatedPoint, Function, Value).

evaluate_expression(impl(A, B), Value) :-
	!,
	evaluate_impl(A, B, Value).

evaluate_expression(Expression, Value) :-
	Expression =.. [Functor | Args],
	evaluate_list(Args, EvaluatedArgs),
	SubEvaluatedExpression =.. [Functor | EvaluatedArgs],
	evaluate_function(SubEvaluatedExpression, Value).


evaluate_impl(A, _, Value) :-
	evaluate_expression(A, ValueA),
	ValueA = log_false,
	!,
	Value = log_true.

evaluate_impl(_, B, Value) :-
	evaluate_expression(B, Value).

?-	evaluate_list([log_true, linebreak, log_false], [log_true, log_false]).
?-	evaluate_expression([log_true, linebreak, log_false], [log_true, log_false]).

?-	evaluate_expression(3*4 - 2*5, 2).
?-	evaluate_expression(3 * (4 - 2) * 5, 30).

?-	evaluate_expression(equal([1/(2+1), 2 / 6, 1 - (2/3)]), log_true).
?-	evaluate_expression(equal([1, 2]), log_false).

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
		declare([set(A, 'A', [set(S1), set(S2), set(S3)]), set(B, 'B', [set(S1), set(S2), set(S3)])],
			set_equal([
				intersection(A, B),
				intersection(B, A)
			])
		),
		log_true
	).

?-	make_set([1, 2, 3], S1),
	make_set([2, 3, 4], S2),
	make_set([4, 5], S3),
	evaluate_expression(
		declare([set(A, 'A', [set(S1), set(S2), set(S3)]), set(B, 'B', [set(S1), set(S2), set(S3)])],
			set_equal([
				intersection(A, B),
				union(A, B)
			])
		),
		log_false
	).

?-	P = X * Y,
	evaluate_expression(
		apply(P, [X, Y], [3, 4]) + apply(P, [X, Y], [5, 6]),
		42
	).

?-	evaluate_expression(
		forall(X, 'X', [1, 2], equal([X, 1])),
		log_false
	).

?-	evaluate_expression(
		forall(X, 'X', [1, 2], or(equal([X, 1]), equal([X, 2]))),
		log_true
	).

?-	evaluate_expression(
		exists(X, 'X', [1, 2], equal([X, 3])),
		log_false
	).

?-	evaluate_expression(
		exists(X, 'X', [1, 2], equal([X, 2])),
		log_true
	).

?-	evaluate_expression(
		forall_in(X, 'X', natural_numbers, [-1, 1], equal(X * X, X)),
		log_true
	).

?-	evaluate_expression(
		forall_in(X, 'X', integers, [-1, 1], equal(X * X, X)),
		log_false
	).

?-	evaluate_expression(
		forall_in([X, Y], ['X', 'Y'], natural_numbers, [1, 2], equal(X + Y, Y + X)),
		log_true
	).

?-	evaluate_expression(
		forall_in([X, Y], ['X', 'Y'], natural_numbers, [1, 2], equal(X + Y, Y * X)),
		log_false
	).

?-	make_set([1, 2], Value),
	evaluate_expression(
		set_by(X, 'x', [1, 2, 3, 4], or(equal([X, 1]), equal([X, 2]))),
		set(Value)
	).


