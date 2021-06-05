:- ensure_loaded(common).
:- ensure_loaded(math).


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
	Y is B^(1 / A).

evaluate_function(log(A, B), Y) :-
	number(A),
	number(B),
	Y is log(B) / log(A).

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

function_derivative(F, []) :-
	number(F).

function_derivative(imag, []).
function_derivative(e, []).
function_derivative(pi, []).
function_derivative(A + B, [[A, 1], [B, 1]]).
function_derivative(A - B, [[A, 1], [B, -1]]).
function_derivative(A * B, [[A, B], [B, A]]).
function_derivative(A / B, [[A, 1 / B], [B, -A / B^2]]).
function_derivative(A^B, [[A, B * A^(B - 1)], [B, log(e, A) * e^(B * log(e, A))]]).
function_derivative(sin(X), [[X, cos(X)]]).
function_derivative(cos(X), [[X, -sin(X)]]).

expression_derivative(X, Expression, 1) :-
	var(Expression),
	X == Expression,
	!.

expression_derivative(X, Expression, 0) :-
	var(Expression),
	X \== Expression,
	!.

expression_derivative(X, integral(X, Expression), Expression) :-
	!.

expression_derivative(_, [], []) :-
	!.

expression_derivative(X, [Expression | ExpressionTail], [Derivative | DerivativeTail]) :-
	!,
	expression_derivative(X, Expression, Derivative),
	expression_derivative(X, ExpressionTail, DerivativeTail).

expression_derivative(X, real_part(Expression), real_part(Derivative)) :-
	!,
	expression_derivative(X, Expression, Derivative).

expression_derivative(X, imag_part(Expression), imag_part(Derivative)) :-
	!,
	expression_derivative(X, Expression, Derivative).

expression_derivative(X, apply(Function, Args, Values), Derivative) :-
	!,
	copy_term([Function, Args], [CopiedFunction, CopiedArgs]),
	CopiedArgs = Values,
	expression_derivative(X, CopiedFunction, Derivative).

expression_derivative(X, Expression, Derivative) :-
	function_derivative(Expression, FunctionDerivative),
	compound_derivative(X, FunctionDerivative, Derivative).


compound_derivative(_, [], 0).

compound_derivative(X, [[SubExpression, Der] | Tail], Result) :-
	expression_derivative(X, SubExpression, SubDerivative),
	compound_derivative(X, Tail, TailDerivative),
	symbolic_multiply(Der, SubDerivative, CompoundDerivative),
	symbolic_add(CompoundDerivative, TailDerivative, Result).

?- 	expression_derivative(X, (2*X + 1)^3, 3 * (2*X + 1)^(3 - 1) * 2).
?-	F = Z^2, expression_derivative(X, apply(F, [Z], [3*X+4]), 2 * (3*X + 4)^(2-1) * 3).


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
		(member(Variable, TestedFunctions), log_choice(Variable, Label), rewrite_expression(SubFormula, RewrittenSubFormula), evaluate_expression_or_fail(RewrittenSubFormula, SubFormulaValue)),
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
	writeln("Cannout evaluate free variable"),
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

evaluate_expression(Expression, Value) :-
	Expression =.. [Functor | Args],
	evaluate_list(Args, EvaluatedArgs),
	SubEvaluatedExpression =.. [Functor | EvaluatedArgs],
	evaluate_function(SubEvaluatedExpression, Value).


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


calculate_lim(Variable, Domain, Point, Function, Value) :-
	var(Variable),
	calculate_lim([Variable], [Domain], [Point], Function, Value).

calculate_lim(VariableList, Domain, Point, Function, Value) :-
	is_list(VariableList),
	findall(FunctionValue, (generate_lim_value_list(VariableList, Domain, Point), evaluate_expression_or_fail(Function, FunctionValue)), ValueList),
	complex_all_equal(ValueList),
	complex_sum(ValueList, ValueSum),
	length(ValueList, Length),
	Coeff is 1.0 / Length, 
	complex_multiply(Coeff, ValueSum, Value).

generate_lim_value_list([], [], []).

generate_lim_value_list([Variable | VariableTail], [Domain | DomainTail], [Coordinate | CoordinateTail]) :-
	generate_lim_value(Variable, Domain, Coordinate),
	generate_lim_value_list(VariableTail, DomainTail, CoordinateTail).


lim_dx(1e-8).


generate_lim_value(Variable, real, Coordinate) :-
	lim_dx(Dx),
	as_real(Coordinate, X),
	Variable is X - Dx.

generate_lim_value(Variable, real, Coordinate) :-
	lim_dx(Dx),
	as_real(Coordinate, X),
	Variable is X + Dx.

generate_lim_value(Variable, complex, Coordinate) :-
	lim_dx(Dx),
	Difference is -Dx,
	complex_add(Coordinate, complex(Difference, 0), Variable).

generate_lim_value(Variable, complex, Coordinate) :-
	lim_dx(Dx),
	Difference is +Dx,
	complex_add(Coordinate, complex(Difference, 0), Variable).

generate_lim_value(Variable, complex, Coordinate) :-
	lim_dx(Dx),
	Difference is -Dx,
	complex_add(Coordinate, complex(0, Difference), Variable).

generate_lim_value(Variable, complex, Coordinate) :-
	lim_dx(Dx),
	Difference is +Dx,
	complex_add(Coordinate, complex(0, Difference), Variable).


?-	evaluate_expression(
		equal(
			lim(X, 'x', real, 1, (2*X - 2) / (X - 1)),
			2
		),
		log_true
	).


rewrite_expression(X, X) :-
	var(X),
	!.

rewrite_expression(declare([], Expression), declare([], RewrittenExpression)) :-
	!,
	rewrite_expression(Expression, RewrittenExpression).

rewrite_expression(declare([function(Variable, Label, Functions) | Tail], Expression), declare([function(Variable, Label, Functions) | Tail], Expression)) :-
	!.

rewrite_expression(declare([substitution(Variable, _,  Value) | Tail], Expression), RewrittenExpression) :-
	!,
	var(Variable),
	copy_variable(Variable, [Tail, Expression], CopiedVariable, [CopiedTail, CopiedExpression]),
	CopiedVariable = Value,
	rewrite_expression(declare(CopiedTail, CopiedExpression), RewrittenExpression).

rewrite_expression(declare([Declaration | Tail], Expression), declare([Declaration | RewrittenTail], RewrittenExpression)) :-
	!,
	rewrite_expression(declare(Tail, Expression), declare(RewrittenTail, RewrittenExpression)).

rewrite_expression(derivative(X, F), Derivative) :-
	!,
	expression_derivative(X, F, Derivative).

rewrite_expression(equal_transform(Transformation, ExpressionList), equal(Derivative)) :-
	is_list(ExpressionList),
	!,
	transform_expression_list(Transformation, ExpressionList, Derivative).

rewrite_expression(Expression, RewrittenExpression) :-
	Expression =.. [Functor | Args],
	rewrite_expression_list(Args, RewrittenArgs),
	RewrittenExpression =.. [Functor | RewrittenArgs].


rewrite_expression_list([], []).

rewrite_expression_list([Expression | ExpressionTail], [RewrittenExpression | RewrittenTail]) :-
	rewrite_expression(Expression, RewrittenExpression),
	rewrite_expression_list(ExpressionTail, RewrittenTail).


transform_expression_list([], Expression, Expression).

transform_expression_list([Transformation | TransformationTail], Expression, TransformedExpression) :-
	single_transform_expression_list(Transformation, Expression, HeadTransformedExpression),
	transform_expression_list(TransformationTail, HeadTransformedExpression, TransformedExpression).


single_transform_expression_list(_, [], []).

single_transform_expression_list(Transformation, [Expression | ExpressionTail], TransformedExpressionTail) :-
	hint(Expression),
	!,
	single_transform_expression_list(Transformation, ExpressionTail, TransformedExpressionTail).

single_transform_expression_list(Transformation, [Expression | ExpressionTail], [TransformedExpression | TransformedExpressionTail]) :-
	single_transform_expression(Transformation, Expression, TransformedExpression),
	single_transform_expression_list(Transformation, ExpressionTail, TransformedExpressionTail).


single_transform_expression(derivative(X), Expression, Derivative) :-
	rewrite_expression(derivative(X, Expression), Derivative).

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


print_declaration(Stream, statement(Value, Label), SubFormula, PR) :-
	atomic_list_concat(['\\predicate{', Label, '}'], Value),
	print_expression_term(Stream, SubFormula, PR).

print_declaration(Stream, predicate(Variable, Label, _), SubFormula, PR) :-
	Variable = Label,
	print_expression_term(Stream, SubFormula, PR).

print_declaration(Stream, set(Variable, Label, _), SubFormula, PR) :-
	Variable = Label,
	print_expression_term(Stream, SubFormula, PR).

print_declaration(Stream, variable(Variable, Label, _), SubFormula, PR) :-
	Variable = Label,
	print_expression_term(Stream, SubFormula, PR).

print_declaration(Stream, substitution(Variable, Label, _), SubFormula, PR) :-
	copy_term([Variable, SubFormula], [CopiedVariable, CopiedSubFormula]),
	CopiedVariable = Label,
	print_expression_term(Stream, CopiedSubFormula, PR).

print_declaration(Stream, function(Variable, Label, _), SubFormula, PR) :-
	Variable = Label,
	print_expression_term(Stream, SubFormula, PR).

print_declaration(Stream, plus_minus(_), SubFormula, PR) :-
	print_expression_term(Stream, SubFormula, PR).


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

print_expression_term(Stream, S, _) :-
	string(S),
	!,
	write(Stream, S).

print_expression_term(Stream, imag, _) :-
	!,
	write(Stream, ' \\imag ').

print_expression_term(Stream, empty_set, _) :-
	!,
	write(Stream, ' \\emptyset ').

print_expression_term(Stream, natural_numbers, _) :-
	!,
	write(Stream, ' \\naturalnumbers ').

print_expression_term(Stream, integers, _) :-
	!,
	write(Stream, ' \\integers ').

print_expression_term(Stream, rational_numbers, _) :-
	!,
	write(Stream, ' \\rationals ').

print_expression_term(Stream, real_numbers, _) :-
	!,
	write(Stream, ' \\real ').

print_expression_term(Stream, positive_real_numbers, _) :-
	!,
	write(Stream, ' \\real^+ ').

print_expression_term(Stream, complex_numbers, _) :-
	!,
	write(Stream, ' \\complex ').

print_expression_term(Stream, set_of(Values), _) :-
	!,
	write(Stream, ' \\{ '),
	print_expression_list(Stream, Values),
	write(Stream, ' \\} ').

print_expression_term(Stream, F, _) :-
	atom(F),
	write(Stream, F).

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

print_expression_term(Stream, declare([], SubFormula), PR) :-
	print_expression_term(Stream, SubFormula, PR).

print_expression_term(Stream, declare([Declaration | Tail], SubFormula), PR) :-
	print_declaration(Stream, Declaration, declare(Tail, SubFormula), PR).

print_expression_term(Stream, forall(Variable, Label, _, SubFormula), PR) :-
	Variable = Label,
	print_bracket_if_needed(Stream, '(', PR, forall), 
	write(Stream, ' \\forall '),
	write(Stream, Variable),
	write(Stream, ' \\ '),
	print_expression_term(Stream, SubFormula, forall),
	print_bracket_if_needed(Stream, ')', PR, forall).

print_expression_term(Stream, forall_in(Variable, Label, Set, Values, SubFormula), PR) :-
	\+ is_list(Variable),
	print_expression_term(Stream, forall_in([Variable], [Label], Set, Values, SubFormula), PR).

print_expression_term(Stream, forall_in(VariableList, LabelList, Set, _, SubFormula), PR) :-
	is_list(VariableList),
	VariableList = LabelList,
	print_bracket_if_needed(Stream, '(', PR, forall), 
	write(Stream, ' \\forall '),
	print_expression_list(Stream, VariableList),
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
	print_binary_operator_with_position(Stream, A, B, PR, minus(xxx), minus(left), minus(right), ' - ').

print_expression_term(Stream, plus_minus(A, B, _), PR) :-
	print_binary_operator(Stream, A, B, PR, plus, ' \\pm ').

print_expression_term(Stream, -A, PR) :-
	print_prefix_operator(Stream, A, PR, opposite, ' -').

print_expression_term(Stream, A * B, PR) :-
	print_binary_operator(Stream, A, B, PR, multiply, ' \\cdot ').

print_expression_term(Stream, A / B, PR) :-
	print_bracket_if_needed(Stream, '(', PR, div),
	write(Stream, '\\frac{'),
	print_expression_term(Stream, A, root),   % root = no need of brackets
	write(Stream, '}{'),
	print_expression_term(Stream, B, root),   % root = no need of brackets
	write(Stream, '}'),
	print_bracket_if_needed(Stream, ')', PR, div).

print_expression_term(Stream, A^B, PR) :-
	print_bracket_if_needed(Stream, '(', PR, pow),
	write(Stream, '{'),
	print_expression_term(Stream, A, pow),
	write(Stream, '}^{'),
	print_expression_term(Stream, B, root),   % root = no need of brackets in exponent
	write(Stream, '}'),
	print_bracket_if_needed(Stream, ')', PR, pow).

print_expression_term(Stream, sqrt(X), PR) :-
	print_bracket_if_needed(Stream, '(', PR, sqrt),
	write(Stream, '\\sqrt{'),
	print_expression_term(Stream, X, root),
	write(Stream, '}'),
	print_bracket_if_needed(Stream, ')', PR, sqrt).

print_expression_term(Stream, sqrt(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, sqrt),
	write(Stream, '\\sqrt['),
	print_expression_term(Stream, A, root),
	write(Stream, ']{'),
	print_expression_term(Stream, B, root),
	write(Stream, '}'),
	print_bracket_if_needed(Stream, ')', PR, sqrt).

print_expression_term(Stream, log(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, log),
	write(Stream, '\\log_{'),
	print_expression_term(Stream, A, log),
	write(Stream, '}{'),
	print_expression_term(Stream, B, log),
	write(Stream, '}'),
	print_bracket_if_needed(Stream, ')', PR, log).

print_expression_term(Stream, sin(X), PR) :-
	print_bracket_if_needed(Stream, '(', PR, goniom),
	write(Stream, '\\sin '),
	print_expression_term(Stream, X, goniom),
	print_bracket_if_needed(Stream, ')', PR, goniom).

print_expression_term(Stream, cos(X), PR) :-
	print_bracket_if_needed(Stream, '(', PR, goniom),
	write(Stream, '\\cos '),
	print_expression_term(Stream, X, goniom),
	print_bracket_if_needed(Stream, ')', PR, goniom).

print_expression_term(Stream, tg(X), PR) :-
	print_bracket_if_needed(Stream, '(', PR, goniom),
	write(Stream, '\\tg '),
	print_expression_term(Stream, X, goniom),
	print_bracket_if_needed(Stream, ')', PR, goniom).

print_expression_term(Stream, abs(Z), _) :-
	write(Stream, '\\left|'),
	print_expression_term(Stream, Z, root),
	write(Stream, '\\right|').

print_expression_term(Stream, arg(X), PR) :-
	print_bracket_if_needed(Stream, '(', PR, goniom),
	write(Stream, '\\arg '),
	print_expression_term(Stream, X, goniom),
	print_bracket_if_needed(Stream, ')', PR, goniom).

print_expression_term(Stream, real_part(X), PR) :-
	print_bracket_if_needed(Stream, '(', PR, goniom),
	write(Stream, '\\realpart '),
	print_expression_term(Stream, X, goniom),
	print_bracket_if_needed(Stream, ')', PR, goniom).

print_expression_term(Stream, imag_part(X), PR) :-
	print_bracket_if_needed(Stream, '(', PR, goniom),
	write(Stream, '\\imagpart '),
	print_expression_term(Stream, X, goniom),
	print_bracket_if_needed(Stream, ')', PR, goniom).

print_expression_term(Stream, equal(List), PR) :-
	is_list(List),
	print_bracket_if_needed(Stream, '(', PR, equal),
	print_chain(Stream, List, ' = ', equal),
	print_bracket_if_needed(Stream, ')', PR, equal).

print_expression_term(Stream, equal(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, equal, ' = ').

print_expression_term(Stream, equal_transform(_, List), PR) :-
	is_list(List),
	print_expression_term(Stream, equal(List), PR).

print_expression_term(Stream, equal_transform(_, A, B), PR) :-
	print_expression_term(Stream, equal(A, B), PR).

print_expression_term(Stream, not_equal(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, equal, ' \\neq ').

print_expression_term(Stream, less_than(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, equal, ' < ').

print_expression_term(Stream, greater_than(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, equal, ' > ').

print_expression_term(Stream, less_or_equal(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, equal, ' \\leq ').

print_expression_term(Stream, greater_or_equal(A, B), PR) :-
	print_binary_operator(Stream, A, B, PR, equal, ' \\geq ').

print_expression_term(Stream, proof(Axioms, Conclusions), _) :-
	print_chain(Stream, Axioms, ' \\\\ ', root),
	print_if_not_empty(Stream, ' \\\\ \\\\ ', Axioms),
	print_chain(Stream, Conclusions, ' \\\\ ', root).

print_expression_term(Stream, min(S), _) :-
	print_function(Stream, '\\min', [S]).

print_expression_term(Stream, max(S), _) :-
	print_function(Stream, '\\max', [S]).

print_expression_term(Stream, inf(S), _) :-
	print_function(Stream, '\\inf', [S]).

print_expression_term(Stream, sup(S), _) :-
	print_function(Stream, '\\sup', [S]).

print_expression_term(Stream, lim(VariableOrList, LabelOrList, _, Point, Func), _) :-
	VariableOrList = LabelOrList,
	write(Stream, '\\lim_{'),
	print_expression_or_list(Stream, VariableOrList),
	write(Stream, ' \\to '),
	print_expression_or_list(Stream, Point),
	write(Stream, '} '),
	print_expression_term(Stream, Func, lim).

print_expression_term(Stream, derivative(Variable, apply(Function, _, _)), PR) :-
	atomic(Function),
	!,
	print_bracket_if_needed(Stream, '(', PR, function_derivative),
	write(Stream, "\\frac{\\partial "),
	write(Stream, Function),
	write(Stream, "}{\\partial "),
	write(Stream, Variable),
	write(Stream, "}"),
	print_bracket_if_needed(Stream, ')', PR, function_derivative).

print_expression_term(Stream, integral(Variable, Expression), PR) :-
	!,
	print_bracket_if_needed(Stream, '(', PR, integral),
	write(Stream, "\\int "),
	print_expression_term(Stream, Expression, integral),
	write(Stream, " \\cdot \\mathrm{d}"),
	write(Stream, Variable),
	print_bracket_if_needed(Stream, ')', PR, integral).

print_expression_term(Stream, derivative(Variable, Function), PR) :-
	print_bracket_if_needed(Stream, '(', PR, expression_derivative),
	write(Stream, "\\frac{\\partial}{\\partial "),
	write(Stream, Variable),
	write(Stream, "}"),
	print_expression_term(Stream, Function, expression_derivative),
	print_bracket_if_needed(Stream, ')', PR, expression_derivative).

print_prefix_operator(Stream, A, SuperOperator, SubOperator, Label) :-
	print_bracket_if_needed(Stream, '(', SuperOperator, SubOperator),
	write(Stream, Label),
	print_expression_term(Stream, A, SubOperator),
	print_bracket_if_needed(Stream, ')', SuperOperator, SubOperator).

print_binary_operator_with_position(Stream, A, B, SuperOperator, SubOperator, LeftSubOperator, RightSubOperator, Label) :-
	print_bracket_if_needed(Stream, '(', SuperOperator, SubOperator),
	print_expression_term(Stream, A, LeftSubOperator),
	write(Stream, Label),
	print_expression_term(Stream, B, RightSubOperator),
	print_bracket_if_needed(Stream, ')', SuperOperator, SubOperator).

print_binary_operator(Stream, A, B, SuperOperator, SubOperator, Label) :-
	print_binary_operator_with_position(Stream, A, B, SuperOperator, SubOperator, SubOperator, SubOperator, Label).

print_function(Stream, Functor, Args) :-
	write(Stream, Functor),
	write(Stream, '('),
	print_expression_list(Stream, Args),
	write(Stream, ')').

print_predicate_args(Stream, [Arg, Next | Tail]) :-
	print_expression_term(Stream, Arg),
	write(Stream, ', '),
	print_predicate_args(Stream, [Next | Tail]).

print_predicate_args(Stream, [Arg]) :-
	print_expression_term(Stream, Arg).

print_predicate_args(_, []).


print_expression_list(Stream, [Arg, Next | Tail]) :-
	print_expression_term(Stream, Arg, root),
	write(Stream, ', '),
	print_expression_list(Stream, [Next | Tail]).

print_expression_list(Stream, [Arg]) :-
	print_expression_term(Stream, Arg, root).

print_expression_list(_, []).


print_expression_or_list(Stream, ExpressionOrList) :-
	\+ is_list(ExpressionOrList),
	print_expression_term(Stream, ExpressionOrList).

print_expression_or_list(Stream, ExpressionOrList) :-
	is_list(ExpressionOrList),
	write(Stream, "["),
	print_expression_list(Stream, ExpressionOrList),
	write(Stream, "]").


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

print_chain(_, [], _, _).


% Prints bracket if it is needed.
% print_bracket_if_needed(Stream, Bracket, SuperOperator, SubOperator)
print_bracket_if_needed(_, _, SuperOperator, SubOperator) :-
	bracket_not_needed(SuperOperator, SubOperator),
	!.

print_bracket_if_needed(Stream, '(', _, _) :-
	write(Stream, '\\left(').

print_bracket_if_needed(Stream, ')', _, _) :-
	write(Stream, '\\right)').


operator_priority(function_derivative, 207).
operator_priority(pow, 206).
operator_priority(sqrt, 206).
operator_priority(log, 206).
operator_priority(div, 205).
operator_priority(goniom, 204).
operator_priority(multiply, 203).
operator_priority(expression_derivative, 202).
operator_priority(integral, 202).
operator_priority(lim, 202).
operator_priority(opposite, 201).
operator_priority(plus, 200).
operator_priority(minus(_), 200).

operator_priority(in, 102).
operator_priority(difference, 101).
operator_priority(union, 101).
operator_priority(intersection, 101).
operator_priority(equal, 100).
operator_priority(subset, 100).

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
bracket_not_needed(minus(left), plus).
bracket_not_needed(multiply, multiply).

bracket_not_needed(SuperOperator, SubOperator) :-
	quantifier(SuperOperator),
	quantifier(SubOperator).

quantifier(forall).
quantifier(exists).

print_hint(Stream, linebreak) :-
	writeln(Stream, ' \\\\').

