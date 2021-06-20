:- ensure_loaded(common).
:- ensure_loaded(math).

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

print_expression_term(Stream, pi, _) :-
	!,
	write(Stream, "\\pi").

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

print_expression_term(Stream, line(X), _) :-
	!,
	print_expression_term(Stream, X),
	writeln(Stream, " \\\\").

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

print_expression_term(Stream, apply(Expression, _, ArgValues), _) :-
	print_expression_term(Stream, Expression),
	write(Stream, '('),
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
	write(Stream, "\\frac{\\partial"),
	polynom_total_order(Variable, Order),
	print_exponent_if_needed(Stream, Order),
	write(Stream, " "),
	write(Stream, Function),
	write(Stream, "}{"),
	print_partial(Stream, Variable),
	write(Stream, "}"),
	print_bracket_if_needed(Stream, ')', PR, function_derivative).

print_expression_term(Stream, derivative(Variable, Function), PR) :-
	print_bracket_if_needed(Stream, '(', PR, expression_derivative),
	write(Stream, "\\frac{\\partial"),
	polynom_total_order(Variable, Order),
	print_exponent_if_needed(Stream, Order),
	write(Stream, "}{"),
	print_partial(Stream, Variable),
	write(Stream, "}"),
	print_expression_term(Stream, Function, expression_derivative),
	print_bracket_if_needed(Stream, ')', PR, expression_derivative).

print_expression_term(Stream, integral(Variable, Expression), PR) :-
	!,
	print_bracket_if_needed(Stream, '(', PR, integral),
	write(Stream, "\\int "),
	print_expression_term(Stream, Expression, integral),
	write(Stream, " \\cdot \\mathrm{d}"),
	write(Stream, Variable),
	print_bracket_if_needed(Stream, ')', PR, integral).

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


print_exponent_if_needed(_, 1) :-
	!.

print_exponent_if_needed(Stream, N) :-
	write(Stream, "^"),
	write(Stream, N).


print_partial(Stream, X) :-
	atomic(X),
	write(Stream, "\\partial "),
	write(Stream, X).

print_partial(Stream, X^N) :-
	write(Stream, "\\partial "),
	write(Stream, X^N).

print_partial(Stream, A * B) :-
	print_partial(Stream, A),
	write(Stream, " "),
	print_partial(Stream, B).


polynom_total_order(Var, 1) :-
	var(Var).

polynom_total_order(Var, 1) :-
	atomic(Var).

polynom_total_order(_^N, N).

polynom_total_order(A * B, Order) :-
	polynom_total_order(A, OrderA),
	polynom_total_order(B, OrderB),
	Order is OrderA + OrderB.


operator_priority(pow, 207).
operator_priority(sqrt, 207).
operator_priority(function_derivative, 206).
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
bracket_not_needed(plus, minus(_)).
bracket_not_needed(multiply, multiply).

bracket_not_needed(SuperOperator, SubOperator) :-
	quantifier(SuperOperator),
	quantifier(SubOperator).

quantifier(forall).
quantifier(exists).

print_hint(Stream, linebreak) :-
	writeln(Stream, ' \\\\').

