
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Formula evaluation %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Evaluates logical formula
% eval_formula(Formula, Value)
eval_formula(log_false, log_false).
eval_formula(log_true, log_true).

eval_formula(not(X), Value) :-
	eval_formula(X, EX),
	log_not(EX, Value).

eval_formula(or(A, B), Value) :-
	eval_formula(A, EA),
	eval_formula(B, EB),
	log_or(EA, EB, Value).

eval_formula(and(A, B), Value) :-
	eval_formula(A, EA),
	eval_formula(B, EB),
	log_and(EA, EB, Value).

eval_formula(impl(A, B), Value) :-
	eval_formula(A, EA),
	eval_formula(B, EB),
	log_impl(EA, EB, Value).

eval_formula(equiv(A, B), Value) :-
	eval_formula(A, EA),
	eval_formula(B, EB),
	log_equiv(EA, EB, Value).

eval_formula(statement(StatementVariable, _, SubFormula), Value) :-
	findall(SubFormulaValue, (boolean(StatementVariable), eval_formula(SubFormula, SubFormulaValue)), [EF1, EF2]),
	log_and(EF1, EF2, Value).


%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Formula print %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%

% Prints formula with given label to file calculated 
print_formula(Label, Formula) :-
	calc_file_path('eq', Label, Path),
	open(Path, write, Stream),
	writeln(Stream, '\\begin{equation}'),
	print_label(Stream, 'eq:', Label),
	print_formula_term(Stream, Formula),
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
print_formula_term(Stream, F) :-
	print_formula_term(Stream, F, root).

print_formula_term(Stream, F, _) :-
	atom(F),
	write(Stream, F).

print_formula_term(Stream, statement(V, L, F), PR) :-
	atomic_list_concat(['\\predicate{', L, '}'], V),
	print_formula_term(Stream, F, PR).

print_formula_term(Stream, impl(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, impl), 
	print_formula_term(Stream, A, equiv),
	write(Stream, ' \\impl '),
	print_formula_term(Stream, B, equiv),
	print_bracket_if_needed(Stream, ')', PR, impl).

print_formula_term(Stream, equiv(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, equiv), 
	print_formula_term(Stream, A, equiv),
	write(Stream, ' \\equivalent '),
	print_formula_term(Stream, B, equiv),
	print_bracket_if_needed(Stream, ')', PR, equiv).

print_formula_term(Stream, or(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, or), 
	print_formula_term(Stream, A, or),
	write(Stream, ' \\lor '),
	print_formula_term(Stream, B, or),
	print_bracket_if_needed(Stream, ')', PR, or).

print_formula_term(Stream, and(A, B), PR) :-
	print_bracket_if_needed(Stream, '(', PR, and), 
	print_formula_term(Stream, A, and),
	write(Stream, ' \\land '),
	print_formula_term(Stream, B, and),
	print_bracket_if_needed(Stream, ')', PR, and).

print_formula_term(Stream, not(F), _) :-
	write(Stream, '\\overline{'),
	print_formula_term(Stream, F, 7),
	write(Stream, '}').


% Validates formula (checks that it is true) and prints it.
print_validated_formula(Label, Formula) :-
	eval_formula(Formula, log_true),
	print_formula(Label, Formula).


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
bracket_not_needed(and, or).
bracket_not_needed(and, impl).
bracket_not_needed(and, equiv).
bracket_not_needed(or, impl).
bracket_not_needed(or, equiv).
bracket_not_needed(impl, equiv).

