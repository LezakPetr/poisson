
:- ensure_loaded(common).

boolean(log_false).
boolean(log_true).

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


% Evaluates logical formula
% eval_formula(F, Y)
eval_formula(log_false, log_false).
eval_formula(log_true, log_true).

eval_formula(not(X), Y) :-
	eval_formula(X, EX),
	log_not(EX, Y).

eval_formula(or(A, B), Y) :-
	eval_formula(A, EA),
	eval_formula(B, EB),
	log_or(EA, EB, Y).

eval_formula(and(A, B), Y) :-
	eval_formula(A, EA),
	eval_formula(B, EB),
	log_and(EA, EB, Y).

eval_formula(impl(A, B), Y) :-
	eval_formula(A, EA),
	eval_formula(B, EB),
	log_impl(EA, EB, Y).

eval_formula(equiv(A, B), Y) :-
	eval_formula(A, EA),
	eval_formula(B, EB),
	log_equiv(EA, EB, Y).

eval_formula(statement(S, _, F), Y) :-
	findall(EF, (boolean(S), eval_formula(F, EF)), [EF1, EF2]),
	log_and(EF1, EF2, Y).


print_formula(L, F) :-
	calc_file_path(L, PATH),
	open(PATH, write, S),
	writeln(S, '\\begin{equation}'),
	print_label(S, 'eq:', L),
	print_formula_term(S, F),
	writeln(S, ''),
	writeln(S, '\\end{equation}'),
	close(S).


print_boolean(S, log_true) :-
	write(S, '\\true').
		
print_boolean(S, log_false) :-
	write(S, '\\false').

print_boolean_in_math_mode(S, B) :-
	write(S, '\\('),
	print_boolean(S, B),
	write(S, '\\)').


print_formula_term(S, F) :-
	print_formula_term(S, F, root).

print_formula_term(S, F, _) :-
	atom(F),
	write(S, F).

print_formula_term(S, statement(V, L, F), PR) :-
	atomic_list_concat(['\\predicate{', L, '}'], V),
	print_formula_term(S, F, PR).

print_formula_term(S, impl(A, B), PR) :-
	print_bracket_if_needed(S, '(', PR, impl), 
	print_formula_term(S, A, equiv),
	write(S, ' \\impl '),
	print_formula_term(S, B, equiv),
	print_bracket_if_needed(S, ')', PR, impl).

print_formula_term(S, equiv(A, B), PR) :-
	print_bracket_if_needed(S, '(', PR, equiv), 
	print_formula_term(S, A, equiv),
	write(S, ' \\equivalent '),
	print_formula_term(S, B, equiv),
	print_bracket_if_needed(S, ')', PR, equiv).

print_formula_term(S, or(A, B), PR) :-
	print_bracket_if_needed(S, '(', PR, or), 
	print_formula_term(S, A, or),
	write(S, ' \\lor '),
	print_formula_term(S, B, or),
	print_bracket_if_needed(S, ')', PR, or).

print_formula_term(S, and(A, B), PR) :-
	print_bracket_if_needed(S, '(', PR, and), 
	print_formula_term(S, A, and),
	write(S, ' \\land '),
	print_formula_term(S, B, and),
	print_bracket_if_needed(S, ')', PR, and).

print_formula_term(S, not(F), _) :-
	write(S, '\\overline{'),
	print_formula_term(S, F, 7),
	write(S, '}').


print_validated_formula(L, F) :-
	eval_formula(F, log_true),
	print_formula(L, F).

print_bracket_if_needed(_, _, SUP, SUB) :-
	bracket_not_needed(SUP, SUB),
	!.

print_bracket_if_needed(S, B, _, _) :-
	write(S, B).


bracket_not_needed(root, _).
bracket_not_needed(or, or).
bracket_not_needed(and, and).
bracket_not_needed(and, or).
bracket_not_needed(and, impl).
bracket_not_needed(and, equiv).
bracket_not_needed(or, impl).
bracket_not_needed(or, equiv).
bracket_not_needed(impl, equiv).


