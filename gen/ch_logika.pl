
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

eval_formula(predicate(P, _, F), Y) :-
	findall(EF, (boolean(P), eval_formula(F, EF)), [EF1, EF2]),
	log_and(EF1, EF2, Y).


print_formula(F) :-
	eval_formula(F, log_true),
	print(F).

