
boolean(log_false).
boolean(log_true).

log_not(log_false, log_true).
log_not(log_true, log_false).

log_or(log_false, log_false, log_false).
log_or(log_true,  log_false, log_true).
log_or(log_true,  log_true,  log_false).
log_or(log_true,  log_true,  log_true).

log_and(log_false, log_false, log_false).
log_and(log_false, log_false, log_true).
log_and(log_false, log_true,  log_false).
log_and(log_true,  log_true,  log_true).

log_impl(log_true,  log_false, log_false).
log_impl(log_true,  log_false, log_true).
log_impl(log_false, log_true,  log_false).
log_impl(log_true,  log_true,  log_true).

log_equiv(log_true,  log_false, log_false).
log_equiv(log_false, log_false, log_true).
log_equiv(log_false, log_true,  log_false).
log_equiv(log_true,  log_true,  log_true).


% Logicke operace
eval_formula(log_false, log_false).
eval_formula(log_true, log_true).

eval_formula(Y, not(X)) :-
	eval_formula(EX, X),
	log_not(Y, EX).

eval_formula(Y, or(A, B)) :-
	eval_formula(EA, A),
	eval_formula(EB, B),
	log_or(Y, EA, EB).

eval_formula(Y, and(A, B)) :-
	eval_formula(EA, A),
	eval_formula(EB, B),
	log_and(Y, EA, EB).

eval_formula(Y, impl(A, B)) :-
	eval_formula(EA, A),
	eval_formula(EB, B),
	log_impl(Y, EA, EB).

eval_formula(Y, equiv(A, B)) :-
	eval_formula(EA, A),
	eval_formula(EB, B),
	log_equiv(Y, EA, EB).

eval_formula(Y, predicate(P, _, F)) :-
	findall(EF, (boolean(P), eval_formula(EF, F)), [EF1, EF2]),
	log_and(Y, EF1, EF2).


print_formula(F) :-
	eval_formula(log_true, F),
	print(F).

