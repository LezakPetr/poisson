
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


