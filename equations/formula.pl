
:- ensure_loaded(common).
:- ensure_loaded(expression).


verify_formula(Formula) :-
	evaluate_expression(Formula, log_true),
	!.

verify_formula(_) :-
	throw(formula_evaluation_failed).

% Validates formula (checks that it is true) and prints it.
print_validated_formula(Label, Formula) :-
	rewrite_expression(Formula, RewrittenFormula),
	verify_formula(RewrittenFormula),
	print_expression(Label, Formula).


