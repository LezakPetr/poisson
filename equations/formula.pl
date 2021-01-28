
:- ensure_loaded(common).
:- ensure_loaded(expression).



% Validates formula (checks that it is true) and prints it.
print_validated_formula(Label, Formula) :-
	evaluate_expression(Formula, log_true),
	print_expression(Label, Formula).


