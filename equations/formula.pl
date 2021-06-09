
:- ensure_loaded(common).
:- ensure_loaded(expression).


verify_formula(Formula) :-
	evaluate_expression(Formula, Value),
	!,
	verify_true(Value).

verify_formula(_) :-
	writeln("Formula cannot be evaluated"),
	fail.


verify_true(log_true) :-
	!.

verify_true(_) :-
	writeln("Formula not truthful"),
	fail.

rewrite_formula_and_validate(Formula) :-
	rewrite_expression(Formula, RewrittenFormula),
	log(["Rewritten formula: ", RewrittenFormula]),
	verify_formula(RewrittenFormula).
	
% Validates formula (checks that it is true) and prints it.
print_validated_formula(Label, Formula) :-
	copy_term(Formula, CopiedFormula),
	rewrite_formula_and_validate(Formula),
	print_formula(Label, CopiedFormula).


print_formula(Label, CopiedFormula) :-
	print_expression(Label, CopiedFormula),
	!.

print_formula(_, _) :-
	writeln("Formula cannot be printed"),
	fail.


print_validated_multiformula(_, []).

print_validated_multiformula(Declarations, [formula(Label, Line) | LineTail]) :-
	copy_term(declare(Declarations, Line), Formula),
	print_validated_formula(Label, Formula),
	print_validated_multiformula(Declarations, LineTail).

