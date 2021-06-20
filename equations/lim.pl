
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



