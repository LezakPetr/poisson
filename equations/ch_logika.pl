:- ensure_loaded(formula).
:- ensure_loaded(truth_table).


%%%%% Tautologie %%%%%

?- print_validated_formula(
	or_symmetry,
	statement(A, 'A', statement(B, 'B',
		equiv(
			or(A, B),
			or(B, A)
		)
	))
).

?- print_validated_formula(
	and_symmetry,
	statement(A, 'A', statement(B, 'B',
		equiv(
			and(A, B),
			and(B, A)
		)
	))
).

?- print_validated_formula(
	or_associativity,
	statement(A, 'A', statement(B, 'B', statement(C, 'C',
		equiv(
			or(par(or(A, B)), C),
			or(A, par(or(B, C)))
		)
	)))
).

?- print_validated_formula(
	and_associativity,
	statement(A, 'A', statement(B, 'B', statement(C, 'C',
		equiv(
			and(par(and(A, B)), C),
			and(A, par(and(B, C)))
		)
	)))
).

?- print_validated_formula(
	or_distributivity,
	statement(A, 'A', statement(B, 'B', statement(C, 'C',
		equiv(
			or(par(and(A, B)), C),
			and(or(A, C), or(B, C))
		)
	)))
).

?- print_validated_formula(
	and_distributivity,
	statement(A, 'A', statement(B, 'B', statement(C, 'C',
		equiv(
			and(or(A, B), C),
			or(par(and(A, C)), par(and(B, C)))
		)
	)))
).

?- print_validated_formula(
	de_morgan_or,
	statement(A, 'A', statement(B, 'B',
		equiv(
			not(or(A, B)),
			and(not(A), not(B))
		)
	))
).

?- print_validated_formula(
	de_morgan_and,
	statement(A, 'A', statement(B, 'B',
		equiv(
			not(and(A, B)),
			or(not(A), not(B))
		)
	))
).

?- print_validated_formula(
	impl_definition,
	statement(A, 'A', statement(B, 'B',
		equiv(
			impl(A, B),
			or(not(A), B)
		)
	))
).

?- print_validated_formula(
	impl_usage,
	statement(A, 'A', statement(B, 'B',
		impl(
			and(
				A,
				impl(A, B)
			),
			B
		)
	))
).

?- print_validated_formula(
	impl_transitivity,
	statement(A, 'A', statement(B, 'B', statement(C, 'C',
		impl(
			and(
				impl(A, B),
				impl(B, C)
			),
			impl(A, C)
		)
	)))
).

?- print_validated_formula(
	impl_swap,
	statement(A, 'A', statement(B, 'B',
		equiv(
			impl(A, B),
			impl(not(B), not(A))
		)
	))
).

?- print_validated_formula(
	double_negation,
	statement(A, 'A',
		equiv(
			not(not(A)),
			A
		)
	)
).

?- print_validated_formula(
	excluded_middle,
	statement(A, 'A',
		or(A, not(A))
	)
).

?- print_validated_formula(
	and_both,
	statement(A, 'A',
		not(and(A, not(A)))
	)
).

?- print_validated_formula(
	or_specific,
	statement(A, 'A', statement(B, 'B',
		equiv(
			or(
				A,
				par(and(A, B))
			),
			A
		)
	))
).

?- print_validated_formula(
	equiv_to_impl,
	statement(A, 'A', statement(B, 'B',
		equiv(
			and(impl(A, B), impl(B, A)),
			equiv(A, B)
		)
	))
).



%%%%% Examples %%%%%

?- print_validated_formula(
	de_morgan_example,
	statement(A, 'A', statement(B, 'B',
		equiv(
			not(or(A, B)),
			and(not(A), not(B))
		)
	))
).

?- print_truth_table(
	de_morgan_example,
	[statement(A, 'A'), statement(B, 'B')],
	[
		or(A, B),
		not(or(A, B)),
		not(A),
		not(B),
		and(not(A), not(B)),
		equiv(
			not(or(A, B)),
			and(not(A), not(B))
		)
	]
).


?- print_truth_table(
	truth_not,
	[statement(A, 'A')],
	[not(A)]
).

?- print_truth_table(
	truth_equiv,
	[statement(A, 'A'), statement(B, 'B')],
	[equiv(A, B)]
).

?- print_truth_table(
	truth_and,
	[statement(A, 'A'), statement(B, 'B')],
	[and(A, B)]
).

?- print_truth_table(
	truth_or,
	[statement(A, 'A'), statement(B, 'B')],
	[or(A, B)]
).

?- print_truth_table(
	truth_impl,
	[statement(A, 'A'), statement(B, 'B')],
	[impl(A, B)]
).


?- print_validated_formula(
	tautology_example,
	statement(C, 'C', statement(D, 'D', statement(E, 'E',
		equiv(
			or(not(C), not(or(C, and(D, E)))),
			not(C)
		)
	)))
).


?- halt.

