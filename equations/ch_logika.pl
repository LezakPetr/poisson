:- ensure_loaded(formula).
:- ensure_loaded(truth_table).


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
			or(or(A, B), C),
			or(A, or(B, C))
		)
	)))
).

?- print_validated_formula(
	and_associativity,
	statement(A, 'A', statement(B, 'B', statement(C, 'C',
		equiv(
			and(and(A, B), C),
			and(A, and(B, C))
		)
	)))
).

?- print_validated_formula(
	or_distributivity,
	statement(A, 'A', statement(B, 'B', statement(C, 'C',
		equiv(
			or(and(A, B), C),
			and(or(A, C), or(B, C))
		)
	)))
).

?- print_validated_formula(
	and_distributivity,
	statement(A, 'A', statement(B, 'B', statement(C, 'C',
		equiv(
			and(or(A, B), C),
			or(and(A, C), and(B, C))
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
	impl_definition,
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



?- halt.

