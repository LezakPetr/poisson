:- ensure_loaded(formula).
:- ensure_loaded(truth_table).


%%%%% Tautologie %%%%%

?- print_validated_formula(
	or_symmetry,
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv(
			or(A, B),
			or(B, A)
		)
	))
).

?- print_validated_formula(
	and_symmetry,
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv(
			and(A, B),
			and(B, A)
		)
	))
).

?- print_validated_formula(
	or_associativity,
	declare_statement(A, 'A', declare_statement(B, 'B', declare_statement(C, 'C',
		equiv(
			or(par(or(A, B)), C),
			or(A, par(or(B, C)))
		)
	)))
).

?- print_validated_formula(
	and_associativity,
	declare_statement(A, 'A', declare_statement(B, 'B', declare_statement(C, 'C',
		equiv(
			and(par(and(A, B)), C),
			and(A, par(and(B, C)))
		)
	)))
).

?- print_validated_formula(
	or_distributivity,
	declare_statement(A, 'A', declare_statement(B, 'B', declare_statement(C, 'C',
		equiv(
			or(par(and(A, B)), C),
			and(or(A, C), or(B, C))
		)
	)))
).

?- print_validated_formula(
	and_distributivity,
	declare_statement(A, 'A', declare_statement(B, 'B', declare_statement(C, 'C',
		equiv(
			and(or(A, B), C),
			or(par(and(A, C)), par(and(B, C)))
		)
	)))
).

?- print_validated_formula(
	de_morgan_or,
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv(
			not(or(A, B)),
			and(not(A), not(B))
		)
	))
).

?- print_validated_formula(
	de_morgan_and,
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv(
			not(and(A, B)),
			or(not(A), not(B))
		)
	))
).

?- print_validated_formula(
	impl_definition,
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv(
			impl(A, B),
			or(not(A), B)
		)
	))
).

?- print_validated_formula(
	impl_usage,
	declare_statement(A, 'A', declare_statement(B, 'B',
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
	declare_statement(A, 'A', declare_statement(B, 'B', declare_statement(C, 'C',
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
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv(
			impl(A, B),
			impl(not(B), not(A))
		)
	))
).

?- print_validated_formula(
	double_negation,
	declare_statement(A, 'A',
		equiv(
			not(not(A)),
			A
		)
	)
).

?- print_validated_formula(
	excluded_middle,
	declare_statement(A, 'A',
		or(A, not(A))
	)
).

?- print_validated_formula(
	and_both,
	declare_statement(A, 'A',
		not(and(A, not(A)))
	)
).

?- print_validated_formula(
	or_specific,
	declare_statement(A, 'A', declare_statement(B, 'B',
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
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv(
			and(impl(A, B), impl(B, A)),
			equiv(A, B)
		)
	))
).

?- 	Values = [1, 2],
	print_validated_formula(
		'forall_not_eq_not_exists',
		declare_predicate(P, 'P', [num_equal([Y, 1], 0), log_true, log_false],
			equiv(
				forall(X, 'x', Values, not(apply(P, [Y], [X]))),
				not(exists(X, 'x', Values, apply(P, [Y], [X])))
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'exists_not_eq_not_forall',
		declare_predicate(P, 'P', [num_equal([Y, 1], 0), log_true, log_false],
			equiv(
				exists(X, 'x', Values, not(apply(P, [Y], [X]))),
				not(forall(X, 'x', Values, apply(P, [Y], [X])))
			)
		)
	).



?- 	Values = [1, 2],
	print_validated_formula(
		'intersection_definition',
		declare_predicate(A, 'A', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
				set_equal([
					intersection(
						set_by(X, 'x', Values, apply(A, [Y], [X])),
						set_by(X, 'x', Values, apply(B, [Y], [X]))
					),
					set_by(X, 'x', Values, and(apply(A, [Y], [X]), apply(B, [Y], [X])))
				])
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'union_definition',
		declare_predicate(A, 'A', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
				set_equal([
					union(
						set_by(X, 'x', Values, apply(A, [Y], [X])),
						set_by(X, 'x', Values, apply(B, [Y], [X]))
					),
					set_by(X, 'x', Values, or(apply(A, [Y], [X]), apply(B, [Y], [X])))
				])
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'difference_definition',
		declare_predicate(A, 'A', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
				set_equal([
					difference(
						set_by(X, 'x', Values, apply(A, [Y], [X])),
						set_by(X, 'x', Values, apply(B, [Y], [X]))
					),
					set_by(X, 'x', Values, and(apply(A, [Y], [X]), not(apply(B, [Y], [X])))),
					set_by(X, 'x', Values, not(impl(apply(A, [Y], [X]), apply(B, [Y], [X]))))
				])
			)
		)
	).



%%%%% Examples %%%%%

?- print_validated_formula(
	de_morgan_example,
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv(
			not(or(A, B)),
			and(not(A), not(B))
		)
	))
).

?- print_truth_table(
	de_morgan_example,
	[declare_statement(A, 'A'), declare_statement(B, 'B')],
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
	[declare_statement(A, 'A')],
	[not(A)]
).

?- print_truth_table(
	truth_equiv,
	[declare_statement(A, 'A'), declare_statement(B, 'B')],
	[equiv(A, B)]
).

?- print_truth_table(
	truth_and,
	[declare_statement(A, 'A'), declare_statement(B, 'B')],
	[and(A, B)]
).

?- print_truth_table(
	truth_or,
	[declare_statement(A, 'A'), declare_statement(B, 'B')],
	[or(A, B)]
).

?- print_truth_table(
	truth_impl,
	[declare_statement(A, 'A'), declare_statement(B, 'B')],
	[impl(A, B)]
).


?- print_validated_formula(
	tautology_example,
	declare_statement(C, 'C', declare_statement(D, 'D', declare_statement(E, 'E',
		equiv(
			or(not(C), not(or(C, and(D, E)))),
			not(C)
		)
	)))
).


?- halt.

