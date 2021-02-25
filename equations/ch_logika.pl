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
	double_or,
	declare_statement(A, 'A',
		equiv(
			or(A, A),
			A
		)
	)
).

?- print_validated_formula(
	double_and,
	declare_statement(A, 'A',
		equiv(
			and(A, A),
			A
		)
	)
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
			or(and(A, B), C),
			and(or(A, C), or(B, C))
		)
	)))
).

?- print_validated_formula(
	and_distributivity,
	declare_statement(A, 'A', declare_statement(B, 'B', declare_statement(C, 'C',
		equiv(
			and(or(A, B), C),
			or(and(A, C), and(B, C))
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
		'forall_set',
		declare_predicate(A, 'A', [num_equal([Y, 1], 0), log_true, log_false],
			equiv(
				forall(X, 'x', Values, apply(A, [Y], [X])),
				set_equal([set_by(X, 'x', Values, not(apply(A, [Y], [X]))), empty_set])
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'exists_set',
		declare_predicate(A, 'A', [num_equal([Y, 1], 0), log_true, log_false],
			equiv(
				exists(X, 'x', Values, apply(A, [Y], [X])),
				set_not_equal(set_by(X, 'x', Values, apply(A, [Y], [X])), empty_set)
			)
		)
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
		'forall_not_eq_not_exists_proof',
		declare_predicate(P, 'P', [num_equal([Y, 1], 0), log_true, log_false],
			equiv([
				forall(X, 'x', Values, not(apply(P, [Y], [X]))),
				set_equal(set_by(X, 'x', Values, not(not(apply(P, [Y], [X])))), empty_set),
				set_equal(set_by(X, 'x', Values, apply(P, [Y], [X])), empty_set),
				not(exists(X, 'x', Values, apply(P, [Y], [X])))
			])
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
		'exists_not_eq_not_forall_proof',
		declare_predicate(P, 'P', [num_equal([Y, 1], 0), log_true, log_false],
			equiv([
				exists(X, 'x', Values, not(apply(P, [Y], [X]))),
				set_not_equal(set_by(X, 'x', Values, not(apply(P, [Y], [X]))), empty_set),
				not(forall(X, 'x', Values, apply(P, [Y], [X])))
			])
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'forall_and',
		declare_predicate(A, 'A', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
				equiv(
					forall(X, 'x', Values, and(apply(A, [Y], [X]), apply(B, [Y], [X]))),
					and(
						forall(X, 'x', Values, apply(A, [Y], [X])),
						forall(X, 'x', Values, apply(B, [Y], [X]))
					)
				)
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'forall_and_proof',
		declare_predicate(A, 'A', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
				equiv([
					forall(X, 'x', Values, and(apply(A, [Y], [X]), apply(B, [Y], [X]))),
					set_equal(set_by(X, 'x', Values, not(and(apply(A, [Y], [X]), apply(B, [Y], [X])))), empty_set),
					linebreak,
					set_equal(set_by(X, 'x', Values, or(not(apply(A, [Y], [X])), not(apply(B, [Y], [X])))), empty_set),
					set_equal(
						union(
							set_by(X, 'x', Values, not(apply(A, [Y], [X]))),
							set_by(X, 'x', Values, not(apply(B, [Y], [X])))
						),
						empty_set
					),
					linebreak,
					and(
						set_equal(set_by(X, 'x', Values, not(apply(A, [Y], [X]))), empty_set),
						set_equal(set_by(X, 'x', Values, not(apply(B, [Y], [X]))), empty_set)
					),
					and(
						forall(X, 'x', Values, apply(A, [Y], [X])),
						forall(X, 'x', Values, apply(B, [Y], [X]))
					)
				])
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'exists_or',
		declare_predicate(A, 'A', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
				equiv(
					exists(X, 'x', Values, or(apply(A, [Y], [X]), apply(B, [Y], [X]))),
					or(
						exists(X, 'x', Values, apply(A, [Y], [X])),
						exists(X, 'x', Values, apply(B, [Y], [X]))
					)
				)
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'exists_or_proof',
		declare_predicate(A, 'A', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Y, 1], 0), num_equal([Y, 2], 0), log_true, log_false],
				equiv([
					exists(X, 'x', Values, or(apply(A, [Y], [X]), apply(B, [Y], [X]))),
					set_not_equal(set_by(X, 'x', Values, or(apply(A, [Y], [X]), apply(B, [Y], [X]))), empty_set),
					linebreak,
					set_not_equal(
						union(
							set_by(X, 'x', Values, apply(A, [Y], [X])),
							set_by(X, 'x', Values, apply(B, [Y], [X]))
						),
						empty_set
					),
					or(
						set_not_equal(set_by(X, 'x', Values, apply(A, [Y], [X])), empty_set),
						set_not_equal(set_by(X, 'x', Values, apply(B, [Y], [X])), empty_set)
					),
					linebreak,
					or(
						exists(X, 'x', Values, apply(A, [Y], [X])),
						exists(X, 'x', Values, apply(B, [Y], [X]))
					)
				])
			)
		)
	).

?- 	Values = [1, 2],
	make_set([], S1),
	make_set([1], S2),
	make_set([2], S3),
	make_set([1, 2], S4),
	Sets = [set(S1), set(S2), set(S3), set(S4)],
	print_validated_formula(
		'set_equal_definition',
		declare_set(SA, 'S_A', Sets,
			declare_set(SB, 'S_B', Sets,
				equiv(
					set_equal(SA, SB),
					forall(X, 'x', Values, equiv(in(X, SA), in(X, SB)))
				)
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

?-	make_set([], S1),
	make_set([1], S2),
	make_set([2], S3),
	make_set([1, 2], S4),
	Sets = [set(S1), set(S2), set(S3), set(S4)],
	print_validated_formula(
		'union_empty_sets',
		declare_set(SA, 'S_A', Sets,
			declare_set(SB, 'S_B', Sets,
				equiv(
					set_equal(union(SA, SB), empty_set),
					and(set_equal(SA, empty_set), set_equal(SB, empty_set))
				)
			)
		)
	).

?- print_validated_formula(
	impl_swap_proof,
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv([
			impl(A, B),
			or(not(A), B),
			or(not(not(B)), not(A)),
			impl(not(B), not(A))
		])
	))
).

?- print_validated_formula(
	or_generalization_proof,
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv([
			or(and(not(A), B), A),
			or(and(not(A), B), and(A, or(B, not(B)))),
			linebreak,
			or(and(not(A), B), par(or(and(A, B), and(A, not(B))))),
			linebreak,
			or(and(not(A), B), or(and(A, B), or(and(A, B), and(A, not(B))))),
			linebreak,
			or(and(or(not(A), A), B), and(A, or(B, not(B)))),
			or(B, A),
			or(A, B)
		])
	))
).


?- print_validated_formula(
	impl_usage_proof,
	declare_statement(A, 'A', declare_statement(B, 'B',
		equiv([
			impl(and(A, impl(A, B)), B),
			or(not(and(A, impl(A, B))), B),
			or(or(not(A), not(impl(A, B))), B),
			linebreak,
			or(or(not(A), not(or(not(A), B))), B),
			or(or(not(A), and(A, not(B))), B),
			or(not(A), par(or(and(A, not(B)), B))),
			linebreak,
			or(not(A), par(or(B, A))),
			log_true
		])
	))
).

?- print_validated_formula(
	impl_transitivity_proof,
	declare_statement(A, 'A', declare_statement(B, 'B', declare_statement(C, 'C',
		equiv([
			impl(and(impl(A, B), impl(B, C)), impl(A, C)),
			or(not(and(impl(A, B), impl(B, C))), impl(A, C)),
			linebreak,
			or(or(not(impl(A, B)), not(impl(B, C))), impl(A, C)),
			or(or(not(or(not(A), B)), not(or(not(B), C))), or(not(A), C)),
			linebreak,
			or(or(and(A, not(B)), and(B, not(C))), or(not(A), C)),
			or(par(or(and(A, not(B)), not(A))), par(or(and(B, not(C)), C))),
			linebreak,
			or(par(or(not(A), not(B))), par(or(C, B))),
			log_true
		])
	)))
).

?- 	Values = [1, 2],
	print_validated_formula(
		'in_set_definition',
		declare_predicate(A, 'A', [num_equal([Z, 1], 0), num_equal([Z, 2], 0), log_true, log_false],
			declare_variable(X, 'x', Values,
				equiv(
					in(X, set_by(Y, 'y', Values, apply(A, [Z], [Y]))),
					apply(A, [Z], [X])
				)
			)
		)
	).
	
?- 	make_set([], S1),
	make_set([1], S2),
	make_set([2], S3),
	make_set([1, 2], S4),
	Sets = [set(S1), set(S2), set(S3), set(S4)],
	Values = [1, 2],
	print_validated_formula(
		'not_in_set_definition',
		declare_set(M, 'M', Sets,
			declare_variable(X, 'x', Values,
				equiv(
					not_in(X, M),
					not(in(X, M))
				)
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'intersection_in_1',
		declare_predicate(A, 'A', [num_equal([Z, 1], 0), num_equal([Z, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Z, 1], 0), num_equal([Z, 2], 0), log_true, log_false],
				declare_variable(Y, 'y', Values, equiv(and(apply(A, [Z], [Y]), apply(B, [Z], [Y])), and(apply(A, [Z], [Y]), apply(B, [Z], [Y]))))
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'intersection_in_2',
		declare_predicate(A, 'A', [num_equal([Z, 1], 0), num_equal([Z, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Z, 1], 0), num_equal([Z, 2], 0), log_true, log_false],
				declare_variable(Y, 'y', Values, equiv(
					in(Y, set_by(X, 'x', Values, and(apply(A, [Z], [X]), apply(B, [Z], [X])))),
					and(in(Y, set_by(X, 'x', Values, apply(A, [Z], [X]))), in(Y, set_by(X, 'x', Values, apply(B, [Z], [X]))))
				))
			)
		)
	).

?- 	Values = [1, 2],
	print_validated_formula(
		'intersection_in_3',
		declare_predicate(A, 'A', [num_equal([Z, 1], 0), num_equal([Z, 2], 0), log_true, log_false],
			declare_predicate(B, 'B', [num_equal([Z, 1], 0), num_equal([Z, 2], 0), log_true, log_false],
				declare_variable(Y, 'y', Values, equiv(
					in(Y, intersection(set_by(X, 'x', Values, apply(A, [Z], [X])), set_by(X, 'x', Values, apply(B, [Z], [X])))),
					and(in(Y, set_by(X, 'x', Values, apply(A, [Z], [X]))), in(Y, set_by(X, 'x', Values, apply(B, [Z], [X]))))
				))
			)
		)
	).

?- 	make_set([], S1),
	make_set([1], S2),
	make_set([2], S3),
	make_set([1, 2], S4),
	Sets = [set(S1), set(S2), set(S3), set(S4)],
	Values = [1, 2],
	print_validated_formula(
		'intersection_in_4',
		declare_set(MA, 'M_A', Sets,
			declare_set(MB, 'M_B', Sets,
				declare_variable(Y, 'y', Values, equiv(
					in(Y, intersection(MA, MB)),
					and(in(Y, MA), in(Y, MB))
				))
			)
		)
	).

?- 	make_set([], S1),
	make_set([1], S2),
	make_set([2], S3),
	make_set([1, 2], S4),
	Sets = [set(S1), set(S2), set(S3), set(S4)],
	Values = [1, 2],
	print_validated_formula(
		'union_in',
		declare_set(MA, 'M_A', Sets,
			declare_set(MB, 'M_B', Sets,
				declare_variable(Y, 'y', Values, equiv(
					in(Y, union(MA, MB)),
					or(in(Y, MA), in(Y, MB))
				))
			)
		)
	).

?- 	make_set([], S1),
	make_set([1], S2),
	make_set([2], S3),
	make_set([1, 2], S4),
	Sets = [set(S1), set(S2), set(S3), set(S4)],
	Values = [1, 2],
	print_validated_formula(
		'difference_in',
		declare_set(MA, 'M_A', Sets,
			declare_set(MB, 'M_B', Sets,
				declare_variable(Y, 'y', Values, equiv(
					in(Y, difference(MA, MB)),
					and(in(Y, MA), not_in(Y, MB))
				))
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

?- print_truth_table(
	or_associativity,
	[declare_statement(A, 'A'), declare_statement(B, 'B'), declare_statement(C, 'C')],
	[
		or(A, B),
		or(B, C),
		or(par(or(A, B)), C),
		or(A, par(or(B, C)))
	]
).

?- print_truth_table(
	and_associativity,
	[declare_statement(A, 'A'), declare_statement(B, 'B'), declare_statement(C, 'C')],
	[
		and(A, B),
		and(B, C),
		and(par(and(A, B)), C),
		and(A, par(and(B, C)))
	]
).

?- print_truth_table(
	or_distributivity,
	[declare_statement(A, 'A'), declare_statement(B, 'B'), declare_statement(C, 'C')],
	[
		and(A, B),
		or(A, C),
		or(B, C),
		or(and(A, B), C),
		and(or(A, C), or(B, C))
	]
).

?- print_truth_table(
	and_distributivity,
	[declare_statement(A, 'A'), declare_statement(B, 'B'), declare_statement(C, 'C')],
	[
		or(A, B),
		and(A, C),
		and(B, C),
		and(or(A, B), C),
		or(and(A, C), and(B, C))
	]
).

?- print_truth_table(
	de_morgan_or,
	[declare_statement(A, 'A'), declare_statement(B, 'B')],
	[
		or(A, B),
		not(A),
		not(B),
		not(or(A, B)),
		and(not(A), not(B))
	]
).

?- print_truth_table(
	de_morgan_and,
	[declare_statement(A, 'A'), declare_statement(B, 'B')],
	[
		and(A, B),
		not(A),
		not(B),
		not(and(A, B)),
		or(not(A), not(B))
	]
).

?- print_truth_table(
	impl_definition,
	[declare_statement(A, 'A'), declare_statement(B, 'B')],
	[
		not(A),
		or(not(A), B),
		impl(A, B)
	]
).

?- print_truth_table(
	equiv_to_impl,
	[declare_statement(A, 'A'), declare_statement(B, 'B')],
	[
		impl(A, B),
		impl(B, A),
		and(impl(A, B), impl(B, A)),
		equiv(A, B)
	]
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

