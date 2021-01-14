
extract_statements(statement(S, L, F), [statement(S, L) | N], FORMULA) :-
	extract_statements(F, N, FORMULA),
	!.

extract_statements(FORMULA, [], FORMULA).


print_truth_table_statements(S, [statement(_, L) | N]) :-
	write(S, '\\(\\predicate{'),
	write(S, L),
	write(S, '}\\) & '),
	print_truth_table_statements(S, N).

print_truth_table_statements(_, _).


print_truth_table_header(S, F) :-
	extract_statements(F, STATEMENTS, _),
	print_truth_table_statements(S, STATEMENTS),
	write(S, '\\('),
	print_formula_term(S, F),
	writeln(S, '\\) \\\\').


print_truth_table_values(S, STATEMENTS, statement(ST, _, F)) :-
	findall(TMP, (boolean(ST), TMP = x, print_truth_table_values(S, STATEMENTS, F)), _),
	!.

print_truth_table_values(S, STATEMENTS, F) :-
	print_hline(S),
	eval_formula(F, VAL),
	print_truth_table_inputs(S, STATEMENTS),
	print_boolean_in_math_mode(S, VAL),
	writeln(S, ' \\\\').
	

print_truth_table_inputs(S, [statement(ST, _) | N]) :-
	print_boolean_in_math_mode(S, ST),
	write(S, ' & '),
	print_truth_table_inputs(S, N).

print_truth_table_inputs(_, []).
	
	
print_truth_table(L, F) :-
	calc_file_path(L, PATH),
	open(PATH, write, S),
	writeln(S, '\\begin{tabular}{| c | c || c |}'),
	print_hline(S),
	copy_term(F, F2),
	print_truth_table_header(S, F),
	extract_statements(F2, STATEMENTS, _),
	print_hline(S),
	print_truth_table_values(S, STATEMENTS, F2),
	print_hline(S),
	writeln(S, '\\end{tabular}'),
	close(S).


print_hline(S) :-
	writeln(S, '\\hline').

