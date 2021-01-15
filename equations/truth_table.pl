
% Prints '| c | c | ... |' with as many 'c' as elements in the list.
% print_columns(Stream, List)
print_columns(Stream, [_ | Tail]) :-
	write(Stream, '| c '),
	print_columns(Stream, Tail).

print_columns(Stream, []) :-
	write(Stream, '|').

% Prints statements in truth table header.
% Assigns statement labels to statement variables.
% print_truth_table_header_statements(Stream, Statements)
print_truth_table_header_statements(Stream, [statement(Label, Label) | Tail]) :-
	write(Stream, '\\(\\predicate{'),
	write(Stream, Label),
	write(Stream, '}\\) & '),
	print_truth_table_header_statements(Stream, Tail).

print_truth_table_header_statements(_, []).


% Prints formulas in truth table header.
print_truth_table_header_formulas(Stream, [Formula | Tail]) :-
	write(Stream, '\\('),
	print_formula_term(Stream, Formula),
	writeln(Stream, '\\)'),
	print_truth_table_header_formulas(Stream, Tail).

print_truth_table_header_formulas(_, []).


% Prints truth table header
print_truth_table_header(Stream, Statements, Formulas) :-
	write(Stream, '\\begin{tabular}{'),
	print_columns(Stream, Statements),
	print_columns(Stream, Formulas),
	writeln(Stream, '}'),
	print_hline(Stream),
	print_truth_table_header_statements(Stream, Statements),
	print_truth_table_header_formulas(Stream, Formulas),
	writeln(Stream, '\\\\').

% Prints value rows in truth table.
% print_truth_table_values(Stream, Statements, Formula
print_truth_table_values_combine(Stream, [statement(StatementVar, _) | Tail], AllStatements, Formulas) :-
	findall(Tmp, (boolean(StatementVar), Tmp = x, print_truth_table_values_combine(Stream, Tail, AllStatements, Formulas)), _),
	!.

print_truth_table_values_combine(Stream, [], AllStatements, Formulas) :-
	print_hline(Stream),
	print_truth_table_inputs(Stream, AllStatements),
	print_truth_table_values(Stream, Formulas),
	writeln(Stream, ' \\\\').
	

print_truth_table_inputs(Stream, [statement(StatementVar, _) | Tail]) :-
	print_boolean_in_math_mode(Stream, StatementVar),
	write(Stream, ' & '),
	print_truth_table_inputs(Stream, Tail).

print_truth_table_inputs(_, []).


print_truth_table_values(Stream, [Formula | Tail]) :-
	eval_formula(Formula, Value),
	print_boolean_in_math_mode(Stream, Value),
	print_truth_table_values(Stream, Tail).
	
print_truth_table_values(_, []).


print_truth_table(Label, Statements, Formulas) :-
	calc_file_path(Label, Path),
	open(Path, write, Stream),
	copy_term([Statements, Formulas], [Statements2, Formulas2]),
	print_truth_table_header(Stream, Statements, Formulas),
	print_hline(Stream),
	print_truth_table_values_combine(Stream, Statements2, Statements2, Formulas2),
	print_hline(Stream),
	writeln(Stream, '\\end{tabular}'),
	close(Stream).


print_hline(S) :-
	writeln(S, '\\hline').

