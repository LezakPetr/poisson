
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Print of the header %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Prints '| c | c | ... |' with as many 'c' as elements in the list.
% print_columns(Stream, List)
print_columns(Stream, [_ | Tail]) :-
	write(Stream, '| c '),
	print_columns(Stream, Tail).

print_columns(Stream, []) :-
	write(Stream, '|').

% Prints statements in truth table header.
% Also assigns statement labels to statement variables.
% print_truth_table_header_statements(Stream, Statements)
print_truth_table_header_statements(Stream, [statement(Label, Label) | Tail]) :-
	write(Stream, '\\(\\predicate{'),
	write(Stream, Label),
	write(Stream, '}\\) & '),
	print_truth_table_header_statements(Stream, Tail).

print_truth_table_header_statements(_, []).


% Prints formulas in truth table header.
% Expects that statement labels has been assigned to statement variables.
print_truth_table_header_formulas(Stream, [Formula | Tail]) :-
	write(Stream, '\\('),
	print_formula_term(Stream, Formula),
	write(Stream, '\\)'),
	print_if_not_empty(Stream, ' & ', Tail),
	print_truth_table_header_formulas(Stream, Tail).

print_truth_table_header_formulas(_, []).


% Prints truth table header
print_truth_table_header(Stream, Statements, Formulas) :-
	copy_term([Statements, Formulas], [StatementsCopy, FormulasCopy]),
	write(Stream, '\\begin{tabular}{'),
	print_columns(Stream, StatementsCopy),
	print_columns(Stream, FormulasCopy),
	writeln(Stream, '}'),
	print_hline(Stream),
	print_truth_table_header_statements(Stream, StatementsCopy),
	print_truth_table_header_formulas(Stream, FormulasCopy),
	writeln(Stream, '\\\\').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% Print of the body %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Prints value rows in truth table. Combines all possible statement values.
% print_truth_table_values_combine(Stream, NotYetAssignedStatements, AllStatements, Formulas)
print_truth_table_values_combine(Stream, [statement(StatementVar, _) | Tail], AllStatements, Formulas) :-
	findall(Tmp, (boolean(StatementVar), Tmp = x, print_truth_table_values_combine(Stream, Tail, AllStatements, Formulas)), _),
	!.

print_truth_table_values_combine(Stream, [], AllStatements, Formulas) :-
	print_hline(Stream),
	print_truth_table_statement_values(Stream, AllStatements),
	print_truth_table_formula_values(Stream, Formulas),
	writeln(Stream, ' \\\\').

% Prints values of the statements.
% print_truth_table_statement_values(Stream, Statements)
print_truth_table_statement_values(Stream, [statement(StatementVar, _) | Tail]) :-
	print_boolean_in_math_mode(Stream, StatementVar),
	write(Stream, ' & '),
	print_truth_table_statement_values(Stream, Tail).

print_truth_table_statement_values(_, []).


% Prints values of the formulas.
% print_truth_table_formula_values(Stream, Formuas)
print_truth_table_formula_values(Stream, [Formula | Tail]) :-
	eval_formula(Formula, Value),
	print_boolean_in_math_mode(Stream, Value),
	print_if_not_empty(Stream, ' & ', Tail),
	print_truth_table_formula_values(Stream, Tail).
	
print_truth_table_formula_values(_, []).



% Prints truth table.
print_truth_table(Label, Statements, Formulas) :-
	calc_file_path(Label, Path),
	open(Path, write, Stream),
	print_truth_table_header(Stream, Statements, Formulas),
	print_hline(Stream),
	print_truth_table_values_combine(Stream, Statements, Statements, Formulas),
	print_hline(Stream),
	writeln(Stream, '\\end{tabular}'),
	close(Stream).


