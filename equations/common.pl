
% Calculates path to the file for given label
calc_file_path(Label, Path) :-
	atomic_list_concat(['../out/', Label, '.tex'], Path).

% Prints label
print_label(Stream, Prefix, Label) :-
	write(Stream, '\\label{'),
	write(Stream, Prefix),
	write(Stream, Label),
	writeln(Stream, '}').


