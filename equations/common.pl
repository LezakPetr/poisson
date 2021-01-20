
% Calculates path to the file for given label
calc_file_path(Prefix, Label, Path) :-
	atomic_list_concat(['../out/', Prefix, '_', Label, '.tex'], Path).

% Prints label
print_label(Stream, Prefix, Label) :-
	write(Stream, '\\label{'),
	write(Stream, Prefix),
	write(Stream, Label),
	writeln(Stream, '}').

% Prints horizontal line
print_hline(Stream) :-
	writeln(Stream, '\\hline').


% Prints given text in case that list is not empty.
% print_if_not_empty(Stream, Text, List)
print_if_not_empty(_, _, []).

print_if_not_empty(Stream, Text, [_ | _]) :-
	write(Stream, Text).

