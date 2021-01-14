
calc_file_path(L, PATH) :-
	atomic_list_concat(['../out/', L, '.tex'], PATH).


print_label(S, P, L) :-
	write(S, '\\label{'),
	write(S, P),
	write(S, L),
	writeln(S, '}').


