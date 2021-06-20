
preprocess_file(InputDir, OutputDir, Name) :-
	atomic_list_concat([InputDir, Name, '.tex'], InputPath),
	atomic_list_concat([OutputDir, Name, '.tex'], TexPath),
	atomic_list_concat([OutputDir, Name, '.pl'], PlPath),
	open(InputPath, 'read', InputFile),
	open(TexPath, 'write', TexFile),
	open(PlPath, 'write', PlFile),
	preprocess_tex(InputFile, TexFile, PlFile),
	writeln(PlFile, '?- halt.'),
	close(PlFile),
	close(TexFile),
	close(InputFile).


preprocess_file(Name) :-
	InputDir = '../',
	OutputDir = '../out/',
	preprocess_file(InputDir, OutputDir, Name).


preprocess_tex(InputFile, TexFile, PlFile) :-
	read_line_to_string(InputFile, Line),
	process_tex_line(InputFile, TexFile, PlFile, Line).


process_tex_line(_, _, _, end_of_file) :-
	!.

process_tex_line(InputFile, TexFile, PlFile, "\\begin{prolog}") :-
	!,
	preprocess_pl(InputFile, TexFile, PlFile).

process_tex_line(InputFile, TexFile, PlFile, Line) :-
	writeln(TexFile, Line),
	preprocess_tex(InputFile, TexFile, PlFile).


preprocess_pl(InputFile, TexFile, PlFile) :-
	read_line_to_string(InputFile, Line),
	process_pl_line(InputFile, TexFile, PlFile, Line).


process_pl_line(_, _, _, end_of_file) :-
	!.

process_pl_line(InputFile, TexFile, PlFile, "\\end{prolog}") :-
	!,
	preprocess_tex(InputFile, TexFile, PlFile).

process_pl_line(InputFile, TexFile, PlFile, Line) :-
	writeln(PlFile, Line),
	preprocess_pl(InputFile, TexFile, PlFile).

