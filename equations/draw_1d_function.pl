:- ensure_loaded(expression).


draw_1d_function(Label, X, Formula, MinX, MaxX) :-
	calc_file_path('img', Label, Path),
	open(Path, write, Stream),
	calculate_function_points(X, Formula, MinX, MaxX, PointList),
	draw_function_points(Stream, PointList),
	close(Stream).


sample_1d(MinX, MaxX, X) :-
	Count = 100,
	between(0, Count, N),
	X is MinX + (MaxX - MinX) * N / Count.

calculate_function_points(X, Formula, MinX, MaxX, PointList) :-
	findall([SampleX, Y], (sample_1d(MinX, MaxX, SampleX), X = SampleX, evaluate_expression(Formula, Y)), PointList).

draw_function_points(_, [_]).

draw_function_points(Stream, [A, B | Tail]) :-
	write(Stream, "\\draw "),
	print_coords(Stream, A),
	write(Stream, " -- "),
	print_coords(Stream, B),
	writeln(Stream, ";"),
	draw_function_points(Stream, [B | Tail]).

print_coords(Stream, [X, Y]) :-
	write(Stream, "("),
	write(Stream, X),
	write(Stream, ", "),
	write(Stream, Y),
	write(Stream, ")").


