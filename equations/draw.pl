
:- ensure_loaded(expression).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 2D Point transformations %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Transforms point by given transformation.
% transform_point_2d(Transformation, OriginalPoint, TransformedPoint)
transform_point_2d(Transformation, [X, Y], [TX, TY]) :-
	copy_term(Transformation, transformation([X, Y], [FX, FY])),
	evaluate_expression(FX, TX),
	evaluate_expression(FY, TY).

?-	transform_point_2d(transformation([X, Y], [2*X, X + Y]), [2, 3], [4, 5]).

% Calculates compound transformation.
% compound_transform_2d(OuterTransformation, InnerTransformation, CompoundTransformation).
compound_transform_2d(TA, transformation([X, Y], [TBX, TBY]), transformation([X, Y], [TCX, TCY])) :-
	copy_term(TA, transformation([TBX, TBY], [TCX, TCY])).

?-	compound_transform_2d(transformation([A, B], [2*A, A + B]), transformation([C, D], [D + 3, C]), TC),
	transform_point_2d(TC, [3, -1], [4, 5]).

% Obtains identity transformation
identity_transform(transformation([X, Y], [X, Y])).

% Obtains transformation that flips X and Y axes
flip_xy_transform(transformation([X, Y], [Y, X])).


print_coords(Stream, [X, Y]) :-
	write(Stream, "("),
	write(Stream, X),
	write(Stream, ", "),
	write(Stream, Y),
	write(Stream, ")").


line_arrow(_, none, "").
line_arrow(begin, arrow, "<").
line_arrow(end, arrow, ">").

line_arg(arrows(Begin, End), Arg) :-
	line_arrow(begin, Begin, BeginArrow),
	line_arrow(end, End, EndArrow),
	atomic_list_concat([BeginArrow, "-", EndArrow], Arg).


line_arg_list([], []).

line_arg_list([Style | StyleTail], [Arg | ArgTail]) :-
	line_arg(Style, Arg),
	line_arg_list(StyleTail, ArgTail).

anchor_x(-1, "west").
anchor_x(0, "").
anchor_x(+1, "east").

anchor_y(-1, "south").
anchor_y(0, "").
anchor_y(+1, "north").

text_arg(anchor(X, Y), Arg) :-
	anchor_x(X, ArgX),
	anchor_y(Y, ArgY),
	atomic_list_concat(["anchor=", ArgY, " ", ArgX], Arg).

text_arg_list([], []).

text_arg_list([Style | StyleTail], [Arg | ArgTail]) :-
	text_arg(Style, Arg),
	text_arg_list(StyleTail, ArgTail).



print_element(Stream, line(A, B, StyleList)) :-
	write(Stream, "\\draw["),
	line_arg_list(StyleList, ArgList),
	write_list(Stream, ArgList),
	write(Stream, "] "),
	print_coords(Stream, A),
	write(Stream, " -- "),
	print_coords(Stream, B),
	writeln(Stream, ";").

print_element(Stream, text(Pos, Text, StyleList)) :-
	write(Stream, "\\draw "),
	print_coords(Stream, Pos),
	write(Stream, " node["),
	text_arg_list(StyleList, ArgList),
	write_list(Stream, ArgList),
	write(Stream, "]{"),
	write(Stream, Text),
	writeln(Stream, "};").


print_element_list(Stream, [Element | Tail]) :-
	print_element(Stream, Element),
	print_element_list(Stream, Tail).

print_element_list(_, []).


print_image_to_file(Label, ElementList) :-
	calc_file_path('img', Label, Path),
	open(Path, write, Stream),
	print_element_list(Stream, ElementList),
	close(Stream).
	

draw_line(Transform, A, B, Style, [Element]) :-
	transform_point_2d(Transform, A, TransformedA),
	transform_point_2d(Transform, B, TransformedB),
	Element = line(TransformedA, TransformedB, Style).


draw_text(Transform, Point, Text, Style, [Element]) :-
	transform_point_2d(Transform, Point, TransformedPoint),
	Element = text(TransformedPoint, Text, Style).



