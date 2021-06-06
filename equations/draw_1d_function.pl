:- ensure_loaded(expression).
:- ensure_loaded(draw).


draw_1d_function(Label, X, LabelX, Formula, MinX, MaxX) :-
	calculate_1d_function_image(X, LabelX, Formula, MinX, MaxX, ElementList),
	print_image_to_file(Label, ElementList).

calculate_1d_function_image(X, LabelX, Formula, MinX, MaxX, ElementList) :-
	calculate_function_points(X, Formula, MinX, MaxX, PointList),
	all_nth0(1, PointList, CoordsY),
	min_list(CoordsY, MinY),
	max_list(CoordsY, MaxY),
	UpdatedMinX is min(MinX, 0),
	UpdatedMaxX is max(MaxX, 0),
	UpdatedMinY is min(MinY - 1e-2, 0),
	UpdatedMaxY is max(MaxY + 1e-2, 0),
	identity_transform(TransformAxisX),
	flip_xy_transform(TransformAxisY),
	draw_marks(TransformAxisX, UpdatedMinX, UpdatedMaxX, UpdatedMinY, UpdatedMaxY, MarksElementsX),
	draw_marks(TransformAxisY, UpdatedMinY, UpdatedMaxY, UpdatedMinX, UpdatedMaxX, MarksElementsY),
	draw_axis(TransformAxisX, UpdatedMinX, UpdatedMaxX, LabelX, AxisXElements),
	calculate_label_y(declare([variable(X, LabelX, [])], Formula), LabelY),
	draw_axis(TransformAxisY, UpdatedMinY, UpdatedMaxY, LabelY, AxisYElements),
	draw_function_points(PointList, PointElements),
	append([MarksElementsX, MarksElementsY, AxisXElements, AxisYElements, PointElements], ElementList).
	
calculate_label_y(Formula, LabelY) :-
	tmp_file_stream('text', File, OutStream),
	print_expression_term(OutStream, Formula),
	close(OutStream),
	open(File, 'read', InStream),
	read_line_to_string(InStream, LabelY),
	close(InStream),
	delete_file(File).



sample_1d(MinX, MaxX, X) :-
	Count = 100,
	between(0, Count, N),
	X is MinX + (MaxX - MinX) * N / Count.

calculate_function_points(X, Formula, MinX, MaxX, PointList) :-
	findall([SampleX, Y], (sample_1d(MinX, MaxX, SampleX), X = SampleX, evaluate_expression(Formula, Y)), PointList).

draw_function_points([_], []).

draw_function_points([A, B | Tail], Elements) :-
	identity_transform(Transform),
	draw_line(Transform, A, B, [], LineElements),
	draw_function_points([B | Tail], SubElements),
	append([LineElements, SubElements], Elements).


draw_axis(Transform, MinX, MaxX, Label, ElementList) :-
	Begin is MinX - 0.5,
	End is MaxX + 0.5,
	draw_line(Transform, [Begin, 0], [End, 0], [arrows(none, arrow)], AxisElements),
	atomic_list_concat(["\\(", Label, "\\)"], Text),
	draw_text(Transform, [End, 0], Text, [anchor(+1, +1)], LabelElements),
	append([AxisElements, LabelElements], ElementList).

draw_marks(Transform, MinX, MaxX, MinY, MaxY, ElementList) :-
	Begin is ceil(MinX),
	End is floor(MaxX),
	draw_marks_int(Transform, Begin, End, MinY, MaxY, ElementList).

draw_marks_int(Transform, X, End, MinY, MaxY, ElementList) :-
	End >= X,
	!,
	draw_line(Transform, [X, MinY], [X, MaxY], [color(lightgray)], MarkElements),
	draw_text(Transform, [X, 0], X, [anchor(+1, +1)], LabelElements),
	Next is X + 1,
	draw_marks_int(Transform, Next, End, MinY, MaxY, SubElementList),
	append([MarkElements, LabelElements, SubElementList], ElementList).

draw_marks_int(_, _, _, _, _, []).


