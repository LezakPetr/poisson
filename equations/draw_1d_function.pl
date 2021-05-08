:- ensure_loaded(expression).
:- ensure_loaded(draw).


draw_1d_function(Label, X, Formula, MinX, MaxX) :-
	calculate_function_points(X, Formula, MinX, MaxX, PointList),
	identity_transform(TransformAxisX),
	draw_axis(TransformAxisX, min(MinX, 0), max(MaxX, 0), AxisXElements),
	all_nth0(1, PointList, CoordsY),
	min_list(CoordsY, MinY),
	max_list(CoordsY, MaxY),
	flip_xy_transform(TransformAxisY),
	draw_axis(TransformAxisY, min(MinY, 0), max(MaxY, 0), AxisYElements),
	draw_function_points(PointList, PointElements),
	append([AxisXElements, AxisYElements, PointElements], ElementList),
	print_image_to_file(Label, ElementList).


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


marks_length(0.15).


draw_axis(Transform, Min, Max, ElementList) :-
	Begin is Min - 0.5,
	End is Max + 0.5,
	draw_line(Transform, [Begin, 0], [End, 0], [arrows(none, arrow)], AxisElements),
	draw_marks(Transform, Min, Max, MarksElements),
	append([AxisElements, MarksElements], ElementList).

draw_marks(Transform, MinX, MaxX, ElementList) :-
	Begin is ceil(MinX),
	End is floor(MaxX),
	draw_marks_int(Transform, Begin, End, ElementList).

draw_marks_int(Transform, X, End, ElementList) :-
	End >= X,
	!,
	marks_length(MarksLength),
	draw_line(Transform, [X, -MarksLength], [X, MarksLength], [], MarkElements),
	draw_text(Transform, [X, 0], X, [anchor(+1, +1)], LabelElements),
	Next is X + 1,
	draw_marks_int(Transform, Next, End, SubElementList),
	append([MarkElements, LabelElements, SubElementList], ElementList).

draw_marks_int(_, _, _, []).


