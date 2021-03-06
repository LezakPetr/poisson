
num_tolerance(1e-4).
num_equal_tolerances(tolerances(8e-6, 1e-6)).
num_imag_tolerance(1e-4).


verbose :- fail.


log([]) :-
	verbose,
	!,
	writeln("").

log([V | Tail]) :-
	verbose,
	!,
	write(V),
	log(Tail).

log(_).


% Casts given number to real number.
as_real(X, X) :-
	number(X).

as_real(complex(Re, Im), Re) :-	
	number(Re),
	num_imag_tolerance(Tolerance),
	abs(Im) =< Tolerance.


?- as_real(2.5, 2.5).
?- as_real(complex(-1.3, 0), -1.3).
?- as_real(complex(2.7, 0.0), 2.7).
?- \+ as_real(complex(2.7, 1.1), _).

% Casts given number to complex number.
as_complex(X, complex(X, 0)) :-
	number(X).

as_complex(complex(Re, Im), complex(Re, Im)) :-	
	number(Re),
	number(Im).

?- as_complex(2.5, complex(2.5, 0)).
?- as_complex(complex(-1.3, 5), complex(-1.3, 5)).

complex_all_equal(Values) :-
	num_equal_tolerances(Tolerances),
	complex_all_equal(Values, Tolerances).


complex_all_equal([_], _).

complex_all_equal([A, B | Tail], Tolerances) :-
	complex_equal(A, B, Tolerances),
	complex_all_equal([B | Tail], Tolerances).


complex_negate(A, Y) :-
	number(A),
	!,
	Y is -A.

complex_negate(A, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	ReY is -ReA,
	ImY is -ImA.

?-	complex_negate(2, -2).
?-	complex_negate(complex(1, 2), complex(-1, -2)).


complex_add(A, B, Y) :-
	number(A),
	number(B),
	!,
	Y is A + B.

complex_add(A, B, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	as_complex(B, complex(ReB, ImB)),
	ReY is ReA + ReB,
	ImY is ImA + ImB.

?-	complex_add(2, 3, 5).
?-	complex_add(5, complex(3, 4), complex(8, 4)).
?-	complex_add(complex(3, 4), 5, complex(8, 4)).
?-	complex_add(complex(1, 2), complex(3, 4), complex(4, 6)).


complex_subtract(A, B, Y) :-
	number(A),
	number(B),
	!,
	Y is A - B.

complex_subtract(A, B, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	as_complex(B, complex(ReB, ImB)),
	ReY is ReA - ReB,
	ImY is ImA - ImB.

?-	complex_subtract(2, 3, -1).
?-	complex_subtract(5, complex(3, 4), complex(2, -4)).
?-	complex_subtract(complex(3, 4), 5, complex(-2, 4)).
?-	complex_subtract(complex(1, 2), complex(3, 4), complex(-2, -2)).


complex_abs(Z, Abs) :-
	as_complex(Z, complex(X, Y)),
	Abs is sqrt(X^2 + Y^2).

?-	complex_abs(-2, 2.0).
?-	complex_abs(complex(3, -4), 5.0).

complex_arg(Z, Arg) :-
	as_complex(Z, complex(X, Y)),
	Arg is atan2(Y, X).


complex_equal(A, B) :-
	num_equal_tolerances(Tolerances),
	complex_equal(A, B, Tolerances).


complex_equal(A, B, tolerances(AbsTolerance, RelTolerance)) :-
	complex_abs(A, AbsA),
	complex_abs(B, AbsB),
	Tolerance is max(AbsTolerance, RelTolerance * max(AbsA, AbsB)),
	complex_subtract(A, B, Diff),
	complex_abs(Diff, AbsDiff),
	AbsDiff =< Tolerance,
	!.

complex_equal(A, B, _) :- verbose, write("Equal failed: "), writeln([A, B]), fail.

?-	complex_equal(2, 2).
?-	complex_equal(-1, complex(-1, 0)).
?-	complex_equal(complex(5, 0), 5).
?-	complex_equal(complex(5, 3), complex(5, 3)).
?-	\+ complex_equal(complex(7, 3), complex(5, 3)).
?-	\+ complex_equal(complex(5, -3), complex(5, 3)).


complex_multiply(A, B, Y) :-
	number(A),
	number(B),
	!,
	Y is A * B.

complex_multiply(A, B, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	as_complex(B, complex(ReB, ImB)),
	ReY is ReA * ReB - ImA * ImB,
	ImY is ReA * ImB + ImA * ReB.

?-	complex_multiply(2, 3, 6).
?-	complex_multiply(5, complex(3, 4), complex(15, 20)).
?-	complex_multiply(complex(3, 4), 5, complex(15, 20)).
?-	complex_multiply(complex(1, 2), complex(3, 4), complex(-5, 10)).

complex_divide(A, B, Y) :-
	number(A),
	number(B),
	!,
	Y is A / B.

complex_divide(A, B, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	as_complex(B, complex(ReB, ImB)),
	ReY is (ReA * ReB + ImA * ImB) / (ReB^2 + ImB^2),
	ImY is (ImA * ReB - ReA * ImB) / (ReB^2 + ImB^2).


complex_exp(A, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	ReY is exp(ReA) * cos(ImA),
	ImY is exp(ReA) * sin(ImA).

?-	complex_exp(0, Y),
	complex_equal(Y, 1).

?-	complex_exp(1, Y),
	E is e,
	complex_equal(Y, E).

?-	Pi is pi,
	complex_exp(complex(0, Pi), Y),
	complex_equal(Y, -1).


complex_ln(A, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	ReY is log(sqrt(ReA * ReA + ImA * ImA)),
	ImY is atan2(ImA, ReA).

complex_pow(A, B, Y) :-
	number(A),
	number(B),
	(A >= 0; integer(B)),
	!,
	Y is A^B.

complex_pow(A, B, Y) :-
	complex_ln(A, LnA),
	complex_multiply(B, LnA, Exponent),
	complex_exp(Exponent, Y).

?-	complex_pow(2, 3, Y),
	complex_equal(Y, 8).

?-	complex_pow(complex(2, 0), complex(3, 0), Y),
	complex_equal(Y, 8).

?-	complex_pow(-3, 5, Y),
	complex_equal(Y, -243).

?-	complex_pow(complex(-3, 0), complex(5, 0), Y),
	complex_equal(Y, -243).

?-	complex_pow(complex(0, 1), 2, Y),
	complex_equal(Y, -1).

?-	complex_pow(complex(0, -1), 2, Y),
	complex_equal(Y, -1).

?-	complex_pow(-1, 0.5, Y),
	complex_equal(Y, complex(0, 1)).

complex_sum([], 0).

complex_sum([Value | Tail], Sum) :-
	complex_sum(Tail, SubSum),
	complex_add(Value, SubSum, Sum).

?-	complex_sum([1, 2, 3], 6).


% Calculates symbolic sum of two numbers, but optimizes situation where one or both arguments are known.
% symbolic_add(A, B, Y) :-
% Y = A + B.
symbolic_add(A, B, Y) :-
	number(A),
	A =:= 0,
	!,
	Y = B.

symbolic_add(A, B, Y) :-
	number(B),
	B =:= 0,
	!,
	Y = A.

symbolic_add(A, B, Y) :-
	number(A),
	number(B),
	!,
	Y is A + B. 

symbolic_add(A, B, A + B).


% Calculates symbolic multiply of two numbers, but optimizes situation where one or both arguments are known.
% symbolic_multiply(A, B, Y) :-
% Y = A * B.
symbolic_multiply(A, _, Y) :-
	number(A),
	A =:= 0,
	!,
	Y = 0.

symbolic_multiply(_, B, Y) :-
	number(B),
	B =:= 0,
	!,
	Y = 0.

symbolic_multiply(A, B, Y) :-
	number(A),
	A =:= 1,
	!,
	Y = B.

symbolic_multiply(A, B, Y) :-
	number(B),
	B =:= 1,
	!,
	Y = A.

symbolic_multiply(A, B, Y) :-
	number(A),
	number(B),
	!,
	Y is A * B. 

symbolic_multiply(A, B, A * B).


sqrt(A, B, Y) :-
	integer(A),
	B < 0,
	A mod 2 =:= 1,
	Y is -((-B)^(1 / A)),
	!.

sqrt(A, B, Y) :-
	Y is B^(1 / A).


?-	sqrt(3, 8, Y),
	complex_equal(Y, 2).

?-	sqrt(3, -8, Y),
	complex_equal(Y, -2).

?-	sqrt(2, 16, Y),
	complex_equal(Y, 4).
