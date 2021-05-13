
num_tolerance(1e-12).



% Casts given number to real number.
as_real(X, X) :-
	number(X).

as_real(complex(Re, Im), Re) :-	
	number(Re),
	num_tolerance(Tolerance),
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

complex_equal(A, B) :-
	as_complex(A, complex(ReA, ImA)),
	as_complex(B, complex(ReB, ImB)),
	num_tolerance(Tolerance),
	abs(ReA - ReB) =< Tolerance,
	abs(ImA - ImB) =< Tolerance.

?-	complex_equal(2, 2).
?-	complex_equal(-1, complex(-1, 0)).
?-	complex_equal(complex(5, 0), 5).
?-	complex_equal(complex(5, 3), complex(5, 3)).
?-	\+ complex_equal(complex(7, 3), complex(5, 3)).
?-	\+ complex_equal(complex(5, -3), complex(5, 3)).

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


complex_plus(A, B, Y) :-
	number(A),
	number(B),
	!,
	Y is A + B.

complex_plus(A, B, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	as_complex(B, complex(ReB, ImB)),
	ReY is ReA + ReB,
	ImY is ImA + ImB.

?-	complex_plus(2, 3, 5).
?-	complex_plus(5, complex(3, 4), complex(8, 4)).
?-	complex_plus(complex(3, 4), 5, complex(8, 4)).
?-	complex_plus(complex(1, 2), complex(3, 4), complex(4, 6)).


complex_minus(A, B, Y) :-
	number(A),
	number(B),
	!,
	Y is A - B.

complex_minus(A, B, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	as_complex(B, complex(ReB, ImB)),
	ReY is ReA - ReB,
	ImY is ImA - ImB.

?-	complex_minus(2, 3, -1).
?-	complex_minus(5, complex(3, 4), complex(2, -4)).
?-	complex_minus(complex(3, 4), 5, complex(-2, 4)).
?-	complex_minus(complex(1, 2), complex(3, 4), complex(-2, -2)).


complex_times(A, B, Y) :-
	number(A),
	number(B),
	!,
	Y is A * B.

complex_times(A, B, complex(ReY, ImY)) :-
	as_complex(A, complex(ReA, ImA)),
	as_complex(B, complex(ReB, ImB)),
	ReY is ReA * ReB - ImA * ImB,
	ImY is ReA * ImB + ImA * ReB.

?-	complex_times(2, 3, 6).
?-	complex_times(5, complex(3, 4), complex(15, 20)).
?-	complex_times(complex(3, 4), 5, complex(15, 20)).
?-	complex_times(complex(1, 2), complex(3, 4), complex(-5, 10)).


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
	!,
	Y is A^B.

complex_pow(A, B, Y) :-
	complex_ln(A, LnA),
	complex_times(B, LnA, Exponent),
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

