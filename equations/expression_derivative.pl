function_derivative(F, []) :-
	number(F).

function_derivative(imag, []).
function_derivative(e, []).
function_derivative(pi, []).
function_derivative(A + B, [[A, 1], [B, 1]]).
function_derivative(A - B, [[A, 1], [B, -1]]).
function_derivative(A * B, [[A, B], [B, A]]).
function_derivative(A / B, [[A, 1 / B], [B, -A / B^2]]).
function_derivative(A^B, [[A, B * A^(B - 1)], [B, log(e, A) * e^(B * log(e, A))]]).
function_derivative(sin(X), [[X, cos(X)]]).
function_derivative(cos(X), [[X, -sin(X)]]).

expression_derivative(X, Expression, 1) :-
	var(Expression),
	X == Expression,
	!.

expression_derivative(X, Expression, 0) :-
	var(Expression),
	X \== Expression,
	!.

expression_derivative(X, integral(X, Expression), Expression) :-
	!.

expression_derivative(_, [], []) :-
	!.

expression_derivative(X, [Expression | ExpressionTail], [Derivative | DerivativeTail]) :-
	!,
	expression_derivative(X, Expression, Derivative),
	expression_derivative(X, ExpressionTail, DerivativeTail).

expression_derivative(X, real_part(Expression), real_part(Derivative)) :-
	!,
	expression_derivative(X, Expression, Derivative).

expression_derivative(X, imag_part(Expression), imag_part(Derivative)) :-
	!,
	expression_derivative(X, Expression, Derivative).

expression_derivative(X, apply(Function, Args, Values), Derivative) :-
	!,
	copy_term([Function, Args], [CopiedFunction, CopiedArgs]),
	CopiedArgs = Values,
	expression_derivative(X, CopiedFunction, Derivative).

expression_derivative(X, Expression, Derivative) :-
	function_derivative(Expression, FunctionDerivative),
	compound_derivative(X, FunctionDerivative, Derivative).


compound_derivative(_, [], 0).

compound_derivative(X, [[SubExpression, Der] | Tail], Result) :-
	expression_derivative(X, SubExpression, SubDerivative),
	compound_derivative(X, Tail, TailDerivative),
	symbolic_multiply(Der, SubDerivative, CompoundDerivative),
	symbolic_add(CompoundDerivative, TailDerivative, Result).

?- 	expression_derivative(X, (2*X + 1)^3, 3 * (2*X + 1)^(3 - 1) * 2).
?-	F = Z^2, expression_derivative(X, apply(F, [Z], [3*X+4]), 2 * (3*X + 4)^(2-1) * 3).


expression_derivative_multiorder(X, Expression, Derivative) :-
	var(X),
	!,
	expression_derivative(X, Expression, Derivative).

expression_derivative_multiorder(A * B, Expression, Derivative) :-
	!,
	expression_derivative_multiorder(A, Expression, DerivativeA),
	expression_derivative_multiorder(B, DerivativeA, Derivative).

expression_derivative_multiorder(X^1, Expression, Derivative) :-
	var(X),
	!,
	expression_derivative(X, Expression, Derivative).

expression_derivative_multiorder(X^N, Expression, Derivative) :-
	var(X),
	N > 1,
	!,
	M is N - 1,
	expression_derivative(X, Expression, SubDerivative),
	expression_derivative_multiorder(X^M, SubDerivative, Derivative).


