rewrite_expression(X, X) :-
	var(X),
	!.

rewrite_expression(declare([], Expression), declare([], RewrittenExpression)) :-
	!,
	rewrite_expression(Expression, RewrittenExpression).

rewrite_expression(declare([function(Variable, Label, Functions) | Tail], Expression), declare([function(Variable, Label, Functions) | Tail], Expression)) :-
	!.

rewrite_expression(declare([substitution(Variable, _,  Value) | Tail], Expression), RewrittenExpression) :-
	!,
	var(Variable),
	copy_variable(Variable, [Tail, Expression], CopiedVariable, [CopiedTail, CopiedExpression]),
	CopiedVariable = Value,
	rewrite_expression(declare(CopiedTail, CopiedExpression), RewrittenExpression).

rewrite_expression(declare([Declaration | Tail], Expression), declare([Declaration | RewrittenTail], RewrittenExpression)) :-
	!,
	rewrite_expression(declare(Tail, Expression), declare(RewrittenTail, RewrittenExpression)).

rewrite_expression(derivative(X, F), Derivative) :-
	!,
	expression_derivative_multiorder(X, F, Derivative).

rewrite_expression(equal_transform(Transformation, ExpressionList), equal(Derivative)) :-
	is_list(ExpressionList),
	!,
	transform_expression_list(Transformation, ExpressionList, Derivative).

rewrite_expression(Expression, RewrittenExpression) :-
	Expression =.. [Functor | Args],
	rewrite_expression_list(Args, RewrittenArgs),
	RewrittenExpression =.. [Functor | RewrittenArgs].


rewrite_expression_list([], []).

rewrite_expression_list([Expression | ExpressionTail], [RewrittenExpression | RewrittenTail]) :-
	rewrite_expression(Expression, RewrittenExpression),
	rewrite_expression_list(ExpressionTail, RewrittenTail).


transform_expression_list([], Expression, Expression).

transform_expression_list([Transformation | TransformationTail], Expression, TransformedExpression) :-
	single_transform_expression_list(Transformation, Expression, HeadTransformedExpression),
	transform_expression_list(TransformationTail, HeadTransformedExpression, TransformedExpression).


single_transform_expression_list(_, [], []).

single_transform_expression_list(Transformation, [Expression | ExpressionTail], TransformedExpressionTail) :-
	hint(Expression),
	!,
	single_transform_expression_list(Transformation, ExpressionTail, TransformedExpressionTail).

single_transform_expression_list(Transformation, [Expression | ExpressionTail], [TransformedExpression | TransformedExpressionTail]) :-
	single_transform_expression(Transformation, Expression, TransformedExpression),
	single_transform_expression_list(Transformation, ExpressionTail, TransformedExpressionTail).


single_transform_expression(derivative(X), Expression, Derivative) :-
	rewrite_expression(derivative(X, Expression), Derivative).


rewrite_expression_or_exception(Orig, Rewritten) :-
	rewrite_expression(Orig, Rewritten),
	!.


rewrite_expression_or_exception(_, _) :-
	throw("Expression cannot be rewritten").

