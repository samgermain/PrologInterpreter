/**
block -->
	['{'],
	stmts,
	['}'].

stmts --> 
	assign,
	stmts.

stmts --> 
	.
**/

assignment(assignment(Ident,Assign_Op,Expr,Semicolon)) --> 
	ident(Ident),
	assign_op(Assign_Op),
	expr(Expr),
	semicolon(Semicolon).

ident(ident(Ident)) --> [Ident], {atom(Ident)}.
inte(inte(Inte)) --> [Inte], {integer(Inte)}.
assign_op(assign_op([=])) --> [=].
semicolon(semicolon(;)) --> [;].
div_op(div_op(['/'])) --> ['/'].
mult_op(mult_op([*])) --> [*].
add_op(add_op([+])) --> [+].
sub_op(sub_op([-])) --> [-].
right_paren(right_paren([')'])) --> [')'].
left_paren(left_paren(['('])) --> ['('].
right_curly(right_curly(['}'])) --> ['}'].
left_curly(left_curly(['{'])) --> ['{'].

expr(expr(Term)) -->
	term(Term).

expr(expr(Term,Add_Op,Expr)) -->
	term(Term),
	add_op(Add_Op),
	expr(Expr).

expr(expr(Term,Sub_Op,Expr)) -->
	term(Term),
	sub_op(Sub_Op),
	expr(Expr).

term(term(Factor)) -->
	factor(Factor).

term(term(Factor,Mult_Op,Term)) -->
	factor(Factor),
	mult_op(Mult_Op),
	term(Term).

term(term(Factor,Div_Op,Term)) -->
	factor(Factor),
	div_op(Div_Op),
	term(Term).

factor(factor(Inte)) -->
	inte(Inte).

factor(factor(Ident)) -->
	ident(Ident).

factor(factor(Left_Paren, Expr, Right_Paren)) --> 
	left_paren(Left_Paren),
	expr(Expr),
	right_paren(Right_Paren).

