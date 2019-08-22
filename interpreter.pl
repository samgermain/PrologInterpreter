/***
A skeleton for Assignment 3 on PROP HT2016 at DSV/SU.
Peter Idestam-Almquist, 2016-12-15.
***/

/* If you choose to use the tokenizer, uncomment the following code. */
/***
:- [tokenizer].

run(InputFile,OutputFile):-
	tokenize(InputFile,Program),
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut), 
	output_result(OutputFile,ParseTree,VariablesOut).
***/

/***
Example call of the top level predicate run/2 if you choose to use the tokenizer:
?- run('program1.txt','myparsetree1.txt').
***/

/* If you choose to NOT use the tokenizer, uncomment the following code. */
run(Program,OutputFile):-
	parse(ParseTree,Program,[]),
	evaluate(ParseTree,[],VariablesOut),
	output_result(OutputFile,ParseTree,VariablesOut).
/***
Example call of the top level predicate run/2 if you choose to NOT use the tokenizer:
?- run([a,=,1,*,2,+,'(',3,-,4,')',/,5,;],'myparsetree1.txt').
?- run(['{',a,=,1,*,2,+,'(',3,-,4,')',/,5,;,b,=,4,-,3,-,a,+,6,/,5,/,2,;,c,=,b,+,a,;,'}'],'myparsetree2.txt').
***/

output_result(OutputFile,ParseTree,Variables):- 
	open(OutputFile,write,OutputStream),
	write(OutputStream,'PARSE TREE:'), 
	nl(OutputStream), 
	writeln_term(OutputStream,0,ParseTree),
	nl(OutputStream), 
	write(OutputStream,'EVALUATION:'), 
	nl(OutputStream), 
	write_list(OutputStream,Variables), 
	close(OutputStream).
	
writeln_term(Stream,Tabs,int(X)):-
	write_tabs(Stream,Tabs), 
	writeln(Stream,int(X)).
writeln_term(Stream,Tabs,ident(X)):-
	write_tabs(Stream,Tabs), 
	writeln(Stream,ident(X)).
writeln_term(Stream,Tabs,Term):-
	functor(Term,_Functor,0), !,
	write_tabs(Stream,Tabs),
	writeln(Stream,Term).
writeln_term(Stream,Tabs1,Term):-
	functor(Term,Functor,Arity),
	write_tabs(Stream,Tabs1),
	writeln(Stream,Functor),
	Tabs2 is Tabs1 + 1,
	writeln_args(Stream,Tabs2,Term,1,Arity).
	
writeln_args(Stream,Tabs,Term,N,N):-
	arg(N,Term,Arg),
	writeln_term(Stream,Tabs,Arg).
writeln_args(Stream,Tabs,Term,N1,M):-
	arg(N1,Term,Arg),
	writeln_term(Stream,Tabs,Arg), 
	N2 is N1 + 1,
	writeln_args(Stream,Tabs,Term,N2,M).
	
write_tabs(_,0).
write_tabs(Stream,Num1):-
	write(Stream,'\t'),
	Num2 is Num1 - 1,
	write_tabs(Stream,Num2).

writeln(Stream,Term):-
	write(Stream,Term), 
	nl(Stream).
	
write_list(_Stream,[]). 
write_list(Stream,[Ident = Value|Vars]):-
	write(Stream,Ident),
	write(Stream,' = '),
	format(Stream,'~1f',Value), 
	nl(Stream), 
	write_list(Stream,Vars).


/**
	Evaluates a parse-tree and returns the state of the program
	after evaluation as a list of variables and their values in 
	the form [var = value, ...].
**/
parse(ParseTree) -->
	block(ParseTree).

/* All tokens used within program.*/
assign_op --> ['='].
semicolon --> [';'].
div_op --> ['/'].
mult_op --> ['*'].
add_op --> ['+'].
sub_op --> ['-'].
right_paren --> [')'].
left_paren --> ['('].
right_curly --> ['}'].
left_curly --> ['{'].
ident(ident(Ident)) --> [Ident], {atom(Ident)}. /* True if an identifier */
int(int(Int)) --> [Int], {integer(Int)}.	/* True if an integer */

/* Builds part of the parse tree for Block statements. True when the statements are enclosed with curly brackets, passes everything between curly brackets to the statements clause*/
block(block(left_curly,Statements,right_curly)) -->
    left_curly,
    statements(Statements),
    right_curly.

/*Builds the statements part of the parse tree. True when there are multiple statements in the Program.*/
statements(statements(Assignment,Statements)) --> 
    assignment(Assignment),
    statements(Statements).

/*Builds the statements part of the parse tree. True when there is only one statement left in the Program.*/
statements(statements(Assignment)) --> 
    assignment(Assignment),
	statements(statements).

statements(statements) --> 
	statements.

statements --> [].

/*Builds part of parse tree for assignment statements. True when the statement begins with a variable, followed by an equals sign, an arithmetic expression and ends with a semicolon*/
assignment(assignment(Ident,assign_op,Expression,semicolon)) --> 
	ident(Ident),
	assign_op,
	expression(Expression),
	semicolon.

/*Builds part of parse tree for expression statements, true when the expression is just a term, with no subtraction or division*/
expression(expression(Term)) -->
	term(Term).

/*Builds part of parse tree for expression statements, true if the expression is a term followed by an addition sign, followed by another expression*/
expression(expression(Term,add_op,Expression)) -->
	term(Term),
	add_op,
	expression(Expression).

/*Builds part of parse tree for expression statements, true if the expression is a term followed by an subtraction sign, followed by another expressio*/
expression(expression(Term,sub_op,Expression)) -->
	term(Term),
	sub_op,
	expression(Expression).

/*Builds part of parse tree for term statements, true when the term is just a factor, with no multiplication or division*/
term(term(Factor)) -->
	factor(Factor).

/*Builds part of parse tree for term statements, true when the term is a factor, followed by a multiplication sign, followed by another term*/
term(term(Factor,mult_op,Term)) -->
	factor(Factor),
	mult_op,
	term(Term).

/*Builds part of parse tree for term statements, true when the term is a factor, followed by a division sign, followed by another term*/
term(term(Factor,div_op,Term)) -->
	factor(Factor),
	div_op,
	term(Term).

/*Builds part of parse tree for factor statements, true when the factor is an integer */
factor(factor(Int)) -->
	int(Int).

/*Builds part of parse tree for factor statements, true when the factor is an identifier */
factor(factor(Ident)) -->
	ident(Ident).

/*Builds part of parse tree for factor statements, true when the factor is an expression contained with parenthesis */
factor(factor(left_paren, Expression, right_paren)) --> 
	left_paren,
	expression(Expression),
	right_paren.

/** Combines two lists, into one list**/	
merge([],Xs,Xs).
merge([X|Xs],Ys,[X|Zs]):-
	merge(Xs,Ys,Zs).

/* used to put atoms together into a single atom */
built_equality_structure(Id,Value,Id = Value).

/* Evaluates the statements within parse trees and returns them in the variable VariablesOut */
evaluate(ParseTree,VariablesIn,VariablesOut):-
	evaluator(ParseTree,VariablesIn,VariablesOut).

/* Evaluates Block statements from the parse tree. True when the statements are enclosed with curly brackets. Returns the answers within the variable VariablesOut*/
evaluator(block(left_curly,Statements,right_curly),VariablesIn,VariablesOut):-
	evaluator(Statements,VariablesIn,VariablesOut).

/* Evaluates Statment statements from the parse tree. True when there is only one statement left from the tree. Returns the answers within the variable VariablesOut*/
evaluator(statements(Assignment),VariablesIn,VariablesOut):-
    evaluator(Assignment,VariablesIn,VariablesOut).

/* Evaluates Statment statements from the parse tree. True when there are multiple statements in the tree. Returns the answers within the variable VariablesOut*/
evaluator(statements(Assignment,Statements),VariablesIn,VariablesOut):-
	evaluator(Assignment,VariablesIn,List1),
	merge(VariablesIn,List1,NewVars),
	evaluator(Statements,NewVars,List2),
	merge(List2,List1,VariablesOut).

/* Makes the last statements word printed in the parse tree */
evaluator(statements,VariablesIn,VariablesOut).

/* Evaluates Assignment statements from the parse tree. True when an identifier is followed by a '=', followed by an Expression statement, followed by a semicolon. Returns the answers within the variable Answers5*/
evaluator(assignment(ident(Ident),assign_op,Expression,semicolon),VariablesIn,VariablesOut):-
	evaluator(Expression,VariablesIn,Answer2),	
	built_equality_structure(Ident,Answer2,Answer3),
	VariablesOut = [Answer3].

/* Evaluates Expression statements from the parse tree. True when the expression is a term followed by a '+', followed by another expression. The result is returned in Answer3.*/
evaluator(expression(Term,add_op,Expression),VariablesIn,VariablesOut):-
	evaluator(Term,VariablesIn,Answer1),
	evaluator(Expression,VariablesIn,Answer2),
	VariablesOut is Answer1 + Answer2.

/* Evaluates Expression statements from the parse tree. True when the expression is a term followed by a '-', followed by another expression. The result is returned in Answer3.*/
evaluator(expression(Term,sub_op,Expression),VariablesIn,VariablesOut):-
	evaluator(Term,VariablesIn,Answer1),
	evaluator(Expression,VariablesIn,Answer2),
	VariablesOut is Answer1 - Answer2.

/* Evaluates Expression statements from the parse tree. True when the expression is just a term. The result is returned in Answer3.*/
evaluator(expression(Term),VariablesIn,VariablesOut):-
	evaluator(Term,VariablesIn,VariablesOut).

/* Evaluates Term statements from the parse tree. True when the term is a factor, followed by a '*', followed by another term. The result is returned in Answer3.*/
evaluator(term(Factor,mult_op,Term),VariablesIn,VariablesOut):-
	evaluator(Factor,VariablesIn,Answer1),
	evaluator(Term,VariablesIn,Answer2),
	VariablesOut is Answer1 * Answer2.

/* Evaluates Term statements from the parse tree. True when the term is a factor, followed by a '/', followed by another term. The result is returned in Answer3.*/
evaluator(term(Factor,div_op,Term),VariablesIn,VariablesOut):-
	evaluator(Factor,VariablesIn,Answer1),
	evaluator(Term,VariablesIn,Answer2),
	VariablesOut is Answer1 / Answer2.

/* Evaluates Term statements from the parse tree. True when the term is just a factor. The result is returned in Answer3.*/
evaluator(term(Factor),VariablesIn,VariablesOut):-
	evaluator(Factor,VariablesIn,VariablesOut).

/* Evaluates Factor statements from the parse tree. True when the factor is an integer. The result is returned in Answer3.*/
evaluator(factor(int(Int)),VariablesIn,VariablesOut):-
	VariablesOut is Int.
/*	evaluator(Int,VariablesIn,VariablesOut). */

/* True when the first parameter is an int. Returns result in VariablesOut */
/**evaluator(int(Int),VariablesIn,VariablesOut):-
	VariablesOut is Int.**/

/*Evaluates Factor statements from the parse tree. True when the factor is an identifier. The result is returned in VariablesOut.*/
evaluator(factor(ident(Id)),VariablesIn,VariablesOut) :- member(Id=VariablesOut, VariablesIn).

/*	evaluator(Id,VariablesIn,VariablesOut). */

/*Evaluates Factor statements from the parse tree. True when the factor is an expression contained within parenthesis. The result is returned in Answer3.*/
evaluator(factor(left_paren,Expression,right_paren),VariablesIn,VariablesOut):-
	evaluator(Expression,VariablesIn,VariablesOut).

/* True when the first parameter is an id. Returns result in VariablesOut */
/*evaluator(ident(Id), VariablesIn, VariablesOut) :- member(Id=VariablesOut, VariablesIn).*/
