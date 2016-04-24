% parser.pl
%
% Robert Smyth
%
% This file contains Prolog rules designed to parse the token stream
% produced by lexical analysis into intermediate code in the form of
% a list of abstract syntax trees.
% (AST's are here realized as Prolog terms.
% Each node of an AST is represented as a Prolog term whose arguments
% represent the node's children.)
% The rules below are typically of the form
%          programcomponent(Z0,Z,X) :- subgoals.
% and have (roughly speaking) the following interpretation:
% If Z0 is the portion of the token stream yet to be recognized, and
% the subgoals are satisfied, then programcomponent has been 
% recognized and captured in the AST list X, and Z is the portion of
% the token stream after programcomponent.

% The whole program is simply a sequence of statements.
program(Z0,Z,X) :-
	statements(Z0,Z,X).

% reststatements was contrived to eliminate left recursion.
% X0 (the AST list for statement) is "passed down" to reststatements and
% (eventually) returned as a prefix of X.  (See the append subgoal
% in the next rule.)
statements(Z0,Z,X):-
	statement(Z0,Z1,X0) ,
	reststatements(Z1,Z,X0,X).

% Notice that ';' serves as a statement separater.
reststatements([';'|Z0],Z,X0,Y) :-
	statements(Z0,Z,X),
	append(X0,X,Y).
reststatements(Z,Z,X,X).

% The statement "procedure" has multiple entry points corresponding
% to the various statement types (asssignment, if-else, while, repeat
% exit and other).
% Repeat-until loops are unrolled into while loops.  This has the
% advantage of simplicity.  In particular, the code generation 
% phase will not need to handle two different loop types. 
% But it has the disadvantage that exit statements within repeat-until
% loops can not be handled properly.  ( repeat S until T endrepeat
% becomes indistinguishable from S;while T do S endwhile ...
% so that some nesting depth information is lost.)
statement([V,:=,BoolVal|Z],Z,[assign(name(V),BoolVal)]):-
	atom(V) ,
	boolval(BoolVal).
statement([if|Z0],Z,[if(Test,Then,Else)]):-               
	compoundboolexpr(Z0,[then|Z1],Test) ,
	statements(Z1,[else|Z2],Then) ,
	statements(Z2,[endif|Z],Else).
statement([while|Z0],Z,[while(Test,Do)]):-                 
	compoundboolexpr(Z0,[do|Z1],Test) ,
	statements(Z1,[endwhile|Z],Do).
statement([repeat|Z0],Z,Repeat):-
	statements(Z0,[until|Z1],Do),
	compoundboolexpr(Z1,[endrepeat|Z],Test),
	append(Do,[while(Test,Do)],Repeat).
statement([exit,'(',NumLevels,')'|Z],Z,[exit(NumLevels)]).
statement([other|Z],Z,[other]).

% Since we're parsing in a top-down fashion we need to recognize
% operations of low precedence before those of high precedence.
% 2 marks our lowest precedence level.
compoundboolexpr(Z0,Z,X) :-
	boolsubexpr(2,Z0,Z,X).

boolsubexpr(N,Z0,Z,X) :-
	N>0, N1 is N-1,
	boolsubexpr(N1,Z0,Z1,X0),
	restboolexpr(N,Z1,Z,X0,X).
boolsubexpr(0,[Digit1,Op,Digit2|Z],Z,comparison(Op,Digit1,Digit2)) :-
	digit(Digit1),
	comparisonop(Op),
	digit(Digit2).
boolsubexpr(0,[X|Z],Z,name(X)) :-
	atom(X).

restboolexpr(N,[Op|Z0],Z,X1,X) :-
	logicop(N,Op), N1 is N-1,
	boolsubexpr(N1,Z0,Z1,X2),
	restboolexpr(N,Z1,Z,logicexpr(Op,X1,X2),X).
restboolexpr(_,Z,Z,X,X).

boolval(true).  boolval(false).

digit(0).  digit(1).  digit(2).  digit(3).  digit(4).
digit(5).  digit(6).  digit(7).  digit(8).  digit(9).

comparisonop(==).  comparisonop(<>).

logicop(1,and).  logicop(2,or).

% program("tokenized source code in a Prolog list",_,X).
% can be invoked directly.  This has the small advantage of bypassing
% the single-character identifier limitation of the lexer.
% On return, X contains the desired intermediate code.
% For debugging purposes, the "don't care" should be replaced
% with a variable... to make it easy to check whether the entire
% input has been parsed.
%
% The rule below may be used to pass a source code file through
% both the lexical analysis and parsing phases.  The file
% lexer.pl must be consulted first.
% With GNU Prolog, the source code file must *not* contain any
% newlines.
parser(FileName) :-
	open(FileName, 'read', Rstream) ,
	read(Rstream,Z1) ,
	close(Rstream).
	phrase(tokens(Z0),Z1) ,
	program(Z0,_,X) ,
	write(X).
