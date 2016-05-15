% encoder.p
%
% Robert Smyth
%
% This file contains Prolog rules designed to translate intermediate
% code into relocatable "von Neumann type" assembly language.

% encodestatement is the top level routine for this phase of compilation.  
% Beware of the following subtlety.  The first argument of encodestatement
% may be a list of ASTs or a simple AST.  The first two entry points
% correspond to the former possibility.  The remaining entry points
% correspond to the latter case.
% In the latter case, encodestatement(S,D,Code,ELE) :- subgoals.
% means (roughly speaking), that if the subgoals are satisfied,
% then S is a Prolog term encapsulating an AST which translates
% to the list of assembly instructions in Code using dictionary D,
% and the labels of the statements immediately following the
% enclosing loops are listed in ELE (from innermost to outermost).
encodestatement([], _, [], _).
encodestatement([S1|S2], D, Code, EnclosingLoopEnds) :-
	encodestatement(S1, D, Code1, EnclosingLoopEnds),
	encodestatement(S2, D, Code2, EnclosingLoopEnds),
	append(Code1, Code2, Code).
encodestatement(assign(name(X),Value), D,
  [instr(loadc,V), instr(store,Addr)], _ ) :-
	lookup(X,D,Addr),
	logic_num(Value,V).
encodestatement(other,_,[instr(nop,0)], _).
encodestatement(if(Test,Then,Else), D, Code, EnclosingLoopEnds) :-
	encodeboolexpr(Test,D,TrueLabel,FalseLabel,Testcode),
	encodestatement(Then,D,Thencode, EnclosingLoopEnds),
	encodestatement(Else,D,Elsecode, EnclosingLoopEnds),
	append(Testcode, [label(TrueLabel)|Thencode], C1),
	append(C1, [instr(jump,AfterIf),label(FalseLabel)|Elsecode], C2),
	append(C2, [label(AfterIf)], Code).
encodestatement(while(Test,Do), D, Code, EnclosingLoopEnds) :-
	encodeboolexpr(Test,D,TrueLabel,FalseLabel,Testcode),
	encodestatement(Do,D,Docode, [FalseLabel|EnclosingLoopEnds]),
	append([label(Loopstart)|Testcode], [label(TrueLabel)|Docode], C1),
	append(C1, [instr(jump,Loopstart),label(FalseLabel)], Code).
encodestatement(exit(K), _, [instr(jump, Label)], EnclosingLoopEnds) :-
	nth(K, EnclosingLoopEnds, Label).
% nth(K, ELE, L) succeeds if the element in position K of list ELE 
% can be unified with L.
% exit(k) should only be used in the source program with 0<k<10
% (k<10 is a restriction imposed by our simple approach to
% lexical analysis.)
% Furthermore, the k innermost loops enclosing exit(k) should all
% be honest-to-goodness while loops.  An enclosing repeat-until
% loop is only acceptable if it is more than k layers "up".
% Note, however, that exit(k) need not be restricted to conditionals.

% The following rules perform efficient translation of Boolean expressions
% in the sense that the second operand of a Boolean operator is evaluated
% only when necessary.  However, the translations are still typically far
% from optimal.  For example, jumps to the next line are commonly generated.
% encodeboolexpr(T,D,TL,FL,C) :- subgoals.
% means (roughly) that, if the subgoals are satisfied, then
% C is a list of assembly instructions for the expression represented by tree T
% (consistent with the symbol table encoded in dictionary D),
% TL is the label of the statement to which control should jump if the
% expression evaluates to true, and
% FL is the label of the statement to which control should jump if the
% expression evaluates to false.
encodeboolexpr(logicexpr(Op,X1,X2), D, TrueLabel, FalseLabel, Code) :-
	encodeboolexpr(X1, D, TL, FL, Code1),
	encodeboolexpr(X2, D, TrueLabel, FalseLabel, Code2),
	shortcircuit(Op, TrueLabel, FalseLabel, TL, FL, BeginArg2),
	append(Code1, [label(BeginArg2)|Code2], Code).
encodeboolexpr(comparison(Op,X1,X2), _, TrueLabel, FalseLabel, 
  [instr(loadc,X1),instr(subc,X2),instr(JumpIf,FalseLabel),
   instr(jump,TrueLabel)] ) :-
	unlessop(Op, JumpIf).
encodeboolexpr(name(BoolVar), D, TrueLabel, FalseLabel,
  [instr(load,Addr),instr(jumpeq,FalseLabel),instr(jump,TrueLabel)]) :-
	lookup(BoolVar,D,Addr).
	
shortcircuit(and, _, FalseLabel, TL, FL, BeginArg2) :-
	FL=FalseLabel, TL=BeginArg2.
shortcircuit(or, TrueLabel, _, TL, FL, BeginArg2) :-
	TL=TrueLabel, FL=BeginArg2.

unlessop(==, jumpne).  unlessop(<>, jumpeq).

% lookup is used **to build** the symbol table during code generation, i.e. the
% second parameter is used as the output during code generation.
% The symbol table so built has uninstantiated variables in place of addresses
% upon completion of code generation.
% The builtin predicate atom_codes is used here since GNU Prolog raises
% an exception upon encountering a comparison of atoms.
lookup(Name, dic(Name,Value,_,_),Value) :- !.
lookup(Name, dic(Name1,_,Before,_),Value) :-
	Name@<Name1, 
	lookup(Name,Before,Value).
lookup(Name, dic(Name1,_,_,After),Value) :-
	Name@>Name1,
	lookup(Name,After,Value).

% Implement the C convention for representing Boolean values numerically.
logic_num(true,1).  logic_num(false,0).

% encodestatement("List of ASTs encoded as Prolog terms", _, RelocatableCode, []).
% may be invoked directly,
% or the following rule may be used to pass a source file through the lexical 
% analysis, parsing and code generation phases.
encode(FileName) :-
	open(FileName, 'read', Rstream) ,
	read(Rstream,Z1) ,
	close(Rstream) ,
	phrase(tokens(Z0),Z1) ,
	program(Z0,_,AST) ,
	encodestatement(AST, _, RelocatableCode, []),
	write(RelocatableCode).
