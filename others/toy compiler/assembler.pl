% assembler.pl
%
% Robert Smyth
%
% This file contains Prolog rules for assembling relocatable code
% into code containing absolute addresses.
% It also contains pretty print routines, and routines designed
% to pump code through the whole "pipeline" (lexical analysis,
% parsing, code generation, assembly and pretty printing).

% Notice that the first argument of assemble may be a list of
% instructions, or a simple instruction.
% Below, N0 refers to the current address, N refers to the 
% address where subsequent code should start.
assemble([Code1|Code2],N0,N) :-
	assemble(Code1,N0,N1),
	assemble(Code2,N1,N).
% For simplicity, each instruction is assumed to occupy only
% one word of memory.
assemble(instr(_,_),N0,N) :-
	N is N0+1.
% When a label is encountered, the previously uninstantiated variable
% contained in the label is unified with the current address in the
% absolute assembly code.  The label itself does not occupy space in
% the final code, so the current address marker is not advanced.
assemble(label(N),N,N).
assemble([],N,N).

% allocate is used to determine how much memory is needed for
% data storage.
% Previously uninstantiated variables at the leaves of the dictionary
% are here set to void (and no memory is allocated).
allocate(void,N,N) :- !.
% Below, N0 refers to the first storage address needed for the
% symbols in dic(*), N refers to the first address **after**
% the storage needed for dic(*), N1 is the address allocated
% for Name.
allocate(dic(Name,N1,Before,After),N0,N) :-
	allocate(Before,N0,N1),
	N2 is N1+1,
	allocate(After,N2,N).

% wl below is adapted from the wl procedure contained in the file
% output-generate-routines.  (It has been modified to work with lists.
% It has also been modified to suppress "instr" from the output.
% And it has been further modified to lookup names corresponding
% addresses.)
wl( [], In_line, Out_line ,_) :- 
	Out_line is In_line, !.
wl( [Next|Rest], In_line, Out_line,D) :-
	wl(Next, In_line, Outline,D), 
	wl(Rest, Outline, Out_line,D), !.
wl( instr(I,Op), In_line, Out_line ,D) :-
	directaddressing(I),
	reverselookup(VarName,D,Op),
	write(In_line), write(':  '),
	write(I), write(' '), write(VarName), write('\n'),
	Out_line is In_line+1.
wl( instr(I,Op), In_line, Out_line ,_) :-
	write(In_line), write(':  '),
	write(I), write(' '), write(Op), write('\n'),
	Out_line is In_line+1.
wl( label(_), In_line, Out_line, _ ) :-
	Out_line is In_line.
wl( block(_), In_line, Out_line, _) :-
	Out_line is In_line.

% To replace a simple block pseudo-op which gives the quantity
% of data storage required, wdata may be used to print the
% symbols corresponding to each data storage address.
wdata( CurrentLine, LastLine, _ ) :-
	CurrentLine>LastLine, !.
wdata( CurrentLine, LastLine, D ) :-
	reverselookup(Name,D,CurrentLine),
	write(CurrentLine), 
	write(':  Storage for '),
	write(Name),
	write('\n'),
	NextLine is CurrentLine+1,
	wdata( NextLine, LastLine, D ).
	
% reverselookup can be used to determine the symbolic name
% corresponding to a given address.
% Since the dictionary is ordered by name and not address
% this reverselookup is slow... but it works.
reverselookup(_,void,_) :- !,fail.
reverselookup(Name, dic(Name, Line,_,_), Line) :- !.
reverselookup(Name, dic(_,_,Before,_), Line) :-
	reverselookup(Name, Before, Line).
reverselookup(Name, dic(_,_,_,After), Line) :-
	reverselookup(Name, After, Line).

directaddressing(load).
directaddressing(store).

%%%%%%%%%%%%%%%%%%%%% do it all! %%%%%%%%%%%%%%%%%%%%%%%%%

% With GNU Prolog, FileName must be file that does not
% contain newlines.

compile(FileName) :-
	open(FileName, 'read', Rstream) ,
	read(Rstream,Z1) ,
	close(Rstream) ,
	phrase(tokens(Z0),Z1) ,
	program(Z0,_,AST) ,
	encodestatement(AST, D, Code, []),
%	write(Code),                                     % For debugging
%	write('\nAfter encode, before assemble...\n'),   % purposes only.
	assemble(Code,1,N0),
%	write('\nAfter assemble...\n'),                  % For debugging
%	write(Code),                                     % only.
	N1 is N0+1,
%	write('\nBefore allocate...\n'),                 % Debugging only.
	allocate(D,N1,N),
%	write('\nAfter allocate...\n'),                  % Debugging only.
	L is N-N1,
	append(Code,[instr(halt,0),block(L)],AbsCode),
	wl(AbsCode,1,DataStartLine,D),
	LastLine is N-1,
	wdata(DataStartLine, LastLine, D).

% Z1 should be a source program in the form of a string.
testPgm(Z1) :-
	phrase(tokens(Z0),Z1) ,
	program(Z0,_,AST) ,
	encodestatement(AST, D, Code, []),
	assemble(Code,1,N0),
	N1 is N0+1,
	allocate(D,N1,N),
	L is N-N1,
	append(Code,[instr(halt,0),block(L)],AbsCode),
	wl(AbsCode,1,DataStartLine,D),
	LastLine is N-1,
	wdata(DataStartLine, LastLine, D).
