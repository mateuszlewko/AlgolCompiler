% lexer.pl
%
% The rules in this file implement the lexical analysis phase of compilation,
% by converting the source file character sequence into a Prolog list
% consisting of tokens for keywords, operators and identifiers.
% Only **single**-character identifiers are supported in this
% implementation.
% Notice that each identifier (and each digit) gets its own token, 
% so there's no need for the lexer to return "values" associated
% with any of the tokens.

% Return a unique token for each control structure keyword in the form of
% a Prolog atom whose name literally matches the keyword.
tokens(Z) --> "while", tokens(Y), {Z = [while | Y]}.
tokens(Z) --> "do", tokens(Y), {Z = [do | Y]}.
tokens(Z) --> "endwhile", tokens(Y), {Z = [endwhile | Y]}.
tokens(Z) --> "repeat", tokens(Y), {Z = [repeat | Y]}.
tokens(Z) --> "until", tokens(Y), {Z = [until | Y]}.
tokens(Z) --> "endrepeat", tokens(Y), {Z = [endrepeat | Y]}.
tokens(Z) --> "if", tokens(Y), {Z = [if | Y]}.
tokens(Z) --> "then", tokens(Y), {Z = [then | Y]}.
tokens(Z) --> "else", tokens(Y), {Z = [else | Y]}.
tokens(Z) --> "endif", tokens(Y), {Z = [endif | Y]}.
tokens(Z) --> "exit", tokens(Y), {Z = [exit | Y]}.
tokens(Z) --> "other", tokens(Y), {Z = [other | Y]}.

% Comparison operators.
tokens(Z) --> "==", tokens(Y), {Z = [== | Y]}.
tokens(Z) --> "<>", tokens(Y), {Z = [<> | Y]}.

% Assignment operator.
tokens(Z) --> ":=", tokens(Y), {Z = [:= | Y]}.  

% Boolean constants and operators.
tokens(Z) --> "true", tokens(Y), {Z = [true | Y]}.  
tokens(Z) --> "false", tokens(Y), {Z = [false | Y]}.  
tokens(Z) --> "and", tokens(Y), {Z = [and | Y]}.  
tokens(Z) --> "or", tokens(Y), {Z = [or | Y]}.  

% Strip spaces, tabs and newlines.
tokens(Z) --> " ", tokens(Y), {Z = Y}.
tokens(Z) --> "	", tokens(Y), {Z = Y}.
% GNU Prolog does not seem to like the newline stripper below. --rws
%tokens(Z) --> "
%", tokens(Y), {Z = Y}.

% Anything not mentioned above gets its own token,
% including single-character identifiers.
tokens(Z) --> [C], tokens(Y), {name(X, [C]), Z = [X | Y]}.
tokens(Z) --> [], {Z = []}.

% If you now invoke the goal
% 
%        phrase(tokens(Z),"while a == b do other ; other endwhile")
% 
% then this goal will succeed and Z will be instantiated to 
%    
%       [while,a,==,b,do,other,;,other,endwhile].
