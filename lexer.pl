% Pytania
%% 1) Jak wyciągnać liczby / identyfikatory? (korzystając z predykatów number / identifier)

% Słowa kluczowe
tokens(Z) --> "and", !, tokens(Y), {Z = [and | Y]}.
tokens(Z) --> "begin", !, tokens(Y), {Z = [begin | Y]}.
tokens(Z) --> "call", !, tokens(Y), {Z = [call | Y]}.
tokens(Z) --> "div", !, tokens(Y), {Z = [div | Y]}.
tokens(Z) --> "do", !, tokens(Y), {Z = [do | Y]}.
tokens(Z) --> "done", !, tokens(Y), {Z = [done | Y]}.
tokens(Z) --> "else", !, tokens(Y), {Z = [else | Y]}.
tokens(Z) --> "end", !, tokens(Y), {Z = [end | Y]}.
tokens(Z) --> "fi", !, tokens(Y), {Z = [fi | Y]}.
tokens(Z) --> "if", !, tokens(Y), {Z = [if | Y]}.
tokens(Z) --> "local", !, tokens(Y), {Z = [local | Y]}.
tokens(Z) --> "mod", !, tokens(Y), {Z = [mod | Y]}.
tokens(Z) --> "not", !, tokens(Y), {Z = [not | Y]}.
tokens(Z) --> "or", !, tokens(Y), {Z = [or | Y]}.
tokens(Z) --> "procedure", !, tokens(Y), {Z = [procedure | Y]}.
tokens(Z) --> "program", !, tokens(Y), {Z = [program | Y]}.
tokens(Z) --> "read", !, tokens(Y), {Z = [read | Y]}.
tokens(Z) --> "return", !, tokens(Y), {Z = [return | Y]}.
tokens(Z) --> "then", !, tokens(Y), {Z = [then | Y]}.
tokens(Z) --> "value", !, tokens(Y), {Z = [value | Y]}.
tokens(Z) --> "while", !, tokens(Y), {Z = [while | Y]}.
tokens(Z) --> "write", !, tokens(Y), {Z = [write | Y]}.

% Operatory
tokens(Z) --> "+", !, tokens(Y), {Z = [+ | Y]}.
tokens(Z) --> "-", !, tokens(Y), {Z = [- | Y]}.
tokens(Z) --> "<", !, tokens(Y), {Z = [< | Y]}.
tokens(Z) --> ">", !, tokens(Y), {Z = [> | Y]}.
tokens(Z) --> "<=", !, tokens(Y), {Z = [<= | Y]}.
tokens(Z) --> ">=", !, tokens(Y), {Z = [>= | Y]}.
tokens(Z) --> "=", !, tokens(Y), {Z = [= | Y]}.

tokens(Z) --> "<>", !, tokens(Y), {Z = [<> | Y]}.
tokens(Z) --> ":=", !, tokens(Y), {Z = [:= | Y]}. 

tokens(Z) --> " ", !, tokens(Y), {Z = Y}.

% Symbole przystankowe
tokens(Z) --> ",", !, tokens(Y), {Z = [+ | Y]}.
tokens(Z) --> "(", !, tokens(Y), {Z = [- | Y]}.
tokens(Z) --> ")", !, tokens(Y), {Z = [< | Y]}.

% Identyfikatory
tokens(Z) --> digit(D), !, number(D, N) !, tokens(Y), {Z = [N | Y]}.
%% tokens(Z) --> digits(Ds), !, tokens(Y), {Z }.
%% tokens(Z) --> identifier(Z, Id), !, tokens(Y), {Z = [Id | Y]}.

white_space -->
   [Char], { code_type(Char, space) }, !, white_space.
white_space -->
   [].
   
digit(D) -->
   [D],
      { code_type(D, digit) }.

digits([D|T]) -->
   digit(D),
   !,
   digits(T).
digits([]) -->
   [].

number(D, N) -->
   digits(Ds),
      { number_chars(N, [D|Ds]) }.

letter(L) -->
   [L], { code_type(L, alpha) }.

alphanum([A|T]) -->
   [A], { code_type(A, alnum) }, !, alphanum(T).
alphanum([]) -->
   [].

identifier(L, Id) -->
   alphanum(As), { atom_codes(Id, [L|As]) }.


% Anything not mentioned above gets its own token,
% including single-character identifiers.
tokens(Z) --> [C], tokens(Y), {name(X, [C]), Z = [X | Y]}.
tokens(Z) --> [], {Z = []}.