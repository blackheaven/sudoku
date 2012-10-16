zone(L, C, Z) :- Z is (1 + floor((C - 1)/3) + 3 * floor((L - 1)/3)).

%% i(): true if the 2nd parameter contains the list of all 
%% integers contained in the string (passed as ASCII list) 
%% passed as 1st parameter
% example usage: i("1 2 3", [49, 50, 51]).
% another example usage: i("1 _ 3", [49, 51]).
digit_code(J) :- J >= 0'0, J =< 0'9.
i([], []).
i([], _) :- fail.
i([A|S], [B|R]) :- (digit_code(A), B=A; (A=0'_, B is 48)),!, i(S, R). % if the char is not a number but is a "_", we evaluate it to 0 (48)
i([_|S], R) :- i(S, R).

%% transf(): true if the 2nd parameter is the list of integers of the 1st parameter subtracted 48
%% used to transform a list of digits ASCII codes into a list of those digits...
% example usage: transf([0,0], [-48, -48]).
% another example usage: transf([48, 48], L). L=[0,0]
transf([], []).
transf([A|S], [B|R]) :- B is A-48, transf(S, R).

% example usage: super_i("1 _ 3", [1, 3]).
% another example usage: i("1 2 3", [1, 2, 3]).
super_i(Sentence, R) :- i(Sentence, A), transf(A, R).

grid([], _).
% grid(L, R).

read_lines(T) :- 
  open('./grille.txt', read, F),!, read_lines(T, F), close(F).

read_lines(T, F) :- 
  read_line_to_codes(F, Temp), 
  (Temp=end_of_file, T = [] ; 
        read_lines(T2, F),
        super_i(Temp, H), 
        append(H, T2, T)
  )
  
  % to "return" an empty value in the case we reach the end of file, instead of having it un-instantiated
  .

transf2([], _).
transf2(G, R) :- 
  transf2(G, 1, R)
.
transf2([], _, []).

transf2([0|G], N, R) :- 
  N2 is N+1,
  transf2(G, N2, R)
.

transf2([A|G], N, [v(p(L,C,Z), A)|R]) :- 
  C is mod(N, 9), % col does not go after 9
  L is floor(N/9)+1, % line is floor(col/9)+1
  zone(L, C, Z),
  N2 is N+1,
  transf2(G, N2, R)
.

%% Somewhere remplis(1, 1, Grille), transf2(Cases, Grille, Remplies, Restantes) %%, complete(Remplies, Couleurs, RestantesBrute)
% transf2([], _, [], []).
% transf2([0|CS], [G|GS], D, [G|TS]) :-
%       transf2(CS, GS, D, TS).
% transf2([C|CS], [G|GS], [v(G, C)|D], TS) :-
%       transf2(CS, GS, D, TS).


test(A) :-
        %trace,
        read_lines(L),
        !,
        transf2(L, A), !.
% @TODO: find a way to get the line as an ASCII list (with read() ? read_lines_to_codes() ? ) and then use super_i()
% @TODO: Then, use the same way remplis() does to generate a grid (zone(X, Y, X), v(p(X, Y, Z), Value))
