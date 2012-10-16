test(G, S) :-
    sudoku_solve(G, [1, 2, 3, 4, 5, 6, 7, 8, 9], S)
    .

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prettyprint prints to the output in an elegant way %%

prettyprint(_, []). % End of the print
prettyprint(F, [v(p(_, 9, _), V)|N]) :- write(F, V), nl(F), prettyprint(F, N).
prettyprint(F, [v(_, V)|N]):- write(F, V), write(F, ' '), prettyprint(F, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sudoku_solve solves the sudoku %%

sudoku_solve(Entrees, Couleurs, Solutions) :-
    % init
    remplis(1, 1, CasesBrute),
    predsort(sort_cases, CasesBrute, Cases),
    transf2(Entrees, Cases, Remplies, Manquantes),
    pondere(Manquantes, Remplies, Couleurs, RestantesBrutes),
    !,
    predsort(sort_cases, RestantesBrutes, Restantes),
    % Solving
    !,
    resout(Restantes, Remplies, Solutions),
    !.

% transf2(Cases, Grille, Remplies, Restantes)
transf2([], _, [], []).
transf2([0|CS], [G|GS], D, [G|TS]) :-
      transf2(CS, GS, D, TS).
transf2([C|CS], [G|GS], [v(G, C)|D], TS) :-
      transf2(CS, GS, D, TS).

remplis(9, 10, []).
remplis(L, 10, R) :-
    N is L + 1,
    remplis(N, 1, R).
remplis(L, C, [p(L, C, Z)|S]) :-
    zone(L, C, Z),
    N is C + 1,
    remplis(L, N, S),
    !.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zone(L, C, Z) :- Z is (1 + floor((C - 1)/3) + 3 * floor((L - 1)/3)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pondere([], _, _, []).
pondere([M|MS], Remplies, Couleurs, [v(M, P)|RS]) :-
    voisins(M, Remplies, Vb),
    extrait_valeurs(Vb, Vv),
    list_to_set(Vv, Vs),
    subtract(Couleurs, Vs, P),
    length(P, NB),
    NB > 0,
    pondere(MS, Remplies, Couleurs, RS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

voisins(_, [], []).
voisins(M, [v(R, V)|RS], [v(R, V)|VS]) :-
    voisin(M, R),
    voisins(M, RS, VS).
voisins(M, [R|RS], VS) :-
    not(voisin(M, R)),
    voisins(M, RS, VS).

voisin(p(L, _, _), p(L, _, _)).
voisin(p(_, C, _), p(_, C, _)).
voisin(p(_, _, Z), p(_, _, Z)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

extrait_valeurs([], []).
extrait_valeurs([v(_, V)|VS], [V|RS]) :- extrait_valeurs(VS, RS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

resout([], R, R).
resout([v(P, [V])|MS], Remplies, Solutions) :-
    N = [v(P, V)|Remplies],
    repondere(V, P, MS, NMS),
    resout(NMS, N, Solutions).

resout([M|MS], Remplies, Solutions) :-
    NT = [M|MS],
    predsort(sort_cases, NT, N),
    [v(P, F)|NS] = N,
    (not(F = [_]) ->
        (member(C, F),
         repondere(C, P, NS, NMS),
         resout(NMS, [v(P, C)|Remplies], Solutions)) ;
         resout(N, Remplies, Solutions)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

repondere(_, _, [], []).
repondere(V, Pa, [v(M, P)|MS], [v(M, R)|RS]) :-
    voisin(Pa, M),
    subtract(P, [V], R),
    length(R, NB),
    NB > 0,
    repondere(V, Pa, MS, RS).
repondere(V, Pa, [v(M, P)|MS], [v(M, P)|RS]) :-
    not(voisin(Pa, M)),
    repondere(V, Pa, MS, RS).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Comparison routines, used by the predsort() predicate %%

sort_cases(>, v(_, X), v(_, Y)) :-
    length(X, Xn),
    length(Y, Yn),
    Xn >= Yn.
sort_cases(<, _, _).

sort_res(>, v(p(L1, C1, _), _), v(p(L2, C2, _), _)) :-
    P1 is L1 * 10 + C1,
    P2 is L2 * 10 + C2,
    P1 >= P2.
sort_res(<, _, _).

% integer(I) -->
%         digit(D0),
%         digits(D),
%         { number_codes(I, [D0|D])
%         }.

% digits([D|T]) -->
%         digit(D), !,
%         digits(T).
% digits([]) -->
%         [].

% digit(D) -->
%         [D],
%         { code_type(D, digit)
%         }.

% ints([], _, _).
% ints(Sentence, Result, R) :- phrase(integer(X), Sentence, Rest), append([X], Result, R), ints(Rest, R, R2), R = R2.

% t([A|_]) :- A is 0'a.

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


read_lines(F, T) :- 
  read_line_to_codes(F, Temp), 
  (Temp=end_of_file, T = [] ; 
        read_lines(F, T2),
        super_i(Temp, H), 
        append(H, T2, T)
  )
  % to "return" an empty value in the case we reach the end of file, instead of having it un-instantiated
  .


% @TODO: find a way to get the line as an ASCII list (with read() ? read_lines_to_codes() ? ) and then use super_i()
% @TODO: Then, use the same way remplis() does to generate a grid (zone(X, Y, X), v(p(X, Y, Z), Value))

process(I, O) :-
    open(I, read, F),
    read_lines(F, L),
    close(F),
    test(L, R),
    open(O, write, S),
    predsort(sort_res, R, T),
    prettyprint(S, T),
    nl(S),
    close(S), !.

solve(0).
solve(N) :-
        %trace,
        number_codes(N, Codes),
        append("solutions/", Codes, Sol_),
        append("grilles/", Codes, Name_),
        append(Name_, ".txt", Na),
        append(Sol_, ".txt", So),
        string_to_list(I, Na),
        string_to_list(O, So),
        process(I, O),
        N2 is N-1,
        solve(N2)
.
