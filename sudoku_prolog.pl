grille(X) :- X = [
v(p(1, 1, 1), 4),
v(p(1, 4, 2), 5),
v(p(1, 6, 2), 3),
v(p(1, 7, 3), 7),
v(p(1, 8, 3), 6),
v(p(2, 1, 1), 5),
v(p(2, 4, 2), 1),
v(p(2, 6, 2), 7),
v(p(3, 5, 2), 9),
v(p(3, 8, 3), 3),
v(p(4, 1, 4), 2),
v(p(4, 2, 4), 5),
v(p(4, 4, 5), 9),
v(p(4, 7, 6), 1),
v(p(5, 2, 4), 9),
v(p(5, 3, 4), 6),
v(p(5, 5, 5), 1),
v(p(5, 7, 6), 3),
v(p(5, 8, 6), 2),
v(p(6, 3, 4), 3),
v(p(6, 6, 5), 6),
v(p(6, 8, 6), 9),
v(p(6, 9, 6), 8),
v(p(7, 2, 7), 2),
v(p(7, 5, 8), 4),
v(p(8, 4, 8), 8),
v(p(8, 6, 8), 5),
v(p(8, 9, 9), 3),
v(p(9, 2, 7), 8),
v(p(9, 3, 7), 5),
v(p(9, 4, 8), 6),
v(p(9, 6, 8), 1),
v(p(9, 9, 9), 2)].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% prettyprint prints to the output in an elegant way %%

prettyprint(_, []). % End of the print
prettyprint(F, [v(p(_, 9, _), V)|N]) :- write(F, V), nl(F), prettyprint(F, N).
prettyprint(F, [v(_, V)|N]):- write(F, V), write(F, ' '), prettyprint(F, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sudoku_solve solves the sudoku %%

sudoku_solve(Remplies, Couleurs, Solutions) :-
	% init
    complete(Remplies, Couleurs, RestantesBrute),
    predsort(sort_cases, RestantesBrute, Restantes),
    % Solving
    resout(Restantes, Remplies, Solutions).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Completes
complete(Remplies, Couleurs, Restantes) :-
    remplis(1, 1, Cases),
    filtre_existantes(Cases, Remplies, Manquantes), !,
    pondere(Manquantes, Remplies, Couleurs, Restantes).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

filtre_existantes([], _, []).
filtre_existantes([C|S], Remplies, [C|R]) :-
    not(memberchk(v(C, _), Remplies)),
    filtre_existantes(S, Remplies, R).
filtre_existantes([C|S], Remplies, R) :-
    memberchk(v(C, _), Remplies),
    filtre_existantes(S, Remplies, R).

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
    % write(v(P, F)), nl,
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%% BATCH PROCESSING %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
  C_ is mod(N, 9), % col does not go after 9
  ((C_ \= 0, C=C_) ; C = 9),
  L is ceil(N/9), % line is ceil(N/9) (1-9 = 1, 2-18=2, etc. ...)
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

test(0, _).
test(N, [Nam|FR]) :-
        %trace,
        number_codes(N, Codes),
        append("solutions/", Codes, Sol_),
        append("grilles/", Codes, Name_),
        append(Name_, ".txt", Na),
        append(Sol_, ".txt", So),
        string_to_list(Nam, Na),
        string_to_list(Sol, So),
        open(Nam, read, F),
        read_lines(L, F),
        close(F),
        !,
        transf2(L, G), !,
        open(Sol, write, S),
        % debug stuff: 
        %trace,
        %write(S, G),
        sudoku_solve(G, [1, 2, 3, 4, 5, 6, 7, 8, 9], R),
        predsort(sort_res, R, T),
        prettyprint(S, T),
        nl(S),
        close(S), !,
        N2 is N-1,
        test(N2, FR)
.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%