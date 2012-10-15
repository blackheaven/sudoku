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

test(S) :- 
    grille(G),
    sudoku_solve(G, [1, 2, 3, 4, 5, 6, 7, 8, 9], S)
    .

solve :-
    test(R),
    open('grille-res.txt', write, S),
    predsort(sort_res, R, T),
    prettyprint(S, T),
    nl(S),
    close(S), !.

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
