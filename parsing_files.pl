integer(I) -->
        digit(D0),
        digits(D),
        { number_codes(I, [D0|D])
        }.

digits([D|T]) -->
        digit(D), !,
        digits(T).
digits([]) -->
        [].

digit(D) -->
        [D],
        { code_type(D, digit)
        }.
        
ints([], _, _).
ints(Sentence, Result, R) :- phrase(integer(X), Sentence, Rest), append([X], Result, R), ints(Rest, R, R2), R = R2.

t([A|_]) :- A is 0'a.

%% i(): true if the 2nd parameter contains the list of all 
%% integers contained in the string (passed as ASCII list) 
%% passed as 1st parameter
% example usage: i("1 2 3", [49, 50, 51]).
% another example usage: i("1 _ 3", [49, 51]).
digit_code(J) :- J >= 0'0, J =< 0'9.
i([], []).
i([], _) :- fail.
i([A|S], [A|R]) :- digit_code(A),!, i(S, R).
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

% @TODO: find a way to get the line as an ASCII list (with read() ? read_lines_to_codes() ? ) and then use super_i()
% @TODO: Then, use the same way remplis() does to generate a grid (zone(X, Y, X), v(p(X, Y, Z), Value))
