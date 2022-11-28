main :-
    read_dims(Dims),
    maplist(pres_paper, Dims, Papers),
    sum_list(Papers, WrappingPaper),
    maplist(pres_ribbon, Dims, Ribbons),
    sum_list(Ribbons, Ribbon),
    write(WrappingPaper),nl,write(Ribbon),nl.

read_dims(Dims) :- read_line_to_string(current_input, Line), read_dims(Line, Dims).
read_dims(end_of_file, []).
read_dims(Line, [Dim | RestDims]) :-
    parse_dim(Line, Dim),
    read_line_to_string(current_input, NextLn), read_dims(NextLn, RestDims).

parse_dim(Line, Dim) :-
    split_string(Line, "x", "", DimRaw),
    maplist(number_codes, Dim, DimRaw).

pres_paper([X, Y, Z], Paper) :-
    A is X * Y,
    B is X * Z,
    C is Y * Z,
    Paper is 2 * A + 2 * B + 2 * C + min(A, min(B, C)).

pres_ribbon([X, Y, Z], Ribbon) :-
    A is X + Y,
    B is X + Z,
    C is Y + Z,
    Ribbon is 2 * min(A, min(B, C)) + X * Y * Z.
