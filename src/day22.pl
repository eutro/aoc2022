:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(input(Grid, Insns), current_input),
    once((between(1, 100, X0), gridref(Grid, (X0, 1), Vl), Vl = 0'.)),
    run(Grid, Insns, (X0, 1)-0, (X, Y)-R),
    P1 is 1000 * Y + 4 * X + R,
    writeln(P1),
    % write_grid(Grid),
    true.

write_grid(Grid) :-
    forall(arg(_, Grid, Row),
           (Row =.. [_ | Codes],
            string_codes(Str, Codes),
            writeln(Str))).

:- det(run/4).
run(_, [], S0, S) => S = S0.
run(Grid, [I | Tl], S0, S) => run0(Grid, I, S0, S1), run(Grid, Tl, S1, S).
:- det(run0/4).
run0(_, rot(D), Pos-R0, Pos-R) :- R is (R0 + D) mod 4, !.
run0(Grid, mv(N), Pos0-R, Pos-R) :-
    delta(R, D),
    mv(Grid, R, D, N, Pos0, Pos).

:- det(mv/6).
mv(_, _, _, 0, Pos, Pos) :- !.
mv(Grid, R, D, N, Pos0, Pos) :-
    symbol(R, Sym),
    gridset(Grid, Pos0, Sym),
    wrap(Grid, D, Pos0, Pos1),
    gridref(Grid, Pos1, Vl),
    (Vl = 0'# -> Pos = Pos0
    ; N1 is N - 1,
      mv(Grid, R, D, N1, Pos1, Pos)).

:- det(wrap/4).
wrap(Grid, D, Pos0, Pos) :-
    mvby(Pos0, D, (X0, Y0)),
    functor(Grid, _, Rows),
    Y is ((Y0 - 1) mod Rows) + 1,
    arg(Y, Grid, Row),
    functor(Row, _, Cols),
    X is ((X0 - 1) mod Cols) + 1,
    Pos1 = (X, Y),
    gridref(Grid, Pos1, Vl),
    (Vl = 0'  -> wrap(Grid, D, Pos1, Pos)
    ; Pos = Pos1).

delta(0, D) => D = ( 1,  0).
delta(1, D) => D = ( 0,  1).
delta(2, D) => D = (-1,  0).
delta(3, D) => D = ( 0, -1).
symbol(0, 0'>) :- !.
symbol(1, 0'v) :- !.
symbol(2, 0'<) :- !.
symbol(3, 0'^) :- !.

:- det(gridref/3).
gridref(Grid, (X, Y), Vl) :-
    arg(Y, Grid, Row),
    arg(X, Row, Vl).
gridset(Grid, (X, Y), Vl) :-
    arg(Y, Grid, Row),
    setarg(X, Row, Vl).

input(Grid, Insns) --> grid(Grid), path(Insns).

grid(Grid) -->
    grid0(Grid0),
    { maplist(length, Grid0, Lengths),
      max_list(Lengths, Width),
      maplist(padrow(Width), Grid0, Grid1),
      Grid =.. [g | Grid1] }.
grid0([]) --> string_without(`\n`, []), eol, !.
grid0([Row | Tl]) --> row(Row), eol, grid0(Tl).
row(Row) --> string_without(`\n`, Row).

padrow(Width, Codes, Row) :-
    length(Row0, Width),
    append(Codes, Padding, Row0),
    maplist(=(0' ), Padding),
    Row =.. [r | Row0].

path([]) --> eol, !.
path([Insn | Tl]) --> insn(Insn), path(Tl).
insn(mv(N)) --> integer(N), !.
insn(rot(-1)) --> `L`, !.
insn(rot(1)) --> `R`.
