:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(input(Grid, Insns), current_input),
    once((between(1, 100, X0), gridref(Grid, (X0, 1), Vl), Vl = 0'.)),
    forall(member(Mode, [p1, p2]),
           (run(Mode, Grid, Insns, (X0, 1)-0, (X, Y)-R),
            Ans is 1000 * Y + 4 * X + R,
            writeln(Ans))).

write_grid(Grid) :-
    forall(arg(_, Grid, Row),
           (Row =.. [_ | Codes],
            string_codes(Str, Codes),
            writeln(Str))).

:- det(run/4).
run(Mode, Grid, Insns, S0, S) :-
    build_wrapmap(Mode, Grid, Map),
    foldl(run0(Grid, Map), Insns, S0, S).
:- det(run0/4).
run0(_, _, rot(D), Pos-R0, Pos-R) :- R is (R0 + D) mod 4, !.
run0(Grid, Map, mv(N), S0, S) :- mv(Grid, Map, N, S0, S).

:- det(mv/4).
mv(_, _, 0, S, S) :- !.
mv(Grid, Map, N, S0, S) :-
    % symbol(R, Sym),
    % gridset(Grid, Pos0, Sym),
    wrap(Map, S0, S1),
    S1 = Pos1-_,
    gridref(Grid, Pos1, Vl),
    (Vl = 0'# -> S = S0
    ; N1 is N - 1,
      mv(Grid, Map, N1, S1, S)).

:- det(wrap/3).
wrap(Map, S0, S) :- rb_lookup(S0, S, Map), !.
wrap(Map, Pos0-R, Pos-R) :- move(R, Pos0, Pos).

move(Dir, Pos0, Pos) :- delta(Dir, D), mvby(Pos0, D, Pos).
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

build_wrapmap(Mode, Grid, Map) :-
    rb_empty(Map0),
    build_wrapmap(Mode, Grid, Map0, Map).
build_wrapmap(p1, Grid, Map0, Map) :-
    bagof(Y-Row, arg(Y, Grid, Row), Rows),
    foldl(build_wraprow, Rows, Map0, Map1),
    arg(1, Grid, Row0),
    functor(Row0, _, ColC),
    bagof(X, between(1, ColC, X), Cols),
    foldl(build_wrapcol(Grid), Cols, Map1, Map).
build_wraprow(Y-Row, Map0, Map) :-
    Row =.. [_ | RowL],
    phrase(fillrange(Offset, Span), RowL),
    LeftX is Offset + 1,
    RightX is Offset + Span,
    rb_insert(Map0, (LeftX, Y)-2, (RightX, Y)-2, Map1),
    rb_insert(Map1, (RightX, Y)-0, (LeftX, Y)-0, Map).
build_wrapcol(Grid, X, Map0, Map) :-
    Grid =.. [_ | Rows],
    maplist(arg(X), Rows, Col),
    phrase(fillrange(Offset, Span), Col),
    TopY is Offset + 1,
    BotY is Offset + Span,
    rb_insert(Map0, (X, BotY)-1, (X, TopY)-1, Map1),
    rb_insert(Map1, (X, TopY)-3, (X, BotY)-3, Map).

fillrange(Offset, Span) -->
    string_without(`\n.#`, Padding),
    string_without(` \n`, Content),
    remainder(_),
    { length(Padding, Offset),
      length(Content, Span) }.

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
