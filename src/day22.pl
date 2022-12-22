:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(input(Grid, Insns), current_input),
    once((between(1, 100, X0), gridref(Grid, (X0, 1), Vl), Vl = 0'.)),
    forall(member(Mode, [p1, p2]),
           (run(Mode, Grid, Insns, (X0, 1)-0, (X, Y)-R),
            Ans is 1000 * Y + 4 * X + R,
            % write_grid(Grid),
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
    S0 = Pos0-R,
    symbol(R, Sym),
    gridset(Grid, Pos0, Sym),
    wrap(Map, S0, S1),
    S1 = Pos1-_,
    gridref(Grid, Pos1, Vl),
    (Vl = 0'# -> S = S0
    ; N1 is N - 1,
      mv(Grid, Map, N1, S1, S)).

:- det(wrap/3).
wrap(Map, S0, S) :- rb_lookup(S0, S, Map), !.
wrap(_, Pos0-R, Pos-R) :- move(R, Pos0, Pos).

move(Dir, Pos0, Pos) :- delta(Dir, D), mvby(Pos0, D, Pos).
delta(0, D) => D = ( 1,  0).
delta(1, D) => D = ( 0,  1).
delta(2, D) => D = (-1,  0).
delta(3, D) => D = ( 0, -1).
symbol(0, 0'>) :- !.
symbol(1, 0'v) :- !.
symbol(2, 0'<) :- !.
symbol(3, 0'^) :- !.

gridref(Grid, (X, Y), Vl) :-
    arg(Y, Grid, Row),
    arg(X, Row, Vl).
gridset(Grid, (X, Y), Vl) :-
    arg(Y, Grid, Row),
    setarg(X, Row, Vl).

build_wrapmap(Mode, Grid, Map) :-
    rb_empty(Map0),
    build_wrapmap(Mode, Grid, Map0, Map).
build_wrapmap(p1, Grid, Map0, Map) :- build_wrapmap_p1(Grid, Map0, Map).
build_wrapmap(p2, Grid, Map0, Map) :- build_wrapmap_p2(Grid, Map0, Map).

build_wrapmap_p1(Grid, Map0, Map) :-
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

build_wrapmap_p2(Grid, Map0, Map) :-
    functor(Grid, _, Rows),
    arg(1, Grid, Row0),
    functor(Row0, _, Cols),
    CellSz is gcd(Rows, Cols),
    Width is Cols // CellSz,
    Height is Rows // CellSz,
    grid_tiles(Grid, Width, Height, CellSz, Tiles),
    walk_tree(Tiles, Map0, Map).
grid_tiles(Grid, Width, Height, CellSz, Tiles) :-
    bagof(Tile, grid_tile(Grid, Width, Height, CellSz, Tile), Tiles).
grid_tile(Grid, Width, Height, CellSz, (U, V)-Tile) :-
    between(1, Width, U),
    between(1, Height, V),
    grid_tile_uv(Grid, U, V, CellSz, Tile).

walk_tree(Tiles, Map0, Map) :-
    [Root | _] = Tiles,
    trans_id(Id),
    walk_tree(Tiles, Root, Id, []-[], _-Edges0),
    keysort(Edges0, Edges),
    phrase(fuse_edges(Map0, Map), Edges).

fuse_edges(Map, Map) --> eos, !.
fuse_edges(Map0, Map) -->
    [_-Lhs, _-Rhs],
    { fuse_edge(Lhs, Rhs, Map0, Map1) },
    fuse_edges(Map1, Map).

fuse_edge(edge(DirL, StartL, EndL),
          edge(DirR, StartR, EndR),
          Map0, Map) :-
    % writeln(fuse_edge(edge(DirL, StartL, EndL),
    %                   edge(DirR, StartR, EndR))),
    edge_list(StartL, EndL, Left),
    edge_list(StartR, EndR, Right),
    foldl(fuse_tile(DirL, DirR), Left, Right, Map0, Map).

edge_list(Start, Start, [Start]) :- !.
edge_list(Start, End, [Start | Tl]) :-
    mvby(Start, (DX, DY), End),
    (X0, Y0) = Start,
    X is X0 + sign(DX),
    Y is Y0 + sign(DY),
    edge_list((X, Y), End, Tl).

fuse_tile(DirL, DirR, Left, Right, Map0, Map) :-
    flip_dir(DirL, RDirL), flip_dir(DirR, RDirR),
    rb_insert(Map0, Left-DirL, Right-RDirR, Map1),
    rb_insert(Map1, Right-DirR, Left-RDirL, Map).

flip_dir(A, B) :- B is (A + 2) mod 4.

walk_tree(Tiles, Pos-Tile, Trans, Seen0-Edges0, Seen-Edges) :-
    maplist(matmulv(Trans), [v(0, 0, 0), v(0, 1, 0), v(1, 0, 0), v(1, 1, 0)], LEdges0),
    pairs_keys_values([Tl, Bl, Tr, Br], LEdges0, Tile.vs),
    maplist(normedge,
            [edge(0'^, Tl, Tr), edge(0'v, Bl, Br), edge(0'<, Tl, Bl), edge(0'>, Tr, Br)],
            LEdges),
    append(LEdges, Edges0, Edges1),
    foldl(walk_edge(Tiles, Trans), Tile.nbs, [Pos | Seen0]-Edges1, Seen-Edges).

normedge(edge(Sym, Start, End), (Swc,Ewc)-edge(Dir, Slc, Elc)) :-
    symbol(Dir, Sym),
    keysort([Start, End], [Swc-Slc, Ewc-Elc]).

walk_edge(Tiles, Trans0, Side-Pos, Seen0-Edges0, Seen-Edges) :-
    (memberchk(Pos, Seen0)
    -> Seen = Seen0, Edges = Edges0
    ; foldtrans(Side, SideTrans),
      matmul(Trans0, SideTrans, Trans),
      member(Pos-Tile, Tiles),
      walk_tree(Tiles, Pos-Tile, Trans, Seen0-Edges0, Seen-Edges)).

matmul(m(M1, M2, M3, Mx1,
         M4, M5, M6, Mx2,
         M7, M8, M9, Mx3),
       m(N1, N2, N3, Nx1,
         N4, N5, N6, Nx2,
         N7, N8, N9, Nx3),
       m(O1, O2, O3, Ox1,
         O4, O5, O6, Ox2,
         O7, O8, O9, Ox3)) :-
    O1 is M1 * N1 + M2 * N4 + M3 * N7,
    O2 is M1 * N2 + M2 * N5 + M3 * N8,
    O3 is M1 * N3 + M2 * N6 + M3 * N9,
    O4 is M4 * N1 + M5 * N4 + M6 * N7,
    O5 is M4 * N2 + M5 * N5 + M6 * N8,
    O6 is M4 * N3 + M5 * N6 + M6 * N9,
    O7 is M7 * N1 + M8 * N4 + M9 * N7,
    O8 is M7 * N2 + M8 * N5 + M9 * N8,
    O9 is M7 * N3 + M8 * N6 + M9 * N9,
    Ox1 is M1 * Nx1 + M2 * Nx2 + M3 * Nx3 + Mx1,
    Ox2 is M4 * Nx1 + M5 * Nx2 + M6 * Nx3 + Mx2,
    Ox3 is M7 * Nx1 + M8 * Nx2 + M9 * Nx3 + Mx3.
matmulv(m(M1, M2, M3, Mx1,
          M4, M5, M6, Mx2,
          M7, M8, M9, Mx3),
        v(V1, V2, V3),
        v(U1, U2, U3)) :-
    U1 is M1 * V1 + M2 * V2 + M3 * V3 + Mx1,
    U2 is M4 * V1 + M5 * V2 + M6 * V3 + Mx2,
    U3 is M7 * V1 + M8 * V2 + M9 * V3 + Mx3.
trans_id(m(1, 0, 0, 0,
           0, 1, 0, 0,
           0, 0, 1, 0)).

foldtrans(left,
          m( 0, 0, 1, 0,
             0, 1, 0, 0,
            -1, 0, 0, 1)).
foldtrans(up,
          m(1,  0, 0, 0,
            0,  0, 1, 0,
            0, -1, 0, 1)).
foldtrans(right,
          m(0, 0, -1, 1,
            0, 1,  0, 0,
            1, 0,  0, 0)).
foldtrans(down,
          m(1, 0,  0, 0,
            0, 0, -1, 1,
            0, 1,  0, 0)).

tile_exists(Grid, U, V, CellSz, X, Y) :-
    X is (U - 1) * CellSz + 1,
    Y is (V - 1) * CellSz + 1,
    X > 0, Y > 0,
    gridref(Grid, (X, Y), Vl),
    Vl \= 0' .

grid_tile_uv(Grid, U, V, CellSz, Tile) :-
    tile_exists(Grid, U, V, CellSz, X, Y),
    Xe is U * CellSz,
    Ye is V * CellSz,
    bagof(Dir-(U1, V1), neighbour(Grid, U, V, CellSz, Dir, U1, V1), Neighbours),
    Tile = t{vs: [(X, Y), (X, Ye), (Xe, Y), (Xe, Ye)],
             nbs: Neighbours}.

neighbour(Grid, U, V, CellSz, Dir, U1, V1) :-
    member(Dir-(DU, DV), [right-(1, 0), left-(-1, 0), down-(0, 1), up-(0, -1)]),
    U1 is U + DU,
    V1 is V + DV,
    tile_exists(Grid, U1, V1, CellSz, _, _).

fillrange(Offset, Span) -->
    string_without(`\n.#`, Padding),
    string_without(` \n`, Content),
    remainder(_),
    { length(Padding, Offset),
      length(Content, Span) }.

input(Grid, Insns) --> grid(Grid), path(Insns), remainder(_).

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
