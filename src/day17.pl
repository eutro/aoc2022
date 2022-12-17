:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(insns(Insns), current_input),
    droprocks(Insns, 1000000000000, P1, P2),
    (var(P1) -> droprocks(Insns, 2022, _, P1) ; true),
    maplist(writeln, [P1, P2]).

droprocks(Insns, N, P1, Height) :- state_init(S0), nthstate(step(Insns), N, S0, P1, Height).

tower_height(s(_, _, Grid), Height) :- Height is -Grid.top.

% N is divisible by 2
nthstate(Step, N, S0, P1, Height) :- nthstate(Step, N, 0, S0, R-R, P1, Height).
nthstate(_, 0, _, S, _, _, Height) :- !, tower_height(S, Height).
nthstate(Step, N0, Off0, Fast0, [Slow | Slows]-R0, P1, Height) :-
    (N0 is 1000000000000 - 2022 -> tower_height(Fast0, P1) ; true),
    call(Step, Fast0, Fast1), call(Step, Fast1, Fast),
    R0 = [Fast1, Fast | R],
    N is N0 - 2,
    Off is Off0 + 1,
    Ss = Slow, Sf = Fast,
    (eqstate(Ss, Sf)
    -> divmod(N, Off, Repeats, Remainder),
       tower_height(Ss, As),
       tower_height(Sf, Ae),
       Acc is (Ae - As) * Repeats,
       Height #= Height0 + Acc,
       nthstate0(Step, Remainder, Sf, Height0)
    ; nthstate(Step, N, Off, Fast, Slows-R, P1, Height)).
% tighter iteration where we know there won't be cycles
nthstate0(_, 0, S, Height) :- tower_height(S, Height), !.
nthstate0(Step, N, S0, Height) :- N1 is N - 1, call(Step, S0, S), nthstate0(Step, N1, S, Height).

state_init(s(1, 0, Grid)) :- grid_init(Grid).
eqstate(s(IP, RP, Grid0), s(IP, RP, Grid1)) :-
    forall(between(1, 7, X),
           % comparing only the top values has no guarantee of working, but conveniently it does
           (rb_min(Grid0.col(X), Y1, _),
            rb_min(Grid1.col(X), Y2, _),
            Yn1 is Y1 - Grid0.top,
            Yn2 is Y2 - Grid1.top,
            Yn1 = Yn2)).

step(Insns, s(IP0, RP0, Grid0), s(IP, RP, Grid)) :-
    nthrock0(RP0, Rock),
    RP is (RP0 + 1) mod 5,
    droprock(Insns, Rock, IP0-Grid0, IP-Grid1),

    Grid1.map =.. [c | Cols0],
    maplist(
        cull_to_size(15), % I pulled this number out of my ass, any lower and it gets incorrect
        Cols0, Cols
    ),
    Map =.. [c | Cols],
    Grid = Grid1.put([map=Map]).

cull_to_size(Size, Map0, Map) :-
    rb_size(Map0, MapSz),
    (MapSz =< Size -> Map = Map0
    ; rb_del_max(Map0, _, _, Map1),
      cull_to_size(Size, Map1, Map)).

droprock(Insns, Rock, IP0-Grid0, IP-Grid) :-
    Y is Grid0.top - 3, X = 3,
    droprock(Insns, Rock, (X, Y), IP0-Grid0, IP-Grid).

droprock(Insns, Rock, Pos0, IP0-Grid0, IP-Grid) :-
    arg(IP0, Insns, Insn),
    (mvrock(Grid0, Rock, Insn, Pos0, Pos1) -> true ; Pos1 = Pos0),
    functor(Insns, _, InsnC),
    IP1 is IP0 mod InsnC + 1,
    (mvrock(Grid0, Rock, 'v', Pos1, Pos)
    -> droprock(Insns, Rock, Pos, IP1-Grid0, IP-Grid)
    ; IP = IP1, placerock(Rock, Pos1, Grid0, Grid)).

mvrock(Grid, Rock, Dir, Pos0, Pos) :-
    mvpos(Pos0, Dir, Pos),
    checkcoll(Grid, Rock, Dir, Pos).

mvpos((X0, Y), '>', (X1, Y)) :- X1 is X0 + 1.
mvpos((X0, Y), '<', (X1, Y)) :- X1 is X0 - 1.
mvpos((X, Y0), 'v', (X, Y1)) :- Y1 is Y0 + 1.

checkcoll(Grid, Rock, Dir, Pos) :-
    forall(member(Offset, Rock.get(Dir)),
           grid_empty(Grid, Pos, Offset)).

grid_init(grid{top: 0, map: Map}) :- length(Cols, 7), maplist(rb_empty, Cols), Map =.. [c | Cols].

grid:col(I, Grid, Col) :- arg(I, Grid.map, Col).

grid_empty(Grid, Pos, Offset) :-
    mvby(Pos, Offset, (X, Y)),
    between(1, 7, X),
    Y =< 0,
    \+ rb_lookup(Y, _, Grid.col(X)).

placerock(Rock, Pos, Grid0, Grid) :-
    copy_term(Grid0.map, Map),
    foldl(placetile(Pos), Rock.tiles, Grid0.put([map=Map]), Grid).
placetile(Pos, Tile, Grid0, Grid) :- mvby(Pos, Tile, PosAbs), grid_set(PosAbs, Grid0, Grid).
grid_set(Pos, Grid0, Grid) :-
    Pos = (X, Y),
    rb_insert_new(Grid0.col(X), Y, '#', Col),
    nb_setarg(X, Grid0.map, Col),
    Height is Y - 1,
    (Height < Grid0.top
    -> Grid = Grid0.put([top=Height])
    ; Grid = Grid0).

nthrock0(0,
         r{tiles: [(0,0), (1,0), (2,0), (3,0)],
           '<': [(0,0)],
           '>': [(3,0)],
           'v': [(0,0), (1,0), (2,0), (3,0)]}).
nthrock0(1,
         r{tiles: [         (1, -2),
                   (0, -1), (1, -1), (2, -1),
                            (1,  0)],
           '<': [(0, -1), (1, -2), (1, 0)],
           '>': [(2, -1), (1, -2), (1, 0)],
           'v': [(0, -1), (1, 0), (2, -1)]}).
nthrock0(2,
         r{tiles: [                (2, -2),
                                   (2, -1),
                   (0, 0), (1, 0), (2,  0)],
           '<': [(0,0), (2, -2), (2, -1)],
           '>': [(2, 0), (2, -1), (2, -2)],
           'v': [(0, 0), (1, 0), (2, 0)]}).
nthrock0(3,
         r{tiles: [(0,-3),
                   (0,-2),
                   (0,-1),
                   (0, 0)],
           '<': [(0,-3), (0,-2), (0,-1), (0, 0)],
           '>': [(0,-3), (0,-2), (0,-1), (0, 0)],
           'v': [(0,0)]}).
nthrock0(4,
         r{tiles: [(0,-1), (1,-1),
                   (0, 0), (1, 0)],
           '<': [(0, -1), (0, 0)],
           '>': [(1, -1), (1, 0)],
           'v': [(0, 0), (1, 0)]}).

insns(Insns) --> insns0(Insns0), { Insns =.. [i | Insns0] }.
insns0([]) --> eol, !.
insns0([I | Tl]) --> [C], { atom_codes(I, [C]) }, insns0(Tl).
