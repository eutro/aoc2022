:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(insns(Insns), current_input),
    forall(member(Count, [2022, 1000000000000]),
           (droprocks(Insns, Count, Ans),
            writeln(Ans))).

droprocks(Insns, N, Height) :- state_init(S0), nthstate(step(Insns), N, S0, Height).

tower_height(s(_, _, Grid), Height) :- Height is -Grid.top.

nthstate(Step, N, S0, Height) :- nthstate(Step, N, 0, S0, S0, Height).
nthstate(Step, N0, Off0, Slow0, Fast0, Height) :-
    call(Step, Slow0, Slow),
    call(Step, Fast0, Fast1),
    succ(N1, N0), (N1 = 0 -> tower_height(Fast1, Height) ;
    call(Step, Fast1, Fast),
    succ(N, N1), (N = 0 -> tower_height(Fast, Height) ;
    succ(Off0, Off),
    Ss = Slow, Sf = Fast,
    (eqstate(Ss, Sf)
    -> divmod(N, Off, Repeats, Remainder),
       tower_height(Ss, As),
       tower_height(Sf, Ae),
       Acc is (Ae - As) * Repeats,
       Height #= Height0 + Acc,
       nthstate0(Step, Remainder, Sf, Height0)
    ; nthstate(Step, N, Off, Slow, Fast, Height)))).
% tighter iteration where we know there won't be cycles
nthstate0(Step, 0, S, Height) :- tower_height(S, Height), !.
nthstate0(Step, N, S0, Height) :- succ(N1, N), call(Step, S0, S), nthstate0(Step, N1, S, Height).

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

placerock(Rock, Pos, Grid0, Grid) :- foldl(placetile(Pos), Rock.tiles, Grid0, Grid).
placetile(Pos, Tile, Grid0, Grid) :- mvby(Pos, Tile, PosAbs), placetile(PosAbs, Grid0, Grid).
placetile(Pos, Grid0, Grid) :- grid_empty(Grid0, Pos), grid_set(Pos, Grid0, Grid).

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

grid_empty(Grid, Pos, Offset) :- mvby(Pos, Offset, PosAbs), grid_empty(Grid, PosAbs).
grid_empty(Grid, Pos) :- Pos = (X, Y), between(1, 7, X), Y =< 0, \+ rb_lookup(Y, _, Grid.col(X)).
grid_set(Pos, Grid0, grid{top: Top, map: Map}) :-
    Pos = (X, Y),
    copy_term(Grid0.map, Map),
    rb_insert(Grid0.col(X), Y, '#', Col),
    nb_setarg(X, Map, Col),
    Top is min(Grid0.top, Y - 1).

:- table nthrock0/2.
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
