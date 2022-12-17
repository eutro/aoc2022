:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(insns(Insns), current_input),
    droprocks(Insns, 2022, P1),
    writeln(P1),
    droprocks(Insns, 1000000000000, P2),
    writeln(P2),
    true.

droprocks(Insns, N, Height) :-
    state_init(S0),
    nthstate(step(Insns), N, S0, Height).

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
       % writeln(loop{end: N, len: Off}),
       tower_height(Ss, As),
       tower_height(Sf, Ae),
       Acc is (Ae - As) * Repeats,
       Height #= Height0 + Acc,
       nthstate(Step, Remainder, Sf, Height0)
    ; nthstate(Step, N, Off, Slow, Fast, Height)))).

state_init(s(1, 0, Grid)) :- grid_init(Grid).
eqstate(s(IP, RP, Grid0), s(IP, RP, Grid1)) :-
    forall(between(1, 7, X),
           (once(rb_in((Y1, X), _, Grid0.map)),
            once(rb_in((Y2, X), _, Grid1.map)),
            Yn1 is Y1 - Grid0.top,
            Yn2 is Y2 - Grid1.top,
            Yn1 = Yn2)).

step(Insns, s(IP0, RP0, Grid0), s(IP, RP, Grid)) :-
    nthrock0(RP0, Rock),
    RP is (RP0 + 1) mod 5,
    droprock(Insns, Rock, IP0-Grid0, IP-Grid1),
    normalize_grid(Grid1, Grid).

normalize_grid(Grid0, Grid0.put(map, Map)) :- cull_to_size(1000, Grid0.map, Map).

cull_to_size(Size, Map0, Map) :-
    rb_size(Map0, MapSz),
    (MapSz =< Size -> Map = Map0
    ; rb_del_max(Map0, _, _, Map1),
      cull_to_size(Size, Map1, Map)).

grid_cut(0, Grid, Grid).
grid_cut(By, Grid0, Grid) :-
    Grid = grid{top: Top, map: Map},
    Top is Grid0.top + By,
    rb_visit(Grid0.map, Kvs0),
    convlist(poscut(By), Kvs0, Kvs),
    list_to_rbtree(Kvs, Map),
    %writeln("before"), write_grid(Grid0),
    %writeln("after"), write_grid(Grid),
    true.

poscut(By, (Y, X)-Val, (Y1, X)-Val) :- Y1 is Y + By, Y1 =< 0.

droprock(Insns, Rock, IP0-Grid0, IP-Grid) :-
    Y is Grid0.top - 3, X = 3,
    droprock(Insns, Rock, (X, Y), IP0-Grid0, IP-Grid).

droprock(Insns, Rock, Pos0, IP0-Grid0, IP-Grid) :-
    arg(IP0, Insns, Insn),
    (mvrock(Grid0, Rock, Insn, Pos0, Pos1) -> true ; Pos1 = Pos0),
    functor(Insns, _, InsnC),
    IP1 is IP0 mod InsnC + 1,
    % writeln(Insn), write_grid(Grid0, Rock, Pos1),
    (mvrock(Grid0, Rock, 'v', Pos1, Pos) %, writeln(v), write_grid(Grid0, Rock, Pos)
    -> droprock(Insns, Rock, Pos, IP1-Grid0, IP-Grid)
    ; IP = IP1,
      placerock(Rock, Pos1, Grid0, Grid)).

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

grid_init(grid{top: 0, map: Empty}) :- rb_empty(Empty).

write_grid(Grid) :- write_grid(Grid, r{tiles:[]}, (0, 0)).
write_grid(Grid, Rock, Pos) :-
    (_, Row) = Pos,
    Top1 is min(Row, Grid.top) - 2,
    forall(between(Top1, 0, Y),
           (write('|'),
            forall(between(1, 7, X),
                   (mvby(Pos, Offset, (X, Y)),
                    (memberchk(Offset, Rock.tiles) -> Vl = '@'
                    ; rb_lookup((Y, X), Vl, Grid.map) -> true
                    ; Vl = '.'),
                    write(Vl))),
            writeln('|'))),
    write('+'), forall(between(1, 7, _), write('-')), writeln('+').

trans((X, Y), (Y, X)).

grid_empty(Grid, Pos, Offset) :- mvby(Pos, Offset, PosAbs), grid_empty(Grid, PosAbs).
grid_empty(Grid, Pos) :- trans(Pos, TPos), \+ rb_lookup(TPos, _, Grid.map), Pos = (X, Y), between(1, 7, X), Y =< 0.
grid_set(Pos, Grid0, room{top: Top, map: Map}) :-
    trans(Pos, TPos), rb_insert(Grid0.map, TPos, '#', Map),
    Pos = (_, Y), Height is Y - 1,
    Top is min(Grid0.top, Height).

nthrock(I, R) :- I5 is I mod 5, nthrock0(I5, R), !.
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
