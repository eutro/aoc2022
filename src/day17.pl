:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(insns(Insns, Insns), current_input),
    grid_init(Grid0),
    droprocks(Insns, 2022, Grid0, Grid),
    % write_grid(Grid),
    tower_height(Grid, Height),
    writeln(Height),
    true.

tower_height(grid(Top, _), Height) :- Height is -Top.

droprocks(Insns, N, Grid0, Grid) :- droprocks(Insns, N, 0, Grid0, Grid).
droprocks(Insns, N, I, Grid0, Grid) :-
    I < N, !,
    droprock(Insns, Insns1, I, Grid0, Grid1),
    succ(I, I1),
    droprocks(Insns1, N, I1, Grid1, Grid).
droprocks(_, _, _, Grid, Grid).

droprock(Insns, Insns1, I, Grid0, Grid) :-
    grid(Top, _) = Grid0,
    Y is Top - 3,
    X = 3,
    nthrock(I, Rock),
    droprock(Insns, Insns1, Rock, (X, Y), Grid0, Grid).

droprock([Insn | Tl], Insns1, Rock, Pos0, Grid0, Grid) :-
    (mvrock(Grid0, Rock, Insn, Pos0, Pos1) -> true ; Pos1 = Pos0),
    % writeln(Insn), write_grid(Grid0, Rock, Pos1),
    (mvrock(Grid0, Rock, 'v', Pos1, Pos) %, writeln(v), write_grid(Grid0, Rock, Pos)
    -> droprock(Tl, Insns1, Rock, Pos, Grid0, Grid)
    ; Insns1 = Tl, placerock(Rock, Pos1, Grid0, Grid)).

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

grid_init(grid(0, Empty)) :- rb_empty(Empty).

write_grid(Grid) :- write_grid(Grid, r{tiles:[]}, (0, 0)).
write_grid(grid(Top, Map), Rock, Pos) :-
    (_, Row) = Pos,
    Top1 is min(Row, Top) - 2,
    forall(between(Top1, 0, Y),
           (write('|'),
            forall(between(1, 7, X),
                   (mvby(Pos, Offset, (X, Y)),
                    (memberchk(Offset, Rock.tiles) -> Vl = '@'
                    ; rb_lookup((X, Y), Vl, Map) -> true
                    ; Vl = '.'),
                    write(Vl))),
            writeln('|'))),
    write('+'), forall(between(1, 7, _), write('-')), writeln('+').

grid_empty(Grid, Pos, Offset) :- mvby(Pos, Offset, PosAbs), grid_empty(Grid, PosAbs).
grid_empty(grid(_, Map), Pos) :- \+ rb_lookup(Pos, _, Map), Pos = (X, Y), between(1, 7, X), Y =< 0.
grid_set(Pos, grid(Top0, Map0), grid(Top, Map)) :-
    rb_insert(Map0, Pos, '#', Map),
    Pos = (_, Y), Top is min(Top0, Y - 1).

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

insns(End, End) --> eol, !.
insns([I | Tl], End) --> [C], { atom_codes(I, [C]) }, insns(Tl, End).
