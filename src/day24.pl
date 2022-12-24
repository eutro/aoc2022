:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(grid(Grid), current_input),
    Start = (1, 0),
    H is Grid.height + 1,
    End = (Grid.width, H),
    find_path(Grid, 0, End, [Start], P1),
    writeln(P1),
    find_path(Grid, P1, Start, [End], Snacks),
    find_path(Grid, Snacks, End, [Start], P2),
    writeln(P2).

write_grid(Grid, Time, Posns) :-
    W is Grid.width + 1,
    H is Grid.height + 1,
    forall(between(0, H, Y),
           (forall(between(0, W, X),
                   ((\+ in_bounds(Grid, (X, Y)) -> Vl = '#'
                    ; memberchk((X, Y), Posns) -> Vl = 'E'
                    ; has_gust(Grid, (X, Y), Time, Gust) -> Vl = Gust
                    ; Vl = '.' ),
                    write(Vl))),
            nl)).

find_path(Grid, Time0, Target, Posns0, Time) :-
    % write_grid(Grid, Time0, Posns0), nl,
    Time1 is Time0 + 1,
    step_grid(Grid, Time1, Posns0, Posns1),
    ( memberchk(Target, Posns1) -> Time = Time1
    ; find_path(Grid, Time1, Target, Posns1, Time) ).

step_grid(Grid, Time, Posns0, Posns) :-
    maplist(neighbours(Grid, Time), Posns0, Posns1),
    append(Posns1, Posns2),
    sort(Posns2, Posns).

in_bounds(Grid, (X, Y)) :-
    ( (X, Y) = (1, 0) -> true
    ; H is Grid.height + 1, (X, Y) = (Grid.width, H) -> true
    ; between(1, Grid.width, X),
      between(1, Grid.height, Y) ).
neighbours(Grid, Time, Pos, Posns) :-
    findall(Nb, ((neighbour(Grid, Pos, Nb) ; Nb = Pos), \+ has_gust(Grid, Nb, Time, _)), Posns).
neighbour(Grid, (X, Y), (X1, Y1)) :-
    ( X1 is X + 1, Y1 = Y
    ; Y1 is Y + 1, X1 = X
    ; X1 is X - 1, Y1 = Y
    ; Y1 is Y - 1, X1 = X ),
    in_bounds(Grid, (X1, Y1)).

has_gust(Grid, Pos, Time, Gust) :-
    once((member(Dir, Grid.dirs),
          unaxis(Dir, Pos, Set, Axis),
          AxisPos is ((Axis + Time * Dir.dt - 1) mod Dir.sz) + 1,
          AxisPos in_set Set,
          Gust = Dir.sym)) .
unaxis(Dir, Pos, Set, Axis) :-
    pick_axis(Dir.axis, Pos, Axis, Idx),
    arg(Idx, Dir.sets, Set).

write_grid(Grid) :-
    forall(arg(_, Grid, Row),
           (forall(arg(_, Row, Tiles),
                   write_tile(Tiles)),
            nl)).
write_tile(C) :- write(C).

gridref(Grid, (X, Y), Tile) :-
    arg(Y, Grid, Row),
    arg(X, Row, Tile).
gridset(Grid, (X, Y), Tile) :-
    arg(Y, Grid, Row),
    setarg(X, Row, Tile).

pick_axis(horiz, (X, Y), X, Y).
pick_axis(vert, (X, Y), Y, X).
index_gusts(Grid-Width-Height, Sym-Dt-Axis,
            d{axis: Axis,
              sz: Sz,
              dt: Dt,
              sym: Atm,
              sets: Sets}) :-
    atom_codes(Atm, [Sym]),
    pick_axis(Axis, (Width, Height), Sz, Count),
    findall(Set,
            (between(1, Count, N),
             pick_axis(Axis, (X, Y), Unbound, N),
             findall(Unbound, gridref(Grid, (X, Y), Sym), Vals),
             list_to_fdset(Vals, Set)),
            Sets0),
    Sets =.. [s | Sets0].

grid(g{width: Width,
       height: Height,
       dirs: Dirs}) -->
    grid0(Rows0),
    { append([_ | Rows], [_], Rows0),
      Grid1 =.. [g | Rows],
      functor(Grid1, _, Height),
      arg(1, Grid1, Row1),
      functor(Row1, _, Width),
      maplist(index_gusts(Grid1-Width-Height),
              [0'<-( 1)-horiz,
               0'>-(-1)-horiz,
               0'^-( 1)-vert,
               0'v-(-1)-vert],
              Dirs) }.
grid0([]) --> eos, !.
grid0([Row | Tl]) --> row(Row0), { append([_ | Row1], [_], Row0), Row =.. [r | Row1] }, grid0(Tl).
row([]) --> eol, !.
row([Tile | Tl]) --> tile(Tile), row(Tl).
tile(C) --> [C], !.
