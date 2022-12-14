:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(cave(Cave), current_input),
    drop_sand(p1, Cave, _, P1),
    writeln(P1),
    drop_sand(p2, Cave, Cave2, P2),
    % write_cave(Cave2),
    writeln(P2),
    true.

drop_sand(Mode, Cave0, Cave, Count) :-
    extrema(Cave0, _, (_, MaxY)),
    drop_sand(Mode, MaxY, (500, 0), Cave0, Cave, 0, Count).

drop_sand(p1, MaxY, (_, Y), Cave, Cave, Count, Count) :- Y > MaxY, !.
drop_sand(Mode, MaxY, (X, Y), Cave0, Cave, Count0, Count) :-
    Y1 is Y + 1,
    (member(Dx, [0, -1, 1]),
     X1 is X + Dx,
     Pos = (X1, Y1),
     pos_empty(Mode, MaxY, Cave0, Pos), !
    -> drop_sand(Mode, MaxY, Pos, Cave0, Cave, Count0, Count)
    ; rb_insert(Cave0, (X, Y), 'o', Cave1),
      Count1 is Count0 + 1,
      ((X, Y) = (500, 0) -> Cave = Cave1, Count = Count1
      ; drop_sand(Mode, MaxY, (500, 0), Cave1, Cave, Count1, Count))).

pos_empty(p1, _, Cave, Pos) :- \+ rb_lookup(Pos, _, Cave).
pos_empty(p2, MaxY, Cave, Pos) :-
    pos_empty(p1, MaxY, Cave, Pos),
    Pos = (_, Y),
    Y < MaxY + 2.

write_cave(Cave) :-
    extrema(Cave, (Lx, Ly), (Hx, Hy)),
    forall(between(Ly, Hy, Y),
           (forall(between(Lx, Hx, X),
                   ((rb_lookup((X, Y), Vl, Cave) -> true ; Vl = '.'),
                    write(Vl))),
            nl)).
extrema(Cave, Lo, Hi) :-
    rb_visit(Cave, Kvs),
    phrase(extrema((999, 999), (0, 0), Lo, Hi), Kvs).
extrema(L, H, L, H) --> eos.
extrema((Lx0, Ly0), (Hx0, Hy0), Lo, Hi) -->
    [(X, Y)-_],
    { Lx is min(Lx0, X), Ly is min(Ly0, Y),
      Hx is max(Hx0, X), Hy is max(Hy0, Y) },
    extrema((Lx, Ly), (Hx, Hy), Lo, Hi).

cave(Cave) --> { rb_empty(Cave0) }, cave(Cave0, Cave).

cave(Cave, Cave) --> eos.
cave(Cave0, Cave) --> path(Path), { draw(Path, Cave0, Cave1) }, cave(Cave1, Cave).

draw([_], Cave, Cave).
draw([S, E | Tl], Cave0, Cave) :-
    (bagof(Pos, inrange((S, E), Pos), Posns) -> true
    ; bagof(Pos, inrange((E, S), Pos), Posns)),
    foldl(drawpos, Posns, Cave0, Cave1),
    draw([E | Tl], Cave1, Cave).
drawpos(Pos, Cave0, Cave) :- rb_insert(Cave0, Pos, '#', Cave).

path([(X, Y) | Tl]) -->
    integer(X), `,`, integer(Y),
    (` -> ` -> path(Tl)
    ; eol, { Tl = [] }).
