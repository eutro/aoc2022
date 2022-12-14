:- use_module(library(dcg/basics)).
:- use_module(util).

main :-
    phrase_from_stream(cave(Cave), current_input),
    drop_sand(s(P1, P2), Cave),
    maplist(writeln, [P1, P2]).

drop_sand(Mode, Cave0) :-
    extrema(Cave0, _, (_, MaxY)),
    drop_sand([(500, 0)], Mode, MaxY, Cave0, 0).

drop_sand([], s(_, Count), _, _, Count) :- !.
drop_sand(Trail, Mode, MaxY, Cave0, Count0) :-
    [(X, Y) | Prev] = Trail,
    Y1 is Y + 1,
    (member(Dx, [0, -1, 1]),
     X1 is X + Dx,
     Pos = (X1, Y1),
     pos_empty(Mode, Count0, MaxY, Cave0, Pos), !
    -> drop_sand([Pos | Trail], Mode, MaxY, Cave0, Count0)
    ; rb_insert(Cave0, (X, Y), 'o', Cave1),
      Count1 is Count0 + 1,
      drop_sand(Prev, Mode, MaxY, Cave1, Count1)).

pos_empty(s(P1, _), Count, MaxY, Cave, Pos) :-
    \+ rb_lookup(Pos, _, Cave),
    Pos = (_, Y),
    (Y > MaxY
    -> (var(P1) -> P1 = Count ; true),
       Y < MaxY + 2
    ; true).

cave(Cave) --> { rb_empty(Cave0) }, cave(Cave0, Cave).

cave(Cave, Cave) --> eos, !.
cave(Cave0, Cave) --> path(Path), { draw(Path, Cave0, Cave1) }, cave(Cave1, Cave).

extrema(Cave, Lo, Hi) :- rb_keys(Cave, Keys), phrase(extrema(Lo, Hi), Keys).
extrema(L, H) --> [Pos], extrema(Pos, Pos, L, H).
extrema(L, H, L, H) --> eos, !.
extrema((Lx0, Ly0), (Hx0, Hy0), Lo, Hi) -->
    [(X, Y)],
    { Lx is min(Lx0, X), Ly is min(Ly0, Y),
      Hx is max(Hx0, X), Hy is max(Hy0, Y) },
    extrema((Lx, Ly), (Hx, Hy), Lo, Hi).

draw([_], Cave, Cave) :- !.
draw([S, E | Tl], Cave0, Cave) :-
    (bagof(Pos, inrange((S, E), Pos), Posns) -> true
    ; bagof(Pos, inrange((E, S), Pos), Posns)),
    foldl(drawpos, Posns, Cave0, Cave1),
    draw([E | Tl], Cave1, Cave).
drawpos(Pos, Cave0, Cave) :- rb_insert(Cave0, Pos, '#', Cave).

path([(X, Y) | Tl]) -->
    integer(X), `,`, integer(Y),
    (` -> ` -> path(Tl) ; eol, { Tl = [] }).

write_cave(Cave) :-
    extrema(Cave, (Lx, Ly), (Hx, Hy)),
    forall(between(Ly, Hy, Y),
           (forall(between(Lx, Hx, X),
                   ((rb_lookup((X, Y), Vl, Cave) -> true ; Vl = '.'),
                    write(Vl))),
            nl)).
