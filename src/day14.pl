:- use_module(library(dcg/basics)).
:- use_module(util).

main :-
    phrase_from_stream(cave(Cave), current_input),
    drop_sand(s(P1, P2), Cave),
    maplist(writeln, [P1, P2]).

drop_sand(Mode, Cave) :- drop_sand([(500, 0)], Mode, Cave, 0).
drop_sand([], s(_, Count), _, Count) :- !.
drop_sand(Trail, Mode, Cave, Count0) :-
    [(X, Y) | Prev] = Trail,
    Y1 is Y + 1,
    (member(Dx, [0, -1, 1]),
     X1 is X + Dx,
     Pos = (X1, Y1),
     pos_empty(Mode, Count0, Cave, Pos), !
    -> drop_sand([Pos | Trail], Mode, Cave, Count0)
    ; caveref(Cave, (X, Y), 'o'),
      Count1 is Count0 + 1,
      drop_sand(Prev, Mode, Cave, Count1)).

pos_empty(s(P1, _), Count, Cave, Pos) :-
    caveref(Cave, Pos, Vl),
    var(Vl),
    Pos = (_, Y),
    Cave = c((_, (_, MaxY)), _),
    (Y = MaxY, var(P1) -> P1 = Count ; true).

caveref(c(Bounds, Arr), Pos, Vl) :-
    index(Bounds, Pos, I),
    arg(I, Arr, Vl).

cave(Cave) -->
    paths(Paths),
    { append(Paths, Ends),
      extrema(Ends, (MinX0, _), (MaxX0, MaxY0)),
      MaxY is MaxY0 + 1,
      MinX is MinX0 - MaxY,
      MaxX is MaxX0 + MaxY,
      Bounds = ((MinX, 0), (MaxX, MaxY)),
      boundslen(Bounds, Len),
      functor(Arr, a, Len),
      Cave = c(Bounds, Arr),
      maplist(plotpath(Cave), Paths) }.

plotpath(_, [_]).
plotpath(Cave, [S, E | Tl]) :-
    (bagof(Pos, inrange((S, E), Pos), Posns) -> true
    ; bagof(Pos, inrange((E, S), Pos), Posns)),
    maplist(plotpos(Cave), Posns),
    plotpath(Cave, [E | Tl]).
plotpos(Cave, Pos) :- caveref(Cave, Pos, '#').

paths([]) --> eos, !.
paths([Path | Tl]) --> path(Path), paths(Tl).

path([(X, Y) | Tl]) -->
    integer(X), `,`, integer(Y),
    (` -> ` -> path(Tl) ; eol, { Tl = [] }).

extrema(Posns, Lo, Hi) :- phrase(extrema(Lo, Hi), Posns).
extrema(L, H) --> [Pos], extrema(Pos, Pos, L, H).
extrema(L, H, L, H) --> eos, !.
extrema((Lx0, Ly0), (Hx0, Hy0), Lo, Hi) -->
    [(X, Y)],
    { Lx is min(Lx0, X), Ly is min(Ly0, Y),
      Hx is max(Hx0, X), Hy is max(Hy0, Y) },
    extrema((Lx, Ly), (Hx, Hy), Lo, Hi).

write_cave(Cave) :-
    Cave = c(((MinX, MinY), (MaxX, MaxY)), _),
    forall(between(MinY, MaxY, Y),
           (forall(between(MinX, MaxX, X),
                   (caveref(Cave, (X, Y), Vl0),
                    (var(Vl0) -> Vl = '.' ; Vl = Vl0),
                    write(Vl))),
            nl)).
