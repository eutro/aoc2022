:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(grid(Grid0), current_input),
    step_state(Grid0, 0, 10, Grid1, 10), % see day23.c
    count_empty(Grid1, P1),
    writeln(P1),
    step_state(Grid1, 10, 9999, _, P2),
    writeln(P2).

count_empty(Grid, Empty) :-
    functor(Grid, _, Filled),
    grid_extrema(Grid, Lo, Hi),
    boundslen((Lo, Hi), All),
    Empty is All - Filled.

grid_extrema(Grid, Lo, Hi) :- Grid =.. [_ | Posns], extrema(Posns, Lo, Hi).
extrema(Posns, Lo, Hi) :- phrase(extrema(Lo, Hi), Posns).
extrema(L, H) --> [Pos], extrema(Pos, Pos, L, H).
extrema(L, H, L, H) --> eos, !.
extrema((Lx0, Ly0), (Hx0, Hy0), Lo, Hi) -->
    [(X, Y)],
    { Lx is min(Lx0, X), Ly is min(Ly0, Y),
      Hx is max(Hx0, X), Hy is max(Hy0, Y) },
    extrema((Lx, Ly), (Hx, Hy), Lo, Hi).

grid(Grid) --> grid0(1, Grid0), { Grid =.. [g | Grid0] }.
grid0(_, []) --> eos, !.
grid0(Y, Row) --> row(1, Y, Tl, Row), { Y1 is Y + 1 }, grid0(Y1, Tl).
row(_, _, Tl, Tl) --> eol, !.
row(X, Y, Tl, Row) --> `.`, !, { X1 is X + 1 }, row(X1, Y, Tl, Row).
row(X, Y, Tl, [(X, Y) | Row]) --> `#`, { X1 is X + 1 }, row(X1, Y, Tl, Row).
