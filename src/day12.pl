:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(grid(Grid, S, E, W, H), current_input),
    table(W, H, Table),
    bfs(Grid, E, Table),
    forall(member(Goal, [gridref(Table, S, Ans),
                         setof(Len, path(W, H, Grid, Table, Len), [Ans | _])]),
           (call(Goal), writeln(Ans))).

path(W, H, Grid, Table, Len) :-
    inrange(((1, 1), (W, H)), Pos),
    gridref(Grid, Pos, 0),
    gridref(Table, Pos, Len),
    integer(Len).

table(W, H, Table) :-
    length(Rows, H),
    maplist(is_row(W), Rows),
    Table =.. [c | Rows].
is_row(W, Row) :- functor(Row, r, W).

bfs(Grid, E, Table) :-
    gridref(Table, E, 0),
    bfs0(Grid, q([], [[E]]), Table).

bfs0(_, Q, _) :- q_empty(Q), !.
bfs0(Grid, Q0, Table) :-
    q_pop(Q0, Path0, Q1),
    foldl(mv(Grid, Path0, Table), [up, down, left, right], Q1, Q),
    bfs0(Grid, Q, Table).

mv(Grid, Path0, Table, Dir, Q0, Q) :-
    [Pos0 | _] = Path0,
    move(Pos0, Dir, Pos),
    gridref(Table, Pos, Len),
    var(Len),
    length(Path0, Len),
    gridref(Grid, Pos0, Height0),
    gridref(Grid, Pos, Height),
    can_move(Height, Height0), !,
    q_push(Q0, [Pos | Path0], Q).
mv(_, _, _, _, Q, Q).

can_move(From, To) :- To =< From + 1.

q_empty(q([], [])).
q_push(q(F, B), X, q([X | F], B)).
q_pop(q(F, [X | B]), X, q(F, B)) :- !.
q_pop(q(F, []), X, q([], F1)) :- reverse(F, [X | F1]).

grid(Grid, S, E, W, H) --> grid(Grid0, 1, S, E, W, H), { Grid =.. [c | Grid0] }.

grid([], Y, _, _, _, H) --> eos, !, { H is Y - 1 }.
grid([Row | Grid], Y, S, E, W, H) -->
    string_without(`\n`, Line), eol,
    { length(Line, W),
      maplist(height, Line, Row0),
      Row =.. [r | Row0],
      (nth1(Sx, Line, 0'S) -> S = (Sx, Y) ; true),
      (nth1(Ex, Line, 0'E) -> E = (Ex, Y) ; true),
      Y1 is Y + 1 },
    grid(Grid, Y1, S, E, W, H).

height(0'S, H) => height(0'a, H).
height(0'E, H) => height(0'z, H).
height(X, H) => 0'a #=< X, X #=< 0'z, H #= X - 0'a.

gridref(Grid, (X, Y), V) :- arg(Y, Grid, Row), arg(X, Row, V).

move((X, Y), up, (X, Y1)) :- !, Y1 #= Y - 1.
move((X, Y), down, (X, Y1)) :- !, Y1 #= Y + 1.
move((X, Y), left, (X1, Y)) :- !, X1 #= X - 1.
move((X, Y), right, (X1, Y)) :- !, X1 #= X + 1.
