:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(grid(Grid, S, E, _, _), current_input), !,
    bfs(Grid, E, Paths),
    rb_lookup(S, P1, Paths),
    writeln(P1),
    rb_visit(Paths, Kvs0),
    include(is_a(Grid), Kvs0, Kvs1),
    maplist([_-V, V]>>true, Kvs1, Kvs),
    msort(Kvs, [P2 | _]),
    writeln(P2),
    true.

is_a(Grid, Pos-_) :- gridref(Grid, Pos, 0).

bfs(Grid, E, Seen) :-
    rb_empty(Seen0),
    rb_insert_new(Seen0, E, 0, Seen1),
    bfs(Grid, q([], [[E]]), Seen1, Seen).

bfs(_, Q, Seen, Seen) :- q_empty(Q).
bfs(Grid, Q0, Seen0, Seen) :-
    q_pop(Q0, Path0, Q1),
    foldl(mv(Grid, Path0), [up, down, left, right], Q1-Seen0, Q-Seen1),
    bfs(Grid, Q, Seen1, Seen).

mv(Grid, Path0, Dir, Q0-Seen0, Q-Seen) :-
    [Pos0 | _] = Path0,
    move(Pos0, Dir, Pos),
    length(Path0, Len),
    rb_insert_new(Seen0, Pos, Len, Seen),
    gridref(Grid, Pos0, Height0),
    gridref(Grid, Pos, Height),
    can_move(Height, Height0), !,
    q_push(Q0, [Pos | Path0], Q).
mv(_, _, _, Q-S, Q-S).

can_move(From, To) :- To =< From + 1.

q_empty(q([], [])).
q_push(q(F, B), X, q([X | F], B)).
q_pop(q(F, [X | B]), X, q(F, B)) :- !.
q_pop(q(F, []), X, q([], F1)) :- reverse(F, [X | F1]).

grid(Grid, S, E, W, H) -->
    grid(Grid0, 1, S, E, W, H),
    { Grid =.. [c | Grid0] }.

grid([], Y, _, _, _, H) --> eos, { H is Y - 1 }.
grid([Row | Grid], Y, S, E, W, H) -->
    string_without(`\n`, Line), eol,
    { length(Line, W),
      maplist(height, Line, Row0),
      Row =.. [r | Row0],
      (nth1(Sx, Line, 0'S) -> S = (Sx, Y) ; true),
      (nth1(Ex, Line, 0'E) -> E = (Ex, Y) ; true),
      Y1 is Y + 1 },
    grid(Grid, Y1, S, E, W, H).

height(0'S, H) :- height(0'a, H).
height(0'E, H) :- height(0'z, H).
height(X, H) :- 0'a #=< X, X #=< 0'z, H #= X - 0'a.

gridref(Grid, (X, Y), V) :-
    arg(Y, Grid, Row),
    arg(X, Row, V).

move((X, Y), up, (X, Y1)) :- !, Y1 #= Y - 1.
move((X, Y), down, (X, Y1)) :- !, Y1 #= Y + 1.
move((X, Y), left, (X1, Y)) :- !, X1 #= X - 1.
move((X, Y), right, (X1, Y)) :- !, X1 #= X + 1.
