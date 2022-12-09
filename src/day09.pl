:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(insns(Insns), current_input),
    flatten_moves(Insns, Flat),
    forall(member(Len, [2, 10]),
           (track(Flat, Len, Ans),
            writeln(Ans))).

track(Flat, I, Unique) :-
    phrase(run(I, Trail), Flat),
    sort(Trail, Visited),
    length(Visited, Unique).

run(I, Vs) -->
    { length(Rope, I), maplist(=((0,0)), Rope) },
    run(I, Rope, [(0, 0)], Vs).
run(_, _, Vs, Vs) --> eos.
run(I, Rope, Vs0, Vs) -->
    [mv(Dir)],
    { move_rope(Rope, Dir, Rope1, Vs0, Vs1) },
    run(I, Rope1, Vs1, Vs).

mvby((X1, Y1), (DX, DY), (X2, Y2)) :-
    X2 #= X1 + DX,
    Y2 #= Y1 + DY.

move_rope([H | Tl], Dir, [H1 | Tl1], Vs0, Vs1) :-
    move(H, Dir, H1),
    move_tl(H1, Tl, Tl1, Vs0, Vs1).

move_tl(H1, [X | Tl], [X | Tl], Vs, Vs) :- touching(X, H1), !.
move_tl(H1, [X], [X1], Vs0, [X1 | Vs0]) :- !, moveto(X, H1, X1).
move_tl(H1, [X | Tl], [X1 | Tl1], Vs0, Vs) :- !, moveto(X, H1, X1), move_tl(X1, Tl, Tl1, Vs0, Vs).
move_tl(_, [], [], Vs, Vs) :- !.

moveto(X, H, X1) :-
    mvby(X, (DX, DY), H),
    DX1 is sign(DX),
    DY1 is sign(DY),
    mvby(X, (DX1, DY1), X1).

touching((X1, Y1), (X2, Y2)) :-
    abs(X1 - X2) =< 1,
    abs(Y1 - Y2) =< 1.

move((X, Y), up, (X, Y1)) :- !, Y1 #= Y - 1.
move((X, Y), down, (X, Y1)) :- !, Y1 #= Y + 1.
move((X, Y), left, (X1, Y)) :- !, X1 #= X - 1.
move((X, Y), right, (X1, Y)) :- !, X1 #= X + 1.

flatten_moves([], []).
flatten_moves([mv(Dir, 1) | Tl], [mv(Dir) | Fl]) :- !, flatten_moves(Tl, Fl).
flatten_moves([mv(Dir, N) | Tl], [mv(Dir) | Fl]) :-
    N1 #= N - 1,
    flatten_moves([mv(Dir, N1) | Tl], Fl).

insns([]) --> eos.
insns([mv(Dir, By) | Tl]) --> dir(Dir), ` `, integer(By), eol, insns(Tl).

dir(up) --> `U`.
dir(down) --> `D`.
dir(left) --> `L`.
dir(right) --> `R`.
