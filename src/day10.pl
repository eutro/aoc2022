:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(insns(Insns), current_input),
    exec(Insns, Xs),
    sigstren(Xs, P1),
    writeln(P1),
    draw(Xs, P2),
    writeln(P2).

draw(Xs, Img) :-
    draw(0, Xs, Flat),
    unflatten(Flat, Grid),
    grid_string(Grid, Img).

draw(_, [], []).
draw(I, [X | Xs], [L | Tl]) :-
    (abs(I mod 40 - X) #=< 1 -> L = 0'# ; L = 0'.),
    I1 #= I + 1,
    draw(I1, Xs, Tl).

unflatten(Flat, [Lhs | Tl]) :-
    length(Lhs, 40),
    append(Lhs, Rhs, Flat), !,
    unflatten(Rhs, Tl).
unflatten(_, []).

sigstren(Xs, R) :-
    T =.. [a | Xs],
    maplist(ss(T), [20, 60, 100, 140, 180, 220], Ss),
    sum(Ss, #=, R).
ss(T, N, R) :- arg(N, T, X), R #= N * X.

exec(Is, Xs) => scanl(plus, Is, 1, Xs).

insns([]) --> eos.
insns([0 | Insns]) --> `noop`, eol, insns(Insns).
insns([0, X | Insns]) --> `addx `, integer(X), eol, insns(Insns).
