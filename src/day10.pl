:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(insns(Insns), current_input),
    exec(Insns, Xs),
    %writeln(Xs),
    sigstren(Xs, P1),
    writeln(P1),
    draw(Xs, P2),
    write(P2).

draw(Xs, Img) :-
    draw(0, Xs, Flat),
    unflatten(Flat, Codes),
    string_codes(Img, Codes).

draw(I, [X | Xs], [L | Tl]) :-
    !,
    (abs(I mod 40 - X) #=< 1
    -> L = 0'#
    ; L = 0'.),
    I1 #= I + 1,
    draw(I1, Xs, Tl).
draw(_, [], []).

unflatten(Flat, Codes) :-
    length(Lhs, 40),
    append(Lhs, Rhs, Flat), !,
    append(Lhs, [0'\n | Rhs1], Codes),
    unflatten(Rhs, Rhs1).
unflatten(Rem, Rem).

split1(I, Xs, X, Tl) :-
    I1 #= I - 1,
    length(Lhs, I1),
    append(Lhs, Rhs, Xs),
    [X | Tl] = Rhs.

sigstren(Xs, R) :-
    split1(20, Xs, X, Tl), !,
    R0 is 20 * X,
    ss(60, Tl, R0, R).
sigstren(_, 0).

ss(I, Xs, R0, R) :-
    I =< 220,
    split1(40, Xs, X, Tl), !,
    %writeln(I-X),
    S0 is I * X,
    R1 is R0 + S0,
    I1 is I + 40,
    ss(I1, Tl, R1, R).
ss(_, _, R, R).

exec(Is, Xs) => exec(Is, 1, Xs).

exec([noop | Is], R, [R | Xs]) :- !, exec(Is, R, Xs).
exec([addx(X) | Is], R, [R, R | Xs]) :-
    !,
    R1 #= R + X,
    exec(Is, R1, Xs).
exec([], _, []) :- !.

insns([]) --> eos.
insns([Insn | Insns]) --> insn(Insn), eol, insns(Insns).

insn(noop) --> `noop`.
insn(addx(N)) --> `addx `, integer(N).
