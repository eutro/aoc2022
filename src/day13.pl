:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(packets(Packets), current_input),
    bagof(I, in_order(Packets, I), Is),
    sum_list(Is, P1),
    writeln(P1),
    predsort(cmp, [[[2]], [[6]] | Packets], Sorted),
    length(Packets, Plen),
    length(Sorted, Slen),
    Slen #= Plen + 2,
    nth1(A, Sorted, [[2]]),
    nth1(B, Sorted, [[6]]),
    P2 #= A * B,
    writeln(P2),
    true.

in_order(Packets, I) :-
    0 #= I0 mod 2,
    I1 #= I0 + 1,
    I #= I0 div 2 + 1,
    nth0(I0, Packets, Lhs),
    nth0(I1, Packets, Rhs),
    cmp(O, Lhs, Rhs),
    memberchk(O, [(<), (=)]).

cmp(O, X, Y), (integer(X), integer(Y)) => compare(O, X, Y).
cmp(O, [], [_|_]) => O = (<).
cmp(O, [_|_], []) => O = (>).
cmp(O, [], []) => O = (=).
cmp(O, [L|Ls], [R|Rs]) => cmp(O0, L, R), (O0 = (=) -> cmp(O, Ls, Rs) ; O = O0).
cmp(O, X, Y), (integer(X), list(Y)) => cmp(O, [X], Y).
cmp(O, X, Y), (list(X), integer(Y)) => cmp(O, X, [Y]).

list([]).
list([_|_]).

packets([]) --> eos.
packets([Lhs, Rhs | Tl]) --> packet(Lhs), packet(Rhs), (eol -> packets(Tl) ; Tl = []).

packet(Packet) --> string_without(`\n`, Codes), eol, { string_codes(Str, Codes), term_string(Packet, Str) }.
