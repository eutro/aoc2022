:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(packets(Packets), current_input),
    forall(member(Goal, [phrase(in_order(Ans), Packets),
                         decoder_key(Packets, Ans)]),
           (call(Goal), writeln(Ans))).

decoder_key(Packets, Key) :-
    Dividers = [[[2]], [[6]]],
    append(Dividers, Packets, DPackets),
    predsort(cmp, DPackets, Sorted),
    same_length(DPackets, Sorted),
    maplist(find_packet(Sorted), Dividers, [A, B]),
    Key #= A * B.
find_packet(Haystack, Needle, I) :- nth1(I, Haystack, Needle), !.

in_order(Is) --> in_order(1, Is).
in_order(_, 0) --> eos, !.
in_order(I, Is) -->
    [Lhs, Rhs],
    { (cmp((>), Lhs, Rhs) -> Is = Is0 ; Is #= I + Is0),
      I1 #= I + 1 },
    in_order(I1, Is0).

cmp(O, X, Y), (integer(X), integer(Y)) => compare(O, X, Y).
cmp(O, [], [_|_]) => O = (<).
cmp(O, [_|_], []) => O = (>).
cmp(O, [], []) => O = (=).
cmp(O, [L|Ls], [R|Rs]) => cmp(O0, L, R), (O0 = (=) -> cmp(O, Ls, Rs) ; O = O0).
cmp(O, X, Y), (integer(X), list(Y)) => cmp(O, [X], Y).
cmp(O, X, Y), (list(X), integer(Y)) => cmp(O, X, [Y]).

list([]).
list([_|_]).

packets([]) --> eos, !.
packets([Lhs, Rhs | Tl]) --> packet(Lhs), packet(Rhs), (eol -> packets(Tl) ; Tl = []).

packet(Packet) --> string_without(`\n`, Codes), eol, { string_codes(Str, Codes), term_string(Packet, Str) }.
