:- use_module(library(dcg/basics)).

main :-
    phrase_from_stream(parse_input(Input), current_input),
    count(has_subs, Input, P1),
    write(P1),nl,!,
    count(has_intr, Input, P2),
    write(P2),nl.

count(Goal, Ls, R) :-
    include(Goal, Ls, Cls),
    length(Cls, R).

has_subs([Llo, Lhi, Rlo, Rhi]) :-
    (forall(between(Llo, Lhi, X), between(Rlo, Rhi, X))
    ; forall(between(Rlo, Rhi, X), between(Llo, Lhi, X))).
has_intr([Llo, Lhi, Rlo, Rhi]) :-
    between(Llo, Lhi, X), between(Rlo, Rhi, X).

parse_input([]) --> eos.
parse_input([[Llo, Lhi, Rlo, Rhi] | R]) -->
    integer(Llo), `-`, integer(Lhi),
    `,`,
    integer(Rlo), `-`, integer(Rhi),
    eol, !,
    parse_input(R).
