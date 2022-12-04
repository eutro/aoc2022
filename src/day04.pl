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

has_subs([_, Lhi, _, Rhi]) :- Rhi =< Lhi.
has_intr([_, Lhi, Rlo, _]) :- Rlo =< Lhi.

parse_input([]) --> eos.
parse_input([[Llo, Lhi, Rlo, Rhi] | R]) -->
    integer(Alo), `-`, integer(Ahi),
    `,`,
    integer(Blo), `-`, integer(Bhi),
    eol, !,
    { (Alo = Blo -> ([Llo, Rlo] = [Alo, Blo], msort([Ahi, Bhi], [Rhi, Lhi])))
      ; (Alo < Blo -> [Llo, Lhi, Rlo, Rhi] = [Alo, Ahi, Blo, Bhi])
      ; [Llo, Lhi, Rlo, Rhi] = [Blo, Bhi, Alo, Ahi] },
    parse_input(R).
