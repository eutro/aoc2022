:- use_module(library(dcg/basics)).

main :-
    phrase_from_stream(assignments(Assignments), current_input),
    forall(member(Goal, [has_subs, has_intr]),
           (count(Goal, Assignments, Ans),
            writeln(Ans))).

count(Goal, Ls, R) :- include(Goal, Ls, Cls), length(Cls, R).

has_subs(p(i(Llo, Lhi), i(Rlo, Rhi))) :- forall(between(Llo, Lhi, X), between(Rlo, Rhi, X)).
has_subs(p(i(Llo, Lhi), i(Rlo, Rhi))) :- forall(between(Rlo, Rhi, X), between(Llo, Lhi, X)).
has_intr(p(i(Llo, Lhi), i(Rlo, Rhi))) :- between(Llo, Lhi, X), between(Rlo, Rhi, X).

assignments([]) --> eos.
assignments([p(Lhs, Rhs) | Tail]) --> interval(Lhs), `,`, interval(Rhs), eol, assignments(Tail).
interval(i(Lo, Hi)) --> integer(Lo), `-`, integer(Hi).
