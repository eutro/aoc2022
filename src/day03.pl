:- use_module(library(dcg/basics)).
:- use_module(library(ordsets)).

main :-
    phrase_from_stream(parse_input(Input), current_input),
    sum_common(Input, P1),
    write(P1),nl,!,
    group_badges(Input, P2),
    write(P2),nl.

sum_common(Ls, R) :- sum_common(Ls, 0, R).
sum_common([[Lhs, Rhs, _] | Tl], A, R) :-
    ord_intersection(Lhs, Rhs, Common),
    sum_list(Common, X),
    A1 is A + X,
    sum_common(Tl, A1, R).
sum_common([], A, A).

group_badges(Ls, R) :- group_badges(Ls, 0, R).
group_badges([[_, _, A], [_, _, B], [_, _, C] | Tl], Acc, R) :-
    ord_intersection(A, B, AB),
    ord_intersection(AB, C, ABC),
    [Badge] = ABC,
    Acc1 is Acc + Badge,
    group_badges(Tl, Acc1, R).
group_badges([], A, A).

split(Ls, [], Ls, 0).
split([X|Ls], [X|Lhs], Rhs, N) :- R is N - 1, split(Ls, Lhs, Rhs, R).

parse_input([]) --> eos.
parse_input([[Lhs, Rhs, All] | R]) -->
    string(Cs), eol, !,
    { length(Cs, L),
      maplist(priority, Cs, S),
      split(S, Lh1, Rh1, L / 2),
      sort(S, All),
      sort(Lh1, Lhs),
      sort(Rh1, Rhs) },
    parse_input(R).

priority(C, R) :-
    [Ac, Zc] = `az`,
    (Ac =< C, C =< Zc, ! -> R is C - Ac + 1)
    ; [Abc, Zbc] = `AZ`,
      Abc =< C, C =< Zbc,
      R is C - Abc + 27.
