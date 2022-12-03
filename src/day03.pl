:- use_module(library(dcg/basics)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).

main :-
    stream_to_lazy_list(current_input, InCodes),
    lazy_list_materialize(InCodes),
    phrase(parse_input(Input), InCodes),
    maplist(sum_common, Input, Prios),
    sum_list(Prios, P1),
    write(P1),nl,
    !,
    groups(Input, Groups),
    maplist(group_badge, Groups, Badges),
    maplist(priority, Badges, BadgePrios),
    sum_list(BadgePrios, BadgeSum),
    write(BadgeSum),nl.

group_badge([[_,_,A], [_,_,B], [_,_,C]], Badge) :-
    ord_intersection(A, B, AB),
    ord_intersection(AB, C, ABC),
    [Badge] = ABC.

groups([A, B, C | InR], [[A, B, C] | GrR]) :- groups(InR, GrR).
groups([], []).

sum_common([Lhs, Rhs, _], R) :-
    ord_intersection(Lhs, Rhs, Common),
    %string_codes(CStr, Common),write(CStr),nl,
    maplist(priority, Common, Prios),
    sum_list(Prios, R).

split(Ls, [], Ls, 0).
split([X|Ls], [X|Lhs], Rhs, N) :- R is N - 1, split(Ls, Lhs, Rhs, R).

parse_input([]) --> eos.
parse_input([[Lhs, Rhs, All] | R]) -->
    string(S), eol, !,
    { length(S, L),
      HL is L / 2,
      split(S, Lh1, Rh1, HL),
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
