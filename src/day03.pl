:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(ordsets)).

main :-
    phrase_from_stream(bags(Bags), current_input),
    forall(member(Mode, [halves, groups]),
           (sum_mode(Mode, Bags, Ans),
            writeln(Ans))).

sum_mode(Mode, Ls, Total) :- sum_mode(Mode, Ls, 0, Total).
sum_mode(_, [], Acc, Acc).
sum_mode(Mode, Ls, Acc0, Total) :-
    step_mode(Mode, Ls, Tail, X),
    Acc1 #= Acc0 + X,
    sum_mode(Mode, Tail, Acc1, Total).

step_mode(halves, [Bag | Tail], Tail, X) :-
    length(Bag, L),
    L #= HL + HL,
    length(Lhs, HL),
    append(Lhs, Rhs, Bag),
    sort(Lhs, LhsS), sort(Rhs, RhsS),
    ord_intersection(LhsS, RhsS, [X]).

step_mode(groups, [A, B, C | Tail], Tail, X) :-
    sort(A, AS), sort(B, BS), sort(C, CS),
    ord_intersection(AS, BS, AB),
    ord_intersection(AB, CS, [X]).

bags([]) --> eos.
bags([S | R]) -->
    string(Cs), eol, !, bags(R),
    { maplist(priority, Cs, S) }.

priority(C, R) :- 0'a =< C, C =< 0'z, R #= C - 0'a + 1.
priority(C, R) :- 0'A =< C, C =< 0'Z, R #= C - 0'A + 27.
