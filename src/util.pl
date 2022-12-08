:- module(util, [
              seqof//4,
              seqof1//4,
              string1//1,
              ntmap//3,
              print_chpoint/0,
              boundslen/2,
              inrange/2,
              index/3,
              list_array/2,
              array_bounds/2,
              arraylen/2,
              arrayref/3,
              arrayset/4
          ]).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

seqof(NonTerm, Suff, End, [Elem | Tail]) -->
    call(NonTerm, Elem), Suff, !,
    seqof(NonTerm, Suff, End, Tail).
seqof(_, _, End, []) --> End.

seqof1(NonTerm, Suff, End, [Elem | Tail]) -->
    call(NonTerm, Elem), Suff,
    seqof(NonTerm, Suff, End, Tail).

ntmap(Func, NonTerm, Res) -->
    call(NonTerm, X),
    { call(Func, X, Res) }.

string1([C|Cs]) --> [C], string(Cs).

print_chpoint :-
   prolog_current_choice(ChI),
   prolog_choice_attribute(ChI, frame, F),
   prolog_frame_attribute(F, goal, Goal),
   format('Last choice point: ~w\n', [Goal]).

list_array(List, Array) :-
    length(List, Len),
    arraylen(Array, Len),
    arr(_, Raw) = Array, !,
    list_array0(1, List, Raw).

list_array0(_, [], _) :- !.
list_array0(I, [X | Tl], Raw) :-
    arrayref0(Raw, I, X),
    I1 is I + 1,
    list_array0(I1, Tl, Raw).

boundslen((A, B), Len) :-
    (integer(A), integer(B) ; integer(A), var(B) ; var(A), integer(A)), !,
    Len #= B - A + 1.
boundslen((A, B), Len) :-
    index((A, B), A, I0),
    index((A, B), B, I1),
    Len #= I1 - I0 + 1.

inrange((A, B), Idx) :- index((A, B), Idx, _).

index((A, B), Idx, Int) :-
    (integer(A), integer(B) ; integer(A), var(B) ; var(A), integer(A)), !,
    between(A, B, Idx),
    Int #= Idx - A + 1.

index(((X1, Y1), (X2, Y2)), (X, Y), Int) :-
    index((X1, X2), X, Horiz),
    index((Y1, Y2), Y, Vert),
    boundslen((Y1, Y2), Height),
    Int #= (Horiz - 1) * Height + Vert.

array_bounds(arr(Bounds, _), Bounds).

arrayref(arr(Bounds, Raw), Idx, Val) :-
    index(Bounds, Idx, N),
    arrayref0(Raw, N, Val).

arrayset(arr(Bounds, Raw), Idx, Val, arr(Bounds, OutRaw)) :-
    index(Bounds, Idx, N),
    arrayset0(Raw, N, Val, OutRaw).

arrayref0(t(A, _), N, X) :-
    N =< 32, !,
    functor(A, l, 32),
    arg(N, A, X).
arrayref0(t(_, A), N, X) :-
    N1 is N div 32,
    I is N mod 32 + 1,
    functor(A, c, 32),
    arg(I, A, K),
    arrayref0(K, N1, X).

arrayset0(t(A, C), N, X, t(A1, C)) :-
    N =< 32, !,
    copy_term(A, A1),
    setarg(N, A, X).
arrayset0(t(V, A), N, X, t(V, A1)) :-
    N1 is N div 32,
    I is N mod 32 + 1,
    arg(I, A, K),
    arrayset0(K, N1, X, K1),
    copy_term(A, A1),
    setarg(I, A1, K1).

arraylen(arr(Bounds, _), Len) :- boundslen(Bounds, Len).
