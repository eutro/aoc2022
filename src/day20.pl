:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    %spy(mix/3),
    phrase_from_stream(nums(Nums), current_input),
    crypt(1, Nums, P1),
    writeln(P1),
    maplist([X, X1]>>(X1 is X * 811589153), Nums, Nums1),
    crypt(10, Nums1, P2),
    writeln(P2),
    true.

crypt(Times, Nums, Ans) :-
    list_to_ll(Nums, Ll),
    mixtimes(Times, Ll),
    valof(Ll, Zero, 0), !,
    step(full, Ll, 1000, Zero, U),
    step(full, Ll, 1000, U, V),
    step(full, Ll, 1000, V, W),
    maplist(valof(Ll), [U, V, W], Vals),
    sum(Vals, #=, Sum),
    Ans = sum(Vals, Sum).

valof(Ll, I, X) :- arg(I, Ll, n(X, _, _)).

write_ll(Ll, O) :-
    valof(Ll, O, V),
    write(V),
    step(full, Ll, 1, O, O1),
    write_ll(Ll, O, O1).
write_ll(_, End, End) :- !, nl.
write_ll(Ll, End, O) :-
    write(", "),
    valof(Ll, O, V),
    write(V),
    step(full, Ll, 1, O, O1),
    write_ll(Ll, End, O1).

mixtimes(0, _) :- !.
mixtimes(N, Ll) :- mix(Ll), N1 is N - 1, mixtimes(N1, Ll).
mix(Ll) :- foreach(arg(I, Ll, n(X, _, _)), mix(Ll, I, X)).
mix(Ll, I, X) :-
    %write_ll(Ll, 6),
    (disconnect(Ll, I),
     step(partial, Ll, X, I, Prev)
    -> stepby(Ll, 1, Prev, Next),
       connect(Ll, I, Prev, Next)
    ; true).

step(Mode, Ll, N0, Idx0, Idx) :-
    reduce(Mode, Ll, N0, N),
    N \= 0,
    stepby(Ll, N, Idx0, Idx).

reduce(Mode, Ll, N0, N) :-
    functor(Ll, _, Len0),
    (Mode = full -> Len = Len0 ; Len is Len0 - 1),
    N is N0 mod Len.

stepby(_, 0, Idx, Idx) :- !.
stepby(Ll, N, Idx0, Idx) :-
    arg(Idx0, Ll, n(_, _, Idx1)),
    N1 is N - 1,
    stepby(Ll, N1, Idx1, Idx).

connect(Ll, Idx, Prev, Next) :-
    arg(Idx, Ll, Node),
    arg(Prev, Ll, PrevNode),
    arg(Next, Ll, NextNode),
    setarg(2, NextNode, Idx),
    setarg(3, PrevNode, Idx),
    setarg(2, Node, Prev),
    setarg(3, Node, Next).

disconnect(Ll, Idx) :-
    arg(Idx, Ll, n(_, Prev, Next)),
    arg(Prev, Ll, PrevNode),
    arg(Next, Ll, NextNode),
    setarg(2, NextNode, Prev),
    setarg(3, PrevNode, Next).

list_to_ll(Xs, Ll) :-
    length(Xs, Len),
    findall(
        n(X, Prev, Next),
        (nth1(I, Xs, X),
         Prev is (Len + I - 2) mod Len + 1,
         Next is I mod Len + 1),
        Nodes
    ),
    Ll =.. [ll | Nodes].

nums([]) --> eos, !.
nums([Num | Tl]) --> integer(Num), eol, nums(Tl).
