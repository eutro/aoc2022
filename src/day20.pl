:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(nums(Nums), current_input),
    foreach((var(Ans), member(p(Times, Factor), [p(1, 1), p(10, 811589153)])),
            (crypt(Times, Factor, Nums, Ans),
             writeln(Ans))).

crypt(Times, Factor, Nums0, Ans) :-
    maplist(times(Factor), Nums0, Nums),
    Mixed0 =.. [f | Nums],
    mixtimes(Times, Mixed0, Mixed), % see day20.c
    argmod(1001, Mixed, U),
    argmod(2001, Mixed, V),
    argmod(3001, Mixed, W),
    Ans is U + V + W.

times(A, B, R) :- R #= A * B.

argmod(I0, Term, X) :-
    functor(Term, _, Len),
    I is (I0 - 1) mod Len + 1,
    arg(I, Term, X).

nums([]) --> eos, !.
nums([Num | Tl]) --> integer(Num), eol, nums(Tl).
