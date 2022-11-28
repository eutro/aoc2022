main :-
    loop(X, Y),
    write(X),
    write(", "),
    write(Y),
    write("\n").

loop(R, T) :-
    get_char(C),
    process(C, 0, 0, R, none, T).

process(end_of_file, _, A, A, B, B).
process(C, I, A, R, G, T) :-
    step(C, A, B),
    step2(A, I, G, H),
    get_char(C1),
    J is I + 1,
    process(C1, J, B, R, H, T).

step2(-1, I, none, I).
step2(_, _, X, X).

step('(', A, R) :- R is A + 1.
step(')', A, R) :- R is A - 1.
step(_, A, A).
