:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(snafus(Snafus), current_input),
    maplist(snafu_number, Snafus, Numbers),
    sum(Numbers, #=, Sum),
    snafu_number(SnafuSum, Sum),
    format("~s~n", [SnafuSum]).

snafu_number(Snafu, Number) :-
    reverse(Snafu, Rsnafu),
    rsnafu_number(Rsnafu, Number), !.
rsnafu_number([], 0).
rsnafu_number([Digit | Tl], N) :-
    snafu_digit(D, Digit),
    D #= N mod 5,
    N1 #= (N + 2) div 5,
    rsnafu_number(Tl, N1).
snafu_digit(D, Digit) :- nth0(D, `012=-`, Digit).

snafus([]) --> eos, !.
snafus([Snafu | Tl]) --> string_without(`\n`, Snafu), eol, snafus(Tl).
