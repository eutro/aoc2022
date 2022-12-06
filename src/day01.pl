:- use_module(library(dcg/basics)).
:- use_module(util).

main :-
    phrase_from_stream(elves(Elves), current_input),
    maplist(sum_list, Elves, ElvesSum),
    msort(ElvesSum, ElvesSorted),
    forall(member(N, [1, 3]),
           (length(Top, N),
            append(_, Top, ElvesSorted),
            sum_list(Top, Ans),
            writeln(Ans))).

elves(Elves) --> seqof(elf, [], eos, Elves).
elf(Elf) --> seqof1(integer, eol, eol, Elf).
