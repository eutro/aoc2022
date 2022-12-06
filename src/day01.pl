:- use_module(library(dcg/basics)).

main :-
    phrase_from_stream(elves(Elves), current_input),
    maplist(sum_list, Elves, ElvesSum),
    msort(ElvesSum, ElvesSorted),
    forall(member(N, [1, 3]),
           (length(Top, N),
            append(_, Top, ElvesSorted),
            sum_list(Top, Ans),
            writeln(Ans))).

elves([]) --> eos.
elves([Elf | Tail]) --> elf(Elf), elves(Tail).

elf([]) --> eol.
elf([Food | Tail]) --> integer(Food), eol, elf(Tail).
