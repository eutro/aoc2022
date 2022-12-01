main :-
    stream_to_lazy_list(current_input, InCodes),
    lazy_list_materialize(InCodes),
    parse_elves(InCodes, Elves),
    maplist(sum_list, Elves, ElvesSum),
    sort(ElvesSum, ElvesSortR),
    reverse(ElvesSortR, ElvesSorted),
    [P1|_] = ElvesSorted,
    write(P1),nl,
    [A,B,C|_] = ElvesSorted,
    P2 is A + B + C,
    write(P2),nl.

split_elves(Codes, [ElfRaw | RestElves]) :- append(ElfRaw, [10, 10 | Rest], Codes), !, split_elves(Rest, RestElves).
split_elves(Codes, [Codes]).

parse_elves(Codes, Elves) :-
    split_elves(Codes, ElvesRaw),
    maplist(parse_elf, ElvesRaw, Elves).

parse_elf(Raw, Foods) :-
    string_codes(Raw, Str),
    split_string(Str, "\n", "\n", FoodsRaw),
    maplist(number_string, Foods, FoodsRaw).
