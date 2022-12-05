:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).

main :-
    phrase_from_stream((parse_crates(Crates), parse_insns(Insns)), current_input),
    run_and_look(Insns, Crates, cm9000, P1),
    write(P1),nl,
    run_and_look(Insns, Crates, cm9001, P2),
    write(P2),nl.

run_and_look(Insns, Crates, Machine, R) :-
    run_insns(Insns, Crates, Machine, CratesRun),
    maplist(first, CratesRun, Top),
    string_codes(R, Top).

first([A|_], A).

run_insns([], C, _, C).
run_insns([Insn | Rest], Crates, Machine, R) :-
    run_insn(Insn, Crates, Machine, C1),
    run_insns(Rest, C1, Machine, R).

run_insn([N, From, To], Crates, Machine, R) :-
    nth1(From, Crates, FStack),
    nth1(To, Crates, TStack),
    move(Machine, N, FStack, TStack, FStack1, TStack1),
    replace(Crates, From, FStack1, C1),
    replace(C1, To, TStack1, R).

replace(L, I, E, O) :-
    nth1(I, L, _, R),
    nth1(I, O, E, R).

move(cm9000, 0, F, T, F, T).
move(cm9000, N, [A|F], T, F1, T1) :-
    N1 #= N - 1,
    move(cm9000, N1, F, [A|T], F1, T1).

move(cm9001, N, F, T, F1, T1) :-
    length(Moved, N),
    append(Moved, F1, F),
    append(Moved, T, T1).

parse_insns([Insn | R]) --> parse_insn(Insn), parse_insns(R).
parse_insns([]) --> eos.

parse_insn([N, From, To]) -->
    `move `, integer(N), ` from `, integer(From), ` to `, integer(To), eol.

parse_single_crate(Crate) --> `[`, string([Crate]), `]`, !.
parse_single_crate(false) --> `   `, !.
parse_crate_row([Crate | R]) --> parse_single_crate(Crate), parse_crow_tail(R).
parse_crow_tail([]) --> eol.
parse_crow_tail(R) --> ` `, !, parse_crate_row(R).

parse_crate_rows([Row | Rest]) -->
    parse_crate_row(Row),
    parse_crate_rows(Rest).
parse_crate_rows([]) --> parse_last_row.

parse_last_row -->
    ` `, (digit(_) ; ` `), !, ` `,
    (` ` -> parse_last_row ; eol, eol).

parse_crates(Crates) -->
    parse_crate_rows(Rows),
    { transpose(Rows, TRows),
      maplist(include(\=(false)), TRows, Crates) }.
