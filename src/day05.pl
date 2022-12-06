:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(input(Crates, Insns), current_input),
    forall(member(Machine, [cm9000, cm9001]),
           (run_and_look(Machine, Insns, Crates, Ans),
            writeln(Ans))).

run_and_look(Machine, Insns, Crates0, Res) :-
    foldl(run_insn(Machine), Insns, Crates0, Crates),
    maplist(nth0(0), Crates, Top),
    string_codes(Res, Top).

run_insn(Machine, [N, From, To], Crates0, Crates) :-
    nth1(From, Crates0, FStack),
    nth1(To, Crates0, TStack),
    move(Machine, N, FStack, TStack, FStack1, TStack1),
    replace(Crates0, From, FStack1, Crates1),
    replace(Crates1, To, TStack1, Crates).

move(Machine, N, F, T, F1, T1) :-
    length(Moved0, N),
    append(Moved0, F1, F),
    maybe_rev(Machine, Moved0, Moved),
    append(Moved, T, T1).

replace(L, I, E, O) :- nth1(I, L, _, R), nth1(I, O, E, R).

maybe_rev(cm9000, M0, M) :- reverse(M0, M).
maybe_rev(cm9001, M, M).

input(Crates, Insns) --> crates(Crates), eol, insns(Insns).

crates(Crates) -->
    rows(Rows),
    { transpose(Rows, TRows),
      maplist(append, TRows, Crates) }.

rows([Row | Tail]) --> row(Row), rows(Tail).
rows([]) --> ` 1 `, string(_), eol.

row([Crate | Tl]) --> crate(Crate), row_tail(Tl).

crate([Crate]) --> `[`, [Crate], `]`.
crate([]) --> `   `.

row_tail([]) --> eol.
row_tail(Row) --> ` `, row(Row).

insns(Insns) --> seqof(insn, [], eos, Insns).
insn([N, From, To]) --> `move `, integer(N), ` from `, integer(From), ` to `, integer(To), eol.
