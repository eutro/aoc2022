:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(monkeys(Monkeys), current_input),
    nthround(p1, 20, Monkeys, R20),
    mbis(R20, P1),
    writeln(P1),

    phrase(mklcm(Lcm), Monkeys),
    nthround(p2(Lcm), 10000, Monkeys, R10K),
    mbis(R10K, P2),
    writeln(P2),

    true.

mklcm(Lcm) --> mklcm(1, Lcm).
mklcm(Lcm, Lcm) --> eos.
mklcm(Lcm0, Lcm) --> [M], { Lcm1 is lcm(Lcm0, M.factor) }, mklcm(Lcm1, Lcm).

mbis(Ms, Mbis) :-
    maplist([M, I]>>(I = M.inspects), Ms, Is),
    msort(Is, Sorted),
    append(_, [A, B], Sorted),
    Mbis is A * B.

nthround(_, 0, M, M).
nthround(Mode, I, M0, M) :-
    I1 is I - 1,
    round(Mode, M0, M1),
    nthround(Mode, I1, M1, M).

write_hands(Monkeys) :-
    forall(member(M, Monkeys),
           (writeln(M.idx-M.hand))).

round(Mode, Ms0, Ms) :-
    rb_empty(Empty),
    phrase(round(Mode, Empty, App, Ms1), Ms0),
    maplist(apply_apps(App), Ms1, Ms).

round(_, A, A, []) --> eos.
round(Mode, App0, App, [M | Tl]) -->
    [M0],
    { turn(Mode, M0, M, App0, App1) },
    round(Mode, App1, App, Tl).

apply_apps(App, M, M1) :-
    (rb_lookup(M.idx, Apps0, App)
    -> reverse(Apps0, Apps),
       append(M.hand, Apps, Hand),
       M1 = M.put([hand=Hand])
    ; M1 = M).

turn(Mode, M, M.put([hand=[], inspects=Ins]), App0, App) :-
    %writeln(M.idx-"start"-App0),
    (rb_lookup(M.idx, Apps0, App0)
    -> rb_delete(App0, M.idx, App1),
       reverse(Apps0, Apps)
    ; App1 = App0, Apps = []),
    append(M.hand, Apps, Hand),
    length(Hand, HandLen),
    Ins is M.inspects + HandLen,
    phrase(inspect(Mode, M, App1, App), Hand),
    %writeln(M.idx-"done"-App),
    true.

inspect(_, _, App, App) --> eos.
inspect(Mode, M, App0, App) -->
    [Worry0],
    { eval(Worry0, M.op, Worry1),
      manage_worry(Mode, Worry1, Worry),
      (0 is Worry mod M.factor
      -> T = M.ift
      ; T = M.iff),
      (rb_lookup(T, Apps0, App0)
      -> rb_insert(App0, T, [Worry | Apps0], App1)
      ; rb_insert_new(App0, T, [Worry], App1)) },
    inspect(Mode, M, App1, App).

manage_worry(p1, W0, W) :- W is W0 div 3.
manage_worry(p2(Lcm), W0, W) :- W is W0 mod Lcm.

monkeys([Monkey | Monkeys]) -->
    monkey(Monkey),
    (eos -> { Monkeys = [] }
    ; eol, monkeys(Monkeys)).

monkey(monkey{
           idx: I,
           hand: Starting,
           op: Op,
           factor: Factor,
           ift: IfT,
           iff: IfF,
           inspects: 0
       }) -->
    `Monkey `, integer(I), `:`, eol,
    `  Starting items: `, items(Starting), eol, !,
    `  Operation: `, operator(Op), eol,
    `  Test: divisible by `, integer(Factor), eol,
    `    If true: throw to monkey `, integer(IfT), eol,
    `    If false: throw to monkey `, integer(IfF), eol.

eval(Old, sqr, New) :- New #= Old * Old.
eval(Old, Term, New) :-
    Term =.. [F, N],
    Calc =.. [F, N, Old],
    New #= Calc.

items([Item | Tl]) --> integer(Item), (`, ` -> items(Tl) ; { Tl = [] }).
operator(Term) -->
    `new = old `, [Op], ` `, integer(N),
    { atom_codes(F, [Op]),
      Term =.. [F, N] }.
operator(sqr) --> `new = old * old`.
