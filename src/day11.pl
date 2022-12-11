:- use_module(library(dcg/basics)).
:- use_module(util).

main :-
    phrase_from_stream(monkeys(Monkeys), current_input),
    phrase(mklcm(Lcm), Monkeys),
    forall(member(r(Mode, N),
                  [r(p1, 20),
                   r(p2(Lcm), 10000)]),
           (nthround(Mode, N, Monkeys, RN),
            mbis(RN, Ans),
            writeln(Ans))).

mklcm(Lcm) --> mklcm(1, Lcm).
mklcm(Lcm, Lcm) --> eos.
mklcm(Lcm0, Lcm) --> [M], { Lcm1 is lcm(Lcm0, M.factor) }, mklcm(Lcm1, Lcm).

mbis(Ms, Mbis) :-
    maplist([M, I]>>(I = M.inspects), Ms, Is),
    msort(Is, Sorted),
    append(_, [A, B], Sorted),
    Mbis is A * B.

nthround(Mode, N, Ms, RN) :-
    rb_empty(App0),
    nthround(Mode, Ms, N, App0, RN).

nthround(_, M, 0, _, M).
nthround(Mode, M0, I, App0, M) :-
    I1 is I - 1,
    phrase(round(Mode, App0, App, M1), M0),
    nthround(Mode, M1, I1, App, M).

round(_, A, A, []) --> eos.
round(Mode, App0, App, [M | Tl]) -->
    [M0],
    { turn(Mode, M0, M, App0, App1) },
    round(Mode, App1, App, Tl).

turn(Mode, M, M.put([hand=[], inspects=Ins]), App0, App) :-
    (rb_lookup(M.idx, Apps0, App0)
    -> rb_delete(App0, M.idx, App1),
       reverse(Apps0, Apps)
    ; App1 = App0, Apps = []),
    append(M.hand, Apps, Hand),
    length(Hand, HandLen),
    Ins is M.inspects + HandLen,
    phrase(inspect(Mode, M, App1, App), Hand).

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

manage_worry(p1, W0, W) => W is W0 div 3.
manage_worry(p2(Lcm), W0, W) => W is W0 mod Lcm.

monkeys([Monkey | Monkeys]) -->
    monkey(Monkey),
    (eos -> { Monkeys = [] }
    ; eol, monkeys(Monkeys)).

monkey(monkey{
           hand: Starting,
           inspects: 0,
           idx: I,
           op: Op,
           factor: Factor,
           ift: IfT,
           iff: IfF
       }) -->
    `Monkey `, integer(I), `:`, eol,
    `  Starting items: `, items(Starting), eol, !,
    `  Operation: `, operator(Op), eol,
    `  Test: divisible by `, integer(Factor), eol,
    `    If true: throw to monkey `, integer(IfT), eol,
    `    If false: throw to monkey `, integer(IfF), eol.

eval(Old, sqr, New) => New is Old * Old.
eval(Old, f(*, N), New) => New is Old * N.
eval(Old, f(+, N), New) => New is Old + N.

items([Item | Tl]) --> integer(Item), (`, ` -> items(Tl) ; { Tl = [] }).
operator(f(F, N)) -->
    `new = old `, [Op], ` `, integer(N),
    { atom_codes(F, [Op]) }.
operator(sqr) --> `new = old * old`.
