:- use_module(library(dcg/basics)).
:- use_module(util).

main :-
    phrase_from_stream(monkeys(Monkeys, States0), current_input),
    foldl(lcmof, Monkeys, 1, Lcm),
    foreach(member(r(Mode, N),
                   [r(p1, 20),
                    r(p2(Lcm), 10000)]),
            simulate(Mode, Monkeys, N, States0)).
simulate(Mode, Monkeys, N, States0) :-
    duplicate_term(States0, States),
    nthround(Mode, Monkeys, N, States, Sn),
    mbis(Sn, Ans),
    writeln(Ans).

lcmof(M, Lcm0, Lcm) :- Lcm is lcm(M.factor, Lcm0).

mbis(Sn, Mbis) :-
    Sn =.. [_ | Ms],
    maplist([s(_, Ins), I]>>(I is -Ins), Ms, Is),
    msort(Is, [A, B | _]),
    Mbis is A * B.

nthround(_, _, 0, S, S) :- !.
nthround(Mode, Ms, I, S0, S) :-
    I1 is I - 1,
    foldl(turn(Mode), Ms, S0, S1),
    nthround(Mode, Ms, I1, S1, S).

turn(Mode, M, S0, S) :-
    sget(M.idx, S0, Sm),
    Sm = s(Hand, Ins0),
    length(Hand, HandLen),
    Ins is Ins0 + HandLen,
    maplist(worry(Mode, M), Hand, NewHand),
    partition(check(M), NewHand, ToTrue, ToFalse),
    sput(M.idx, S0, s([], Ins), S1),
    throw_to(M.ift, ToTrue, S1, S2),
    throw_to(M.iff, ToFalse, S2, S).

throw_to(I, Ls, S0, S) :-
    sget(I, S0, s(Hand0, Ins)),
    append(Ls, Hand0, Hand),
    sput(I, S0, s(Hand, Ins), S).

sget(I, S, Si) :- arg(I, S, Si).
% safety: we discard the old state every time (we still backtrack in the forall, though)
sput(I, S, Si, S) :- setarg(I, S, Si).

worry(Mode, M, Worry0, Worry) :- eval(Worry0, M.op, Worry1), manage_worry(Mode, Worry1, Worry).
manage_worry(p1, W0, W) => W is W0 div 3.
manage_worry(p2(Lcm), W0, W) => W is W0 mod Lcm.

check(M, Worry) :- 0 is Worry mod M.factor.

eval(Old, sqr, New) => New is Old * Old.
eval(Old, f(*, N), New) => New is Old * N.
eval(Old, f(+, N), New) => New is Old + N.

monkeys(Monkeys, States) --> monkeys0(Monkeys, States0), { States =.. [ss | States0] }.
monkeys0([Monkey | Monkeys], [State | States]) -->
    monkey(Monkey, State),
    (eos -> { Monkeys = [], States = [] }
    ; eol, monkeys0(Monkeys, States)).

monkey(monkey{
           idx: I,
           op: Op,
           factor: Factor,
           ift: IfT,
           iff: IfF
       },
       s(Starting, 0)) -->
    `Monkey `, monkeyref(I), `:`, eol,
    `  Starting items: `, items(Starting), eol, !,
    `  Operation: `, operator(Op), eol,
    `  Test: divisible by `, integer(Factor), eol,
    `    If true: throw to monkey `, monkeyref(IfT), eol,
    `    If false: throw to monkey `, monkeyref(IfF), eol.

monkeyref(I) --> integer(I0), { I is I0 + 1 }.
items([Item | Tl]) --> integer(Item), (`, ` -> items(Tl) ; { Tl = [] }).
operator(sqr) --> `new = old * old`, !.
operator(f(F, N)) --> `new = old `, [Op], ` `, integer(N), { atom_codes(F, [Op]) }.
