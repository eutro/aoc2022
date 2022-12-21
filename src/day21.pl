:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(monkeys(Monkeys), current_input),
    forall(member(Mode, [p1, p2]),
           (constrain_all(Mode, Monkeys, Ans),
            writeln(Ans))).

constrain_all(Mode, Monkeys, Ans) :-
    rb_visit(Monkeys, Kvs),
    maplist(constrain(Mode, Monkeys), Kvs),
    mode_monkey(Mode, Target),
    rb_lookup(Target, m(Ans, _), Monkeys).

mode_monkey(p1, "root").
mode_monkey(p2, "humn").

constrain(Mode, Monkeys, Name-m(Val, Expr)) :- eval(Mode, Monkeys, Name, Expr, Val).

eval(p2, Monkeys, "root", f(_, Lhs, Rhs), V) :- !,
    eval(p1, Monkeys, _, f((=), Lhs, Rhs), V).
eval(p2, _, "humn", _, _) :- !.

eval(_, _, _, l(N), V) :- V #= N.
eval(_, Monkeys, _, f(Op, Lhs, Rhs), V) :-
    rb_lookup(Lhs, m(LhsV, _), Monkeys),
    rb_lookup(Rhs, m(RhsV, _), Monkeys),
    eval0(Op, LhsV, RhsV, V).

eval0((*), Lhs, Rhs, V) => V #= Lhs * Rhs.
eval0((+), Lhs, Rhs, V) => V #= Lhs + Rhs.
eval0((-), Lhs, Rhs, V) => V #= Lhs - Rhs.
eval0((/), Lhs, Rhs, V) => V * Rhs #= Lhs.
eval0((=), Lhs, Rhs, _) => Lhs #= Rhs.

monkeys(Monkeys) --> monkeys0(Kvs), { list_to_rbtree(Kvs, Monkeys) }.
monkeys0([]) --> eos, !.
monkeys0([M | Tl]) --> monkey(M), eol, monkeys0(Tl).
monkey(Name-m(_, Expr)) --> name(Name), `: `, expr(Expr).
name(Name) --> string_without(`\n :`, Codes), { string_codes(Name, Codes) }.
expr(l(N)) --> integer(N), !.
expr(f(Op, Lhs, Rhs)) --> name(Lhs), ` `, [C], ` `, name(Rhs), { atom_codes(Op, [C]) }.
