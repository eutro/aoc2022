:- use_module(library(prolog_stack)).
:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(valves(Valves), current_input),
    valve_graph(Valves, Graph),
    best_pressure(Graph, 1, 30, P1),
    writeln(P1),
    true.

best_pressure(Graph, From, Rem, Best) :- best_pressure(Graph, From, Rem, [], Best).
:- table best_pressure/5.
best_pressure(Graph, From, Rem, Open, Best) :-
    (bagof(Pressure, next_pressure(Graph, From, Rem, Open, Pressure), Pressures)
    -> once(max_member(solLe, Best, Pressures))
    ; Best = s(0, [wait(Rem)]) % nothing more we can do except wait
    ).

solLe(s(X, _), s(Y, _)) :- X =< Y.

next_pressure(Graph, From0, Rem0, Open0, s(Pressure, S)) :-
    arg(From0, Graph, Node),
    arg(From, Node.dsts, Dist),
    \+ ord_memberchk(From, Open0),
    ord_add_element(Open0, From, Open),
    Rem is Rem0 - Dist - 1, Rem > 0,
    arg(From, Graph, DstNode),
    S = [open{name: DstNode.a, rem: Rem} | S0],
    best_pressure(Graph, From, Rem, Open, s(Pressure0, S0)),
    Pressure is Pressure0 + Rem * DstNode.flow.

q_empty(q([], [])).
q_push(X, q(F, B), q([X | F], B)).
q_pop(q(F, [X | B]), X, q(F, B)) :- !.
q_pop(q(F, []), X, q([], F1)) :- reverse(F, [X | F1]).

valve_graph(Valves, Graph) :-
    bagof(Valve, working_valve(Valves, Valve), Working),
    maplist(get_dict(name), Working, WorkingNames),
    maplist(build_node(Valves, WorkingNames), Working, Nodes),
    Graph =.. [g | Nodes].

working_valve(Valves, Valve) :-
    rb_in(_, Valve, Valves),
    once((Valve.flow \= 0 ; Valve.name = "AA")).

build_node(Valves, Working, Valve, n{a: Valve.name, flow: Valve.flow, dsts: Dsts}) :-
    bfs(Valves, Valve.name, DstsMap),
    maplist(get_map(DstsMap), Working, Dsts0),
    Dsts =.. [d | Dsts0].
get_map(Map, Key, Val) :- rb_lookup(Key, Val, Map).

bfs(Valves, From, Dsts) :-
    rb_empty(Empty),
    rb_insert(Empty, From, 0, Dsts0),
    bfs(Valves, q([], [From]), Dsts0, Dsts).
bfs(_, Q, Dsts, Dsts) :- q_empty(Q), !.
bfs(Valves, Q0, Dsts0, Dsts) :-
    q_pop(Q0, Name, Q1),
    rb_lookup(Name, Node, Valves),
    rb_lookup(Name, Dst0, Dsts0),
    Dst1 is Dst0 + 1,
    findall(DstN, (member(DstN, Node.dsts), \+ rb_lookup(DstN, _, Dsts0)), Unseen),
    foldl(q_push, Unseen, Q1, Q),
    foldl(add_dst(Dst1), Unseen, Dsts0, Dsts1),
    bfs(Valves, Q, Dsts1, Dsts).
add_dst(Dst, Name, Dsts0, Dsts) :- rb_insert(Dsts0, Name, Dst, Dsts).

valves(Valves) --> valves0(Valves0), { list_to_rbtree(Valves0, Valves) }.

valves0([]) --> eos, !.
valves0([Kv | Tl]) --> valve(Valve), { Kv = Valve.name-Valve }, valves0(Tl).

valve(v{name: Name, flow: Rate, dsts: Dsts}) -->
    `Valve `, valve_name(Name), ` has flow rate=`, integer(Rate),
    `; `, (`tunnel leads to valve ` ; `tunnels lead to valves `), valve_names(Dsts), eol, !.

valve_names([Name | Tl]) --> valve_name(Name), (`, ` -> valve_names(Tl) ; { Tl = [] }).
valve_name(Name) --> string_without(` ,\n`, Codes), { string_codes(Name, Codes) }.
