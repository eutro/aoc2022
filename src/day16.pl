:- use_module(library(dcg/basics)).
:- use_module(util).

main :-
    phrase_from_stream(valves(Valves), current_input),
    valve_graph(Valves, Graph),
    forall(member(s(Who, Time), [s(1, 30), s(2, 26)]),
           (best_pressure(Graph, Who, Time, Ans), % implemented in day16.c
            writeln(Ans))).

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
