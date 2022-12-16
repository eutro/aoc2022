:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(valves(Valves), current_input),
    valve_graph(Valves, Graph),
    all_nodes(Graph, AllNodes),
    best_pressure(Graph, 1, 1, 30, AllNodes, P1),
    writeln(P1),
    best_pressure(Graph, 2, 1, 26, AllNodes, P2),
    writeln(P2),
    true.

all_nodes(Graph, AllNodes) :-
    functor(Graph, _, Count),
    bagof(I, between(1, Count, I), AllNodes0),
    foldl(set_bit, AllNodes0, 0, AllNodes).

set_bit(N, Mask0, Mask) :- Mask is Mask0 \/ (1 << (N - 1)).
unset_bit(N, Mask0, Mask) :- Mask is Mask0 /\ \ (1 << (N - 1)).
bit_set(N, Mask) :- N in 1..16, 0 #\= Mask /\ (1 << (N - 1)).

:- table best_pressure/6.
best_pressure(Graph, Who, From, Rem, NotOpen, Best) :-
    BestBox = box(-1),
    (bagof(Pressure, next_pressure(Graph, Who, From, Rem, NotOpen, BestBox, Pressure), Pressures)
    -> max_list(Pressures, Best)
    ; Best = 0 % nothing more we can do except wait
    ).

next_pressure(Graph, 2, _, _, NotOpen, _, P) :-
    best_pressure(Graph, me, 1, 26, NotOpen, P).
next_pressure(Graph, Who, From0, Rem0, NotOpen0, BestBox, Pressure) :-
    arg(From0, Graph, Node),
    arg(From, Node.dsts, Dist),
    bit_set(From, NotOpen0), label([From]), unset_bit(From, NotOpen0, NotOpen),
    Rem is Rem0 - Dist - 1, Rem > 0,
    arg(From, Graph, DstNode), DstNode.flow \= 0,
    FlowHere is Rem * DstNode.flow,

    % (Who = 2 -> true
    % ; arg(1, BestBox, LastBest),
    %   best_possible(Graph, NotOpen, Rem, BestPossible0),
    %   BestPossible is BestPossible0 + FlowHere,
    %   BestPossible > LastBest),
    best_pressure(Graph, Who, From, Rem, NotOpen, Pressure0),
    Pressure is Pressure0 + FlowHere,
    % (Who = 2 -> true
    % ; LocalBest is max(LastBest, Pressure),
    %   nb_setarg(1, BestBox, LocalBest)),
    true.

best_possible(Graph, NotOpen, Rem, Score) :-
    bit_set(N, NotOpen),
    bagof(N, label([N]), Idcs),
    maplist(pressure_rel(Graph), Idcs, Rels),
    sum(Rels, #=, Score0),
    Score is Score0 * (Rem - 2).
pressure_rel(Graph, I, Rel) :- arg(I, Graph, V), Rel = V.flow.

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
