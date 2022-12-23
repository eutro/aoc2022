:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(grid(Grid), current_input),
    % write_grid(Grid),
    steptimes(10, 0-Grid, S1-Grid1),
    count_empty(Grid1, P1),
    writeln(P1),
    stepfix(10, P2, S1-Grid1, _),
    writeln(P2),
    true.

count_empty(Grid, Empty) :-
    grid_extrema(Grid, Lo, Hi),
    findall(Pos,
            (inrange((Lo, Hi), Pos),
             \+ rb_lookup(Pos, _, Grid)),
            Posns),
    length(Posns, Empty).

write_grid_posns(Grid) :-
    rb_keys(Grid, Keys),
    writeln(Keys).
write_grid(Grid) :-
    write_grid_posns(Grid),
    grid_extrema(Grid, (X1, Y1), (X2, Y2)),
    forall(between(Y1, Y2, Y),
           (forall(between(X1, X2, X),
                   ((rb_lookup((X, Y), _, Grid)
                    -> Vl = '#'
                    ; Vl = '.'),
                    write(Vl))),
            nl)).

steptimes(0, G, G) :- !.
steptimes(N, S0-Grid0, S-Grid) :-
    N1 is N - 1,
    step(S0-Grid0, S1-Grid1, _),
    % write_grid(Grid1),
    steptimes(N1, S1-Grid1, S-Grid).

stepfix(N0, N, S0-Grid0, S-Grid) :-
    N1 is N0 + 1,
    step(S0-Grid0, S1-Grid1, Moved),
    (Moved -> stepfix(N1, N, S1-Grid1, S-Grid)
    ; N = N1).

step(S0-Grid0, S-Grid, Moved) :-
    collect_proposals(S0-Grid0, Proposals0),
    reduce_proposals(Proposals0, Proposals),
    (Proposals = [] -> Moved = false ; Moved = true),
    apply_proposals(Grid0, Proposals, Grid),
    S is (S0 + 1) mod 4.

collect_proposals(S-Grid, Proposals) :-
    findall(Proposal, proposal(S-Grid, Proposal), Proposals).
proposal(S-Grid, Proposal) :-
    rb_in(Pos, _, Grid),
    once((between(-1, 1, DX),
          between(-1, 1, DY),
          once((DX \= 0 ; DY \= 0)),
          mvby(Pos, (DX, DY), Nb),
          rb_lookup(Nb, _, Grid))),
    once((between(0, 3, CDir),
          Consider is (S + CDir) mod 4,
          \+ (consider_dir(Consider, Delta),
              mvby(Pos, Delta, CPos),
              rb_lookup(CPos, _, Grid)),
          consider_delta(Consider, Delta),
          mvby(Pos, Delta, Target),
          Proposal = Target-Pos)).

consider_dir(0, D) => member(D, [( 0, -1), (-1, -1), ( 1, -1)]).
consider_dir(1, D) => member(D, [( 0,  1), (-1,  1), ( 1,  1)]).
consider_dir(2, D) => member(D, [(-1,  0), (-1, -1), (-1,  1)]).
consider_dir(3, D) => member(D, [( 1,  0), ( 1, -1), ( 1,  1)]).
consider_delta(Dir, D) :- once(consider_dir(Dir, D)).

reduce_proposals(P0, P) :- keysort(P0, P1), phrase(rprop(P), P1).
rprop([]) --> eos, !.
rprop(P) --> [K-V], (consume_props(K) -> { P = Tl } ; { P = [K-V | Tl] }), rprop(Tl).
consume_props(K) --> [K-_], (consume_props(K) -> [] ; []).

apply_proposals(Grid0, Proposals, Grid) :- foldl(apply_proposal, Proposals, Grid0, Grid).
:- det(apply_proposal/3).
apply_proposal(To-From, Grid0, Grid) :-
    rb_delete(Grid0, From, Grid1),
    rb_insert(Grid1, To, [], Grid).

grid_extrema(Grid, Lo, Hi) :- rb_keys(Grid, Posns), extrema(Posns, Lo, Hi).
extrema(Posns, Lo, Hi) :- phrase(extrema(Lo, Hi), Posns).
extrema(L, H) --> [Pos], extrema(Pos, Pos, L, H).
extrema(L, H, L, H) --> eos, !.
extrema((Lx0, Ly0), (Hx0, Hy0), Lo, Hi) -->
    [(X, Y)],
    { Lx is min(Lx0, X), Ly is min(Ly0, Y),
      Hx is max(Hx0, X), Hy is max(Hy0, Y) },
    extrema((Lx, Ly), (Hx, Hy), Lo, Hi).

grid(Grid) --> grid0(1, Grid0), { list_to_rbtree(Grid0, Grid) }.
grid0(_, []) --> eos, !.
grid0(Y, Row) --> row(1, Y, Tl, Row), { Y1 is Y + 1 }, grid0(Y1, Tl).
row(_, _, Tl, Tl) --> eol, !.
row(X, Y, Tl, Row) --> `.`, !, { X1 is X + 1 }, row(X1, Y, Tl, Row).
row(X, Y, Tl, [(X, Y)-[] | Row]) --> `#`, { X1 is X + 1 }, row(X1, Y, Tl, Row).
