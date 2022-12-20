:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(blueprints(Blueprints), current_input),
    foreach((var(Ans), member(Mode, [p1, p2])),
            (simulate(Mode, Blueprints, Ans),
             writeln(Ans))).

simulate(p1, Bps, Ans) :-
    maplist(simulate_bp(24) /* see day19.c */, Bps, Geodes),
    maplist([bp(_, Id), G, R]>>(R is Id * G), Bps, Geodes, Qualities),
    sum(Qualities, #=, Ans).

simulate(p2, Bps0, Ans) :-
    length(Bps, 3),
    append(Bps, _, Bps0),
    maplist(simulate_bp(32), Bps, Geodes),
    foldl([A, B, C]>>(C is A * B), Geodes, 1, Ans).

blueprints([]) --> blanks, eos, !.
blueprints([Blueprint | Tl]) --> blueprint(Blueprint), blueprints(Tl).

blueprint(bp(ps(Ore, Clay, Obsidian, Geode), Id)) -->
    blanks, `Blueprint `, integer(Id), `:`, !,
    pattern(ore, Ore),
    pattern(clay, Clay),
    pattern(obsidian, Obsidian),
    pattern(geode, Geode).

empty_pat(p{ore: 0, clay: 0, obsidian: 0, geode: 0}).
is_pat(p{ore: _, clay: _, obsidian: _, geode: _}).

pattern(Mat, Deps) -->
    blanks, `Each `, !, mat(Mat), ` robot costs `, deps(Deps).

deps(M) -->
    integer(Cost), ` `, mat(Mat),
    (`.` -> { R = p{ore: 0, clay: 0, obsidian: 0, geode: 0} }
    ; ` and `, deps(R)),
    { M = R.put(Mat, Cost) }.

mat(ore) --> `ore`, !.
mat(clay) --> `clay`, !.
mat(obsidian) --> `obsidian`, !.
mat(geode) --> `geode`, !.
