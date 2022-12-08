:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(input(Grid), current_input),
    grid_array(Grid, Arr),!,
    solve(Arr, P1, P2),
    writeln(P1),
    writeln(P2).

move((X, Y), up, (X, Y1)) :- !, Y1 #= Y - 1.
move((X, Y), down, (X, Y1)) :- !, Y1 #= Y + 1.
move((X, Y), left, (X1, Y)) :- !, X1 #= X - 1.
move((X, Y), right, (X1, Y)) :- !, X1 #= X + 1.

grid_array(Grid, Arr) :-
    length(Grid, Height),
    functor(Arr, y, Height),
    foreach(nth1(I, Grid, Row), ga_row(Arr, I, Row)).
ga_row(Arr, I, Row) :- arg(I, Arr, RV), RV =.. [x | Row].

grid_ref(Arr, (X, Y), Val) :-
    arg(Y, Arr, Row),
    arg(X, Row, Val).

solve(Arr, P1, P2) :-
    functor(Arr, _, Height),
    arg(1, Arr, Row0),
    functor(Row0, _, Width),
    bagof(Pos, inrange(((1,1),(Width,Height)), Pos), Posns),
    foldl(improve(Arr), Posns, (0, 0), (P1, P2)).

improve(Arr, Pos, (Vis0, Best0), (Vis, Best)) :-
    foldl(improve0(Arr, Pos), [up,down,left,right], (false, 1), (IsVis, Score)),
    (IsVis -> Vis #= Vis0 + 1 ; Vis = Vis0),
    (Best0 < Score -> Best = Score ; Best = Best0).
improve0(Arr, Pos, Dir, (IsVis0, Score0), (IsVis, Score)) :-
    look(Arr, Pos, Dir, Dist, SeesEdge),
    (IsVis0 -> IsVis = true ; IsVis = SeesEdge),
    Score #= Score0 * Dist.

look(Arr, From, Dir, Dist, SeesEdge) :-
    grid_ref(Arr, From, Height),
    move(From, Dir, From1),
    look(Arr, From1, Height, Dir, Dist, SeesEdge).

look(Arr, From, Height, Dir, Dist, SeesEdge) :-
    (grid_ref(Arr, From, HeightHere)
    -> (HeightHere >= Height
       -> Dist = 1, SeesEdge = false
       ; move(From, Dir, From1),
         look(Arr, From1, Height, Dir, Dist0, SeesEdge),
         Dist is Dist0 + 1)
    ; Dist = 0, SeesEdge = true).

input([]) --> eos.
input([Row | Tail]) -->
    string_without(`\n`, Codes), eol, input(Tail),
    { maplist(code_tree, Codes, Row) }.

code_tree(Code, Tree) :- Tree #= Code - 0'0.
