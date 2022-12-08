:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(input(Grid), current_input),
    grid_visible(Grid, Vis),
    length(Vis, P1),
    writeln(P1),
    best_score(Grid, P2),
    writeln(P2),
    true.

best_score(Grid, Best) :-
    bagof(Score, any_score(Grid, Score), Scores),
    max_list(Scores, Best).

any_score(Grid, Score) :-
    length(Grid, Height),
    Height0 #= Height - 1,
    [Row0 | _] = Grid,
    length(Row0, Width),
    Width0 #= Width - 1,
    between(0, Height0, Y),
    between(0, Width0, X),
    look_score(Grid, X, Y, Score).

look_score(Grid, X, Y, Score) :-
    look_lr(Grid, X, Y, Left, Right),
    transpose(Grid, GridT),
    look_lr(GridT, Y, X, Up, Down),
    Score #= Left * Right * Up * Down.

look_lr(Grid, X, Y, Left, Right) :-
    nth0(Y, Grid, Row),
    length(Lhs, X),
    append(Lhs, [Height | Rhs], Row),
    look(Height, Rhs, Right),
    reverse(Lhs, LhsR),
    look(Height, LhsR, Left).

look(Height, Trees, Count) :- look(Height, Trees, 0, Count).
look(_, [], Acc, Acc).
look(Height, [Tree | Tl], Acc0, Acc) :-
    Acc1 #= Acc0 + 1,
    (Tree #< Height -> look(Height, Tl, Acc1, Acc)
    ; Acc = Acc1).

grid_visible(Grid, Vis) :-
    grid_row_visible(Grid, VisR),
    transpose(Grid, GridT),
    grid_row_visible(GridT, VisCT),
    maplist([(X,Y),(Y,X)]>>true, VisCT, VisC),
    append(VisR, VisC, VisU),
    sort(VisU, Vis).

grid_row_visible(Grid, Vis) :- grid_row_visible(Grid, 0, Vis0), append(Vis0, Vis).
grid_row_visible([], _, []).
grid_row_visible([Row | Tl], Y, [RVis | Vis]) :-
    row_bivisible(Row, Y, RVis),
    Y1 #= Y + 1,
    grid_row_visible(Tl, Y1, Vis).

row_bivisible(Row, Y, Vis) :-
    row_visible(Row, Y, 0, 1, VisL),
    reverse(RRow, Row),
    length(Row, Len),
    LenM #= Len - 1,
    row_visible(RRow, Y, LenM, -1, VisR),
    append(VisL, VisR, VisBoth),
    sort(VisBoth, Vis).

row_visible(Row, Y, I0, Inc, Vis) :-
    I #= I0 + Inc,
    row_visible(Row, Y, I, Inc, [(I0, Y)], Vis).
row_visible([P, C | Tl], Y, I, Inc, Vis0, Vis) :-
    P #< C, !,
    I1 #= I + Inc, 
    row_visible([C | Tl], Y, I1, Inc, [(I, Y) | Vis0], Vis).
row_visible([P, _ | Tl], Y, I, Inc, Vis0, Vis) :-
    I1 #= I + Inc,
    row_visible([P | Tl], Y, I1, Inc, Vis0, Vis).
row_visible(_, _, _, _, Vis, Vis).

input([]) --> eos.
input([Row | Tail]) -->
    string_without(`\n`, Codes), eol, input(Tail),
    { maplist(code_tree, Codes, Row) }.

code_tree(Code, Tree) :- Tree #= Code - 0'0.
