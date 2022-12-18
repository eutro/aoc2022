:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(tiles(Tiles), current_input),
    surface(Tiles, Surface),
    length(Surface, Area),
    writeln(Area),

    extrema(Min0, Max0, Tiles),
    pos:add(Min, p(1,1,1), Min0),
    pos:add(Max0, p(1,1,1), Max),
    rb_empty(Empty),
    foldl(rb_add, Tiles, Empty, TilesMap),
    flood(TilesMap, Min-Max, Max, Empty, OutsideTilesMap),
    rb_keys(OutsideTilesMap, OutsideTiles),
    surface(OutsideTiles, OutsideSurface0),
    ord_intersect(OutsideSurface0, Surface, OutsideSurface),
    length(OutsideSurface, OuterArea),
    writeln(OuterArea),
    true.

surface(Tiles, Surface) :-
    maplist(faces, Tiles, AllFaces0),
    append(AllFaces0, AllFaces),
    msort(AllFaces, FacesSorted),
    phrase(deldupes(Surface), FacesSorted).

rb_contains(Rb, Point) :- rb_lookup(Point, _, Rb).
rb_add(Point, Rb0, Rb) :- rb_insert(Rb0, Point, [], Rb).

in_bounds(p(MinX, MinY, MinZ)-p(MaxX, MaxY, MaxZ), p(X, Y, Z)) :-
    between(MinX, MaxX, X),
    between(MinY, MaxY, Y),
    between(MinZ, MaxZ, Z).

flood(Points, Bounds, From, Seen0, Seen) :-
    rb_add(From, Seen0, Seen1),
    neighbours(From, Neighbours),
    include(should_visit(Points, Bounds, Seen0), Neighbours, ToVisit),
    foldl(flood(Points, Bounds), ToVisit, Seen1, Seen).

should_visit(Points, Bounds, Seen, Point) :-
    in_bounds(Bounds, Point),
    \+ rb_contains(Seen, Point),
    \+ rb_contains(Points, Point).

extrema(p(MinX, MinY, MinZ), p(MaxX, MaxY, MaxZ), Points) :-
    extrema(1, MinX, MaxX, Points),
    extrema(2, MinY, MaxY, Points),
    extrema(3, MinZ, MaxZ, Points).
extrema(Axis, Min, Max, Points) :-
    maplist(arg(Axis), Points, AxisPosns),
    max_list(AxisPosns, Max),
    min_list(AxisPosns, Min).

deldupes([]) --> eos, !.
deldupes(Tl) --> [A, A], !, deldupes(Tl).
deldupes([A | Tl]) --> [A], deldupes(Tl).

neighbours(Point, Neighbours) :-
    directions(Dirs),
    maplist(pos:add(Point), Dirs, Neighbours).

faces(Point, Faces) :-
    neighbours(Point, Neighbours),
    maplist(face(Point), Neighbours, Faces).

face(From, To, Face) :- From @< To, !, Face = From-To.
face(To, From, Face) :- Face = From-To.

axes([p(1,0,0), p(0,1,0), p(0,0,1)]).
directions(Dirs) :-
    axes(Axes),
    maplist(pos:negate, Axes, AxesInv),
    append(Axes, AxesInv, Dirs).
    
pos:negate(Pos0, Pos) :- pos:add(Pos0, Pos, p(0,0,0)).
pos:add(p(X1, Y1, Z1), p(X2, Y2, Z2), p(X, Y, Z)) :-
    maplist(plus, [X1, Y1, Z1], [X2, Y2, Z2], [X, Y, Z]).

tiles([]) --> eos, !.
tiles([Pos | Tl]) --> pos(Pos), eol, tiles(Tl).
pos(p(X, Y, Z)) --> integer(X), `,`, integer(Y), `,`, integer(Z). 
