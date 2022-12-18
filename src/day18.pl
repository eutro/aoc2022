:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(util).

main :-
    phrase_from_stream(tiles(Tiles), current_input),
    surface(Tiles, Surface),
    length(Surface, Area),
    writeln(Area),

    extrema(Min0, Max0, Tiles),
    pos:add(Min, p{x:1, y:1, z:1}, Min0),
    pos:add(Max0, p{x:1, y:1, z:1}, Max),
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

in_bounds(p{x: MinX, y: MinY, z: MinZ}-p{x: MaxX, y: MaxY, z: MaxZ},
          p{x: X, y: Y, z: Z}) :-
    between(MinX, MaxX, X),
    between(MinY, MaxY, Y),
    between(MinZ, MaxZ, Z).

flood(Points, Bounds, From, Seen0, Seen) :-
    rb_add(From, Seen0, Seen1),
    neighbours(From, Neighbours),
    exclude(rb_contains(Seen0), Neighbours, ToVisit0),
    exclude(rb_contains(Points), ToVisit0, ToVisit1),
    include(in_bounds(Bounds), ToVisit1, ToVisit),
    % writeln(flooding{from: From, visiting: ToVisit}),
    foldl(flood(Points, Bounds), ToVisit, Seen1, Seen).

extrema(p{x: MinX, y: MinY, z: MinZ},
        p{x: MaxX, y: MaxY, z: MaxZ},
        Points) :-
    extrema(x, MinX, MaxX, Points),
    extrema(y, MinY, MaxY, Points),
    extrema(z, MinZ, MaxZ, Points).
extrema(Axis, Min, Max, Points) :-
    maplist(get_dict(Axis), Points, AxisPosns),
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

axes([p{x:1,y:0,z:0}, p{x:0,y:1,z:0}, p{x:0,y:0,z:1}]).
directions(Dirs) :-
    axes(Axes),
    maplist(pos:negate, Axes, AxesInv),
    append(Axes, AxesInv, Dirs).
    
pos:negate(Pos0, Pos) :- pos:add(Pos0, Pos, p{x:0,y:0,z:0}).
pos:add(p{x:X1, y:Y1, z:Z1}, p{x:X2, y:Y2, z:Z2}, p{x:X, y:Y, z:Z}) :-
    maplist(plus, [X1, Y1, Z1], [X2, Y2, Z2], [X, Y, Z]).

tiles([]) --> eos, !.
tiles([Pos | Tl]) --> pos(Pos), eol, tiles(Tl).
pos(p{x:X, y:Y, z:Z}) --> integer(X), `,`, integer(Y), `,`, integer(Z). 
