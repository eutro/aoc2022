:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(solution_sequences)).
:- use_module(util).

main :-
    phrase_from_stream(sensors(Sensors), current_input),
    forall(member(c(G, N), [c(row_count_occluded, 2000000),
                            c(find_distress, 4000000)]),
           (call(G, Sensors, N, Ans), writeln(Ans))).

find_distress(Sensors, MaxPos, Distress) :-
    nth0(I, Sensors, First),
    J #> I,
    nth0(J, Sensors, Second),
    penumbra((U, V), First),
    penumbra((U, V), Second),
    limit(2, label([U, V])), % the two we care about will only intersect once or twice
    map_coord((X, Y), (U, V)),
    [X, Y] ins 0..MaxPos,
    \+ (member(Sensor, Sensors),
        occludes(Sensor, (X, Y))),
    Distress is X * 4000000 + Y.

penumbra((U, V), Sensor) :-
    map_coord(Sensor.pos, (Su, Sv)),
    R = Sensor.range,
    Umin is Su - R - 1, Umax is Su + R + 1,
    Vmin is Sv - R - 1, Vmax is Sv + R + 1,
    U in Umin..Umax, V in Vmin..Vmax,
    (U #= Umin #\/ U #= Umax) #\/ (V #= Vmin #\/ V #= Vmax).

map_coord((X, Y), (U, V)) :-
    U #= X + Y,
    V #= X - Y,
    2 * X #= U + V.

row_count_occluded(Sensors, Row, Occluded) :-
    occlusions(Row, Sensors, Area),
    count_beacons(Sensors, Row, Count),
    Occluded is Area - Count.

count_beacons(Sensors, Row, Count) :-
    maplist(get_dict(beacon), Sensors, Beacons0),
    sort(Beacons0, Beacons),
    include(beacon_row(Row), Beacons, Ls),
    length(Ls, Count).
beacon_row(Row, (_, Row)).

occlusions(Y, Sensors, Count) :-
    maplist(occludes(Y), Sensors, Occlusions),
    fdset_union(Occlusions, Set),
    fdset_size(Set, Count).
occludes(Y, Sensor, Set) :- occludes(Sensor, (X, Y)), !, fd_set(X, Set).
occludes(_, _, Set) :- empty_fdset(Set).
occludes(Sensor, (X, Y)) :- dist_mh(Sensor.pos, (X, Y), Dist), Dist #=< Sensor.range.

sensors([]) --> eos, !.
sensors([Sensor | Tl]) --> sensor(Sensor), sensors(Tl).

sensor(s{pos: Pos, beacon: Beacon, range: Range}) -->
    `Sensor at `, pos(Pos), `: closest beacon is at `, pos(Beacon), eol,
    { dist_mh(Pos, Beacon, Range) }.

dist_mh((X1, Y1), (X2, Y2), Dist) :- Dist #= abs(X1 - X2) + abs(Y1 - Y2).

pos((X, Y)) --> `x=`, integer(X), `, y=`, integer(Y).
