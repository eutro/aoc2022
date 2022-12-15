:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(sensors(Sensors), current_input),
    writeln(Sensors),
    Row = 2000000,
    maplist(occludes(Row), Sensors, Spans),
    collect_spans(Spans, RangeSet),
    area(RangeSet, Area),
    count_beacons(Sensors, Row, Count),
    P1 #= Area - Count,
    writeln(P1),
    true.

count_beacons(Sensors, Row, Count) :-
    maplist(get_dict(beacon), Sensors, Beacons0),
    sort(Beacons0, Beacons),
    include(beacon_row(Row), Beacons, Ls),
    length(Ls, Count).
beacon_row(Row, (_, Row)).

sensors([]) --> eos, !.
sensors([Sensor | Tl]) --> sensor(Sensor), sensors(Tl).

area([], 0) :- !.
area([s(Lo, Hi) | Tl], R) :- R #= (Hi - Lo + 1) + R1, area(Tl, R1).

collect_spans(Spans, RangeSet) :-
    include(\=(false), Spans, NotFalse),
    sort(NotFalse, Sorted),
    merge_spans(Sorted, RangeSet),
    writeln(Sorted),
    writeln(RangeSet).

merge_spans(Spans, Rs) :- merge_spans(Spans, [], Rs).
merge_spans([], Rs, Rs) :- !.
merge_spans([Nxt | Spans], [], Rs) :- !, merge_spans(Spans, [Nxt], Rs).
merge_spans([Nxt | Spans], [Fst | Tl], Rs) :-
    Fst = s(Sf, Ef),
    Nxt = s(Sn, En),
    (Sn =< Ef
    -> E is max(En, Ef),
       Rs1 = [s(Sf, E) | Tl],
       merge_spans(Spans, Rs1, Rs)
    ; Rs = [Fst | Tl1],
      merge_spans(Spans, [Nxt | Tl], Tl1)).

occludes(Row, Sensor, Span) :-
    (Sx, Sy) = Sensor.pos,
    LocalRange is Sensor.range - abs(Row - Sy),
    (LocalRange >= 0
    -> Lo is Sx - LocalRange,
       Hi is Sx + LocalRange,
       Span = s(Lo, Hi)
    ; Span = false).

sensor(s{pos: Pos, beacon: Beacon, range: Range}) -->
    `Sensor at `, pos(Pos), `: closest beacon is at `, pos(Beacon), eol,
    { dist_mh(Pos, Beacon, Range) }.

dist_mh((X1, Y1), (X2, Y2), Dist) :- Dist #= abs(X1 - X2) + abs(Y1 - Y2).

pos((X, Y)) --> `x=`, integer(X), `, y=`, integer(Y).
