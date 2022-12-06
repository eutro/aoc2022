:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

main :-
    stream_to_lazy_list(current_input, Line),
    phrase(marker(4, P1), Line),
    write(P1),nl,
    phrase(marker(14, P2), Line),
    write(P2),nl,
    true.

marker(Len, I) -->
    string(Prefix),
    { length(Marker, Len) },
    Marker,
    remainder(_),
    { sort(Marker, Sorted),
      length(Sorted, Len),
      length(Prefix, I0),
      I #= I0 + Len }.
