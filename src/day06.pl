:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

main :-
    stream_to_lazy_list(current_input, Line),
    forall(member(Len, [4, 14]),
           (phrase(marker(Len, Ans), Line),
            writeln(Ans))).

marker(Len, I) -->
    { length(Marker, Len) },
    string(Prefix), Marker, remainder(_),
    { sort(Marker, Sorted),
      length(Sorted, Len),
      length(Prefix, I0),
      I #= I0 + Len }.
