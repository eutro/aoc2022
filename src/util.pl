:- module(util, [seqof//4, seqof1//4, string1//1, ntmap//3]).
:- use_module(library(dcg/basics)).

seqof(NonTerm, Suff, End, [Elem | Tail]) -->
    call(NonTerm, Elem), Suff, !,
    seqof(NonTerm, Suff, End, Tail).
seqof(_, _, End, []) --> End.

seqof1(NonTerm, Suff, End, [Elem | Tail]) -->
    call(NonTerm, Elem), Suff,
    seqof(NonTerm, Suff, End, Tail).

ntmap(Func, NonTerm, Res) -->
    call(NonTerm, X),
    { call(Func, X, Res) }.

string1([C|Cs]) --> [C], string(Cs).
