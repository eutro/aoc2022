:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(util).

main :-
    phrase_from_stream(commands(Cmds), current_input),
    simulate_all(Cmds, Fs),
    sizeof(Fs, Used),
    alldirs(Fs, Dirs),
    maplist([dir(Sz,_),Sz]>>true, Dirs, USizes),
    msort(USizes, Sizes),
    include(>=(100000), Sizes, FSizes),
    sum_list(FSizes, Total),
    writeln(Total),
    ToFree #= Used - 40000000,
    include(=<(ToFree), Sizes, [ToDel|_]),
    writeln(ToDel),
    true.

alldirs(file(_), []).
alldirs(Dir, [Dir | Children]) :-
    Dir = dir(_, Map),
    rb_visit(Map, Pairs),
    maplist([_-Child, Out]>>alldirs(Child, Out), Pairs, ChildLists),
    append(ChildLists, Children).

sizeof(file(Size), Size).
sizeof(dir(Size, Map), Size) :-
    rb_fold([_-Val, S0, S]>>
            (sizeof(Val, So),
             S #= So + S0),
            Map,
            0,
            Size).

simulate_all(Cmds, Map) :-
    rb_empty(Empty),
    foldl(simulate, Cmds, s(dir(_, Empty), []), s(Map, _)).

simulate(cd(`..`), s(M, [_ | Cwd]), s(M, Cwd)) :- !.
simulate(cd(`/`), s(M, _), s(M, [])) :- !.
simulate(cd(Dir), s(M, Cwd0), s(M, [Dir | Cwd0])).

simulate(ls(Files), s(M0, Cwd), s(M, Cwd)) :- foldl(mapfile(Cwd), Files, M0, M).

mapfile(Cwd, File, M0, M) :-
    reverse(Cwd, Rcwd),
    addfile(Rcwd, File, M0, M).

addfile([], file(Size, Name), dir(Sz, M0), dir(Sz, M)) :-
    rb_insert(M0, Name, file(Size), M).
addfile([], dir(Dirname), dir(Sz, M0), dir(Sz, M)) :-
    (rb_empty(Empty), rb_insert_new(M0, Dirname, dir(_, Empty), M)
    ; true).

addfile([Dir | Tail], File, dir(Sz, M0), dir(Sz, M)) :-
    (rb_lookup(Dir, N0, M0) ; rb_empty(Empty), N0 = dir(_, Empty)),
    addfile(Tail, File, N0, N),
    rb_insert(M0, Dir, N, M).

commands([]) --> eos.
commands([Cmd | Tail]) --> cmd(Cmd), !, commands(Tail).

cmd(cd(Dir)) --> `$ cd `, string(Dir), eol.
cmd(ls(Files)) --> `$ ls`, eol, files(Files).

files([File | Tail]) --> file(File), eol, !, files(Tail).
files([]) --> string_without(`$`, []).

file(dir(Dirname)) --> `dir `, string_without(`\n`, Dirname).
file(file(Size, Name)) --> integer(Size), ` `, string_without(`\n`, Name).

pp_map(Map) :- pp_map(0, `/`, Map).
pp_map(I, Name, dir(Sz, Map)) :-
    pp_head(I, Name, (dir, size=Sz)),
    I1 #= I + 1,
    forall(rb_in(Key, Val, Map),
           (Val = file(Size) -> pp_head(I1, Key, (file, size=Size))
           ; pp_map(I1, Key, Val))).

pp_head(I, Codes, Tag) :-
    string_codes(Str, Codes),
    forall(between(1, I, _), write("  ")),
    write("- "), write(Str), write(" ("), write(Tag), write(")"), nl.
