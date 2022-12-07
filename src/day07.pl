:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

main :-
    phrase_from_stream(commands(Cmds), current_input),
    'eval'(Cmds, Fs),
    'du'(Fs, Sizes),

    % 'tree'(Fs),

    'find -size -100000c'(Sizes, P1),
    'echo'(P1),

    'find -size +$(($USED - 40000000)) | head -1'(Fs, Sizes, P2),
    'echo'(P2).

'echo'(X) :- writeln(X).

'find -size +$(($USED - 40000000)) | head -1'(Fs, Sizes, ToDel) :-
    sizeof(Fs, Used),
    ToFree #= Used - 40000000,
    member(ToDel, Sizes),
    ToDel >= ToFree,!.

'find -size -100000c'(Sizes, Ans) :-
    include(>=(100000), Sizes, Small),
    sum_list(Small, Ans).

'du'(Fs, Dirs) :- sizeof(Fs, _), 'find -type d'(Fs, UDirs), msort(UDirs, Dirs).
'find -type d'(file(_), []).
'find -type d'(dir(Sz, Map), [Sz | Children]) :-
    rb_visit(Map, Pairs),
    maplist([_-Child, Out]>>'find -type d'(Child, Out), Pairs, ChildLists),
    append(ChildLists, Children).

'eval'(Cmds, Fs) :-
    rb_empty(Empty),
    foldl('eval', Cmds, s(dir(_, Empty), []), s(Fs, _)).

'eval'(cd(`..`), s(M, [_ | Cwd]), s(M, Cwd)) :- !.
'eval'(cd(`/`), s(M, _), s(M, [])) :- !.
'eval'(cd(Dir), s(M, Cwd0), s(M, [Dir | Cwd0])).
'eval'(ls(Files), s(M0, Cwd), s(M, Cwd)) :-
    foldl('mkdir -p $1 && touch $2'(Cwd), Files, M0, M).

'mkdir -p $1 && touch $2'(Cwd, File, M0, M) :-
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

sizeof(file(Size), Size).
sizeof(dir(Size, Map), Size) :-
    var(Size),
    rb_fold([_-Val, S0, S]>>
            (sizeof(Val, So),
             S #= So + S0),
            Map, 0, Size).
sizeof(dir(Size, _), Size).

'tree'(Fs) :- 'tree'(0, `/`, Fs).
'tree'(I, Name, dir(Sz, Map)) :-
    'stat -c "%n (%F,%s)"'(I, Name, (directory, Sz)),
    I1 #= I + 1,
    forall(rb_in(Key, Val, Map),
           (Val = file(Size)
           -> 'stat -c "%n (%F,%s)"'(I1, Key, ('regular file', Size))
           ; 'tree'(I1, Key, Val))).

'stat -c "%n (%F,%s)"'(I, Codes, Tag) :-
    string_codes(Str, Codes),
    forall(between(1, I, _), write("  ")),
    write("- "), write(Str), write(" ("), write(Tag), write(")"), nl.
