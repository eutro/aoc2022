:- use_module(library(dcg/basics)).
:- use_module(library(clpfd)).

main :-
    '$INPUT="$(awk $COMMANDS)"'(Cmds),
    'eval'(Cmds, Fs),
    'du -b'(Fs, Sizes),

    % 'tree'(Fs),

    'find -size -100000c'(Sizes, P1),
    'echo'(P1),

    'find -size +$(($USED - 40000000)) | head -1'(Fs, Sizes, P2),
    'echo'(P2).

'$INPUT="$(awk $COMMANDS)"'(Cmds) :- phrase_from_stream('awk $COMMANDS'(Cmds), current_input).

'echo'(X) :- writeln(X).

'find -size +$(($USED - 40000000)) | head -1'(Fs, Sizes, ToDel) :-
    'du -bs'(Fs, Used),
    ToFree #= Used - 40000000,
    member(ToDel, Sizes),
    ToDel >= ToFree,!.

'find -size -100000c'(Sizes, Ans) :-
    include(>=(100000), Sizes, Small),
    sum_list(Small, Ans).

'du -b'(Fs, Dirs) :- 'find -type d'(Fs, UDirs), msort(UDirs, Dirs).
'find -type d'(_-(_, file), []).
'find -type d'(_-(Sz, dir(Files)), [Sz | Children]) :-
    rb_visit(Files, FilesLs),
    maplist('find -type d', FilesLs, ChildLists),
    append(ChildLists, Children).

'eval'(Cmds, '/'-Fs) :-
    Fs = (_, dir(_)),
    foldl('eval', Cmds, [Fs], _).

'eval'(cd(`..`), [_ | Cwd], Cwd) :- !.
'eval'(cd(`/`), Cwd0, [Fs]) :- !, append(_, [Fs], Cwd0).
'eval'(cd(Name), Cwd0, [Dir | Cwd0]) :-
    [(_, dir(Files)) | _] = Cwd0,
    rb_lookup(Name, Dir, Files).

'eval'(ls(Files), Cwd, Cwd) :-
    [(Size, dir(Children)) | _] = Cwd,
    maplist('stat -c "dir(%n,%s,$(ls $1))"', Files, ChildrenLs),
    maplist('du -bs', ChildrenLs, ChildSizes),
    list_to_rbtree(ChildrenLs, Children),
    sum(ChildSizes, #=, Size).

'du -bs'(_-(Size, _), Size).

'stat -c "dir(%n,%s,$(ls $1))"'(file(Size, Name), Name-(Size, file)).
'stat -c "dir(%n,%s,$(ls $1))"'(dir(Name), Name-(_, dir(_))).

'awk $COMMANDS'([]) --> eos.
'awk $COMMANDS'([Cmd | Tail]) --> 'awk $CMD'(Cmd), !, 'awk $COMMANDS'(Tail).

'awk $CMD'(cd(Dir)) --> `$ cd `, string(Dir), eol.
'awk $CMD'(ls(Files)) --> `$ ls`, eol, 'awk $FILES'(Files).

'awk $FILES'([File | Tail]) --> 'awk $FILE'(File), eol, !, 'awk $FILES'(Tail).
'awk $FILES'([]) --> string_without(`$`, []).

'awk $FILE'(dir(Dirname)) --> `dir `, string_without(`\n`, Dirname).
'awk $FILE'(file(Size, Name)) --> integer(Size), ` `, string_without(`\n`, Name).
