:- initialization(lint_files).

:- use_module(library(prolog_xref)).

:- dynamic failed/1.

goal_not_available(File, Goal) :-
    xref_called(File, Goal, _),
    \+ xref_defined(File, Goal, _),
    term_to_atom(Goal, GoalAtom),
    \+ sub_atom(GoalAtom, _, _, _, 'clpfd:'),
    \+ sub_atom(GoalAtom, _, _, _, 'file_search_path'),
    \+ sub_atom(GoalAtom, _, _, _, 'user:file_search_path').

lint_file(File) :-
    xref_source(File),
    (   goal_not_available(File, Goal)
    ->  format("Predicate ~q not found in file ~s~n", [Goal, File]),
        asserta(failed(true))).

check_file(File) :-
    file_name_extension(_, pl, File),
    lint_file(File).
check_file(File) :-
    exists_directory(File),
    atomic_list_concat([File, '/'], NewFile),
    check_dir(NewFile).
check_file(_) :-
    true.

filter_current_dir(String) :-
    sub_string(String, 0, 1, _, '.').

add_dir_name(Dir, File, NewFile) :-
    atomic_list_concat([Dir, File], NewFile).

check_dir(Dir) :-
    directory_files(Dir, Entries),
    exclude(filter_current_dir, Entries, FilteredEntries),
    maplist(add_dir_name(Dir), FilteredEntries, Files),
    forall(member(File, Files), check_file(File)).

exit_script :-
    failed(Status),
    Status = true,
    halt(1).
exit_script :-
    failed(Status),
    Status = false,
    halt(1).

lint_files :-
    asserta(failed(false)),
    check_dir('./'),
    exit_script.

