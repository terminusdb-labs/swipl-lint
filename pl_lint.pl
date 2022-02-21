:- initialization(lint_files).

:- use_module(library(prolog_xref)).
:- use_module(library(prolog_clause), [predicate_name/2]).
:- use_module(library(apply), [exclude/3, maplist/3]).
:- use_module(library(lists), [member/2]).

:- dynamic failed/1.

load_rules :-
    catch(['./.lint_config.pl'], _, true).

check_goal(GoalAtom, _, _) :-
    split_string(GoalAtom, ':', "", [Module, Goal]),
    !,
    atom_string(ModuleAtom, Module),
    atom_string(ModuleGoalAtom, Goal),
    module_property(ModuleAtom, file(File)),
    xref_source(File),
    term_to_atom(ModuleGoalTerm, ModuleGoalAtom),
    \+ xref_defined(File, ModuleGoalTerm, _).
check_goal(GoalAtom, File, Goal) :-
    \+ xref_defined(File, Goal, _),
    \+ sub_atom(GoalAtom, 0, _, _, '\'$'),
    \+ sub_atom(GoalAtom, _, _, _, 'file_search_path'),
    \+ sub_atom(GoalAtom, _, _, _, 'hup(').

goal_not_available(File) :-
    forall((xref_called(File, Goal, _, _, LineNumber),
            term_to_atom(Goal, GoalAtom),
            check_goal(GoalAtom, File, Goal),
            predicate_name(Goal, PredName),
            \+ catch(ignore_predicate(PredName), _, false),
            asserta(failed(true))
           ), (
    format("ERROR: ~s:~d| Predicate ~q not found~n", [File, LineNumber, PredName])
    )).

lint_file(File) :-
    xref_source(File),
    goal_not_available(File).

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
    halt(0).

lint_files :-
    load_rules,
    asserta(failed(false)),
    check_dir('./'),
    exit_script.

