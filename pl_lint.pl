:- initialization(lint_files).

:- use_module(library(prolog_xref)).
:- use_module(library(prolog_clause), [predicate_name/2]).
:- use_module(library(apply), [exclude/3, maplist/3]).
:- use_module(library(lists), [member/2]).

:- dynamic failed/1.

load_rules :-
    catch(['./.lint_config.pl'], _, true).

goal_not_available(File) :-
    forall((xref_called(File, Goal, _, _, LineNumber),
            \+ xref_defined(File, Goal, _),
            term_to_atom(Goal, GoalAtom),
            \+ sub_atom(GoalAtom, _, _, _, 'clpfd:'),
            \+ sub_atom(GoalAtom, 0, _, _, '\'$'),
            \+ sub_atom(GoalAtom, _, _, _, 'file_search_path'),
            \+ sub_atom(GoalAtom, _, _, _, 'hup('),
            \+ sub_atom(GoalAtom, _, _, _, 'user:file_search_path'),
            predicate_name(Goal, PredName),
            \+ catch(ignore_predicate(PredName), _, false),
            asserta(failed(true))
           ), (
    format("ERROR: ~s:~d| Predicate ~q not found~n", [File, LineNumber, PredName])
           )).

% TODO: This doesn't work with meta-predicates like mapm and DCG
%predicate_unused(File) :-
%    forall(
%        (   xref_defined(File, Goal, local(_)),
%            \+ xref_exported(File, Goal),
%            \+ xref_called(File, Goal, _, _, _),
%            predicate_name(Goal, PredName),
%            \+ PredName = "test/2",
%            \+ catch(ignore_predicate(PredName), _, false),
%            asserta(failed(true))
%           ), (
%    format("ERROR: ~s| Predicate ~q unused~n", [File, PredName])
%    )).

lint_file(File) :-
    xref_source(File),
    goal_not_available(File).
%    predicate_unused(File).

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

