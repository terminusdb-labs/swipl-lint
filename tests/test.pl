main :-
   X = 'test',
   lists:memberchk(X, [X]),
   memberchk(X, [X]),
   member(X, [X]),
   lists:nonExistingPredicate.
