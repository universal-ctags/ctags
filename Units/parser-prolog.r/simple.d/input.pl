%% Taken from https://en.wikipedia.org/wiki/Prolog
mother_child(trude, sally).

father_child(tom, sally).
father_child(tom, erica).
father_child(mike, tom).

sibling(X, Y)      :- parent_child(Z, X), parent_child(Z, Y).

parent_child(X, Y) :- father_child(X, Y).
parent_child(X, Y) :- mother_child(X, Y).

% dummy0()
/* dummy1() */
/* /* dummy2() */ */
not_dummy.

'quoted predicate'(
X
) :-
	father_child(tom, X).

rel(X,Y,Z) :- Z =:= X + Y + 0.25.
rel(X,Y) :- Y =:= X + 0.25.
rel(X) :- X =:= 100_000.

rel0(X) :- X =:= 100_000 0.
rel0(X, 100_000_11, Y) :- X =:= Y.
