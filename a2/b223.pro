rel(a,b).
rel(b,c).
rels(X,Y):-
	rel(A,B),
	rel(B,C).
