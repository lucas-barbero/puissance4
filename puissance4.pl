%afficher le plateau de jeu
afficherplateau(X) :- write("1 2 3 4 5 6 7"), nl, afficherGrille(X,7).

puissance4:- initial(X), afficherplateau(X).




afficherElement([]) :- write(' ').
afficherElement(E) :- write(E).

afficherListe([]) :- write('|').
afficherListe([E|L]) :- write('|'), afficherElement(E), afficherListe(L).

afficherGrille(_,0).
afficherGrille(G,N) :- N > 0,  N1 is N-1,
				getNthElem(G, N, L),
				afficherListe(L), write('\n'),
				afficherGrille(G,N1).

getNthElem([], N, []).
getNthElem([F|R], N, [L|LF]) :- length(F,Long),
				Long >= N,
				nth1(N, F, L),
				getNthElem(R, N, LF).

getNthElem([F|R], N, [L|LF]) :- length(F,Long),
				Long < N,
				L = ' ',
				getNthElem(R, N, LF).
