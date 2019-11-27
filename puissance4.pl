% :- use_module(library(clpfd)).

initial(board([['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-']])).


%transpose la première colonne de la matrice d'entrée, on obtient donc uniquement la première ligne de la matrice voulue
transpose_col([], [], []).
transpose_col([[H|T]|R], [H|HF], [T|TF]) :- transpose_col(R, HF, TF).

%transpose tout une matrice en appelant récursivement transpose_col
transpose([[]|_], []).
transpose(T, [A|B]) :- transpose_col(T, A, C), transpose(C, B).

%afficher le plateau de jeu
afficherplateau(board(X)) :- write("1 2 3 4 5 6 7"), nl, afficherGrille(Y).

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