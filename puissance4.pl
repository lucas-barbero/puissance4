
initial(board([['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-'],
	       ['-','-','-','-','-','-']])).


%permet d'afficher une liste proprement
afficherListe([]) :- write(' ').
afficherListe([A|B]) :- write(A),write(' '), afficherListe(B).

%affiche une grille en appelant afficherListe de manière récursive
afficherGrille([]) :- write(' ').
afficherGrille([A|B]) :- afficherListe(A), write('\n'), afficherGrille(B).

%transpose la première colonne de la matrice d'entrée, on obtient donc uniquement la première ligne de la matrice voulue
transpose_col([], [], []).
transpose_col([[H|T]|R], [G|HF], [T|TF]) :- transpose_col(R, HF, TF).

%transpose tout une matrice en appelant récursivement transpose_col
transpose([[]|_], []).
transpose(T, [A|B]) :- transpose_col(T, A, C), transpose(C, B).

%afficher le plateau de jeu
afficherplateau(board(X)) :- write("1 2 3 4 5 6 7"), nl, transpose(X, Y), afficherGrille(Y).

puissance4:- initial(X), afficherplateau(X).

