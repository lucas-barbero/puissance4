%afficher le plateau de jeu
afficherplateau(X) :- write("1 2 3 4 5 6 7"), nl, afficherGrille(X,7).


%lecture de la colonne sur laquelle jouer par le joueur J
lireColonne(J,C):- repeat, nl,
    write('Joueur '), write(J), write(' sur quelle colonne voulez vous jouer ? '),nl,
    read(C), colonneCorrecte(C), !.

%verification de l'input
colonneCorrecte(X) :-
    (   number(X),
        X >= 1,
        X =< 7
     -> true
     ;  writeln('Le num�ro de colonne doit �tre compris entre 1 et 7'),
        fail
    ).

jouerTour('X',B):-     lireColonne('X',C),
                       enregistrerCoup(C,B,'X', NB),
                       write("Apres le tour de X"),
                       afficherplateau(NB),
                       jouerTour('O',NB).

jouerTour('O',B):-     lireColonne('O',C),
                       enregistrerCoup(C,B,'O', NB),
                       afficherplateau(NB),
                       jouerTour('X',NB).

% Placement du jeton du joueur J sur la colonne C sur le board B, avec
% NB le nouveau board apr�s le coup
% enregistrerCoup(1, [L|G], J, _):- length(L,N), N >= 6, write('Coup
% Invalide\n'), jouerTour(J,[L|G]).
enregistrerCoup(1, [L|G], J, F):- write("hey"),length(L,N), N < 6, append(J,L,M), F=[M|G].
enregistrerCoup(N, [T|X], J, [T|G]):-	write("yo"), write(J),		N1 is N-1,
							enregistrerCoup(N1, X, J, G).


%lancement du jeu
puissance4:- afficherplateau([[],[],[],[],[],[],[]]),
             jouerTour('X',[[],[],[],[],[],[],[]]).



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
