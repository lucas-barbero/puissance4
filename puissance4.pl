

%afficher le plateau de jeu
afficherplateau(X) :- write("1 2 3 4 5 6 7"), nl, afficherGrille(X,6).


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
     ;  writeln('Le numero de colonne doit etre compris entre 1 et 7'),
        fail
    ).
jouerTour('X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTour('O',B):- victoire(B,'X'), write("Victoire du joueur X").

jouerTour('X',B):-     repeat,
                       lireColonne('X',C),
                       verifierCoup(C,B),
                       enregistrerCoup(C,B,'X', NB),
                       afficherplateau(NB),
                       jouerTour('O',NB).

jouerTour('O',B):-    repeat,
                       lireColonne('O',C),
                       verifierCoup(C,B),
                       enregistrerCoup(C,B,'O', NB),
                       afficherplateau(NB),
                       jouerTour('X',NB).

% verification de la taille de liste pour savoir si on peut encore y
% poser un jeton
verifierCoup(C,B) :-     (
    nth1(C,B,L),
        length(L,I),
        I < 6
     -> true
     ;  writeln('Coup impossible place inssuffisante'),
        afficherplateau(B),
        fail
    ).
% Placement du jeton du joueur J sur la colonne C sur le board B=[L|G],
% avec NB le nouveau board apr�s le coup
enregistrerCoup(1, [L|G], J, NB):- append(L,[J],M), NB=[M|G].
enregistrerCoup(N, [T|X], J, [T|G]):-
                                       N1 is
                                       N-1,
						enregistrerCoup(N1, X, J, G).




afficherElement([]) :- write(' ').
afficherElement(E) :- write(E).

afficherListe([]) :- write('|').
afficherListe([E|L]) :- write('|'), afficherElement(E), afficherListe(L).

afficherGrille(_,0).
afficherGrille(G,N) :- N > 0,  N1 is N-1,
				getNthElem(G, N, L),
				afficherListe(L), write('\n'),
				afficherGrille(G,N1).

% Renvoie une liste contenant le Ne element (ou un espace) de chaque
% liste de la grille en param
getNthElem([], _, []).
getNthElem([F|R], N, [L|LF]) :- length(F,Long),
				Long >= N,
				nth1(N, F, L),
				getNthElem(R, N, LF).

getNthElem([F|R], N, [L|LF]) :- length(F,Long),
				Long < N,
				L = ' ',
				getNthElem(R, N, LF).


 % Renvoie le nième élément de la liste.
 % Renvoie '-' si N est
 % supérieur à la longueur de la liste.

niemeElement(N, Ligne, '-'):- \+ nth1(N, Ligne, _).
niemeElement(N, Ligne, Couleur):- nth1(N, Ligne, Couleur).


 %
 % Verifie que la SousListe est comprise dans la Liste.
 %
 % Appeler cette fonction avec une variable generale à la place de SousListe
 % permet de lister toutes les sous listes de Liste. %
 estSousListe(SousListe,Liste):-append(SousListe,_,Liste).
 estSousListe(SousListe,[_|Queue]):-estSousListe(SousListe,Queue).


genererLigne(N, Grille, Ligne):- maplist(niemeElement(N), Grille, Ligne).



% ----- Conditions de victoire pour les joueurs -----


% Teste si il y a 4 même couleur sur une colonne
victoireVerticale([Colonne|_],Couleur):- estSousListe([Couleur,Couleur,Couleur,Couleur],
                                                      Colonne),!.
victoireVerticale([_|SousGrille],Couleur):- victoireVerticale(SousGrille,Couleur).

% Teste si il y a 4 même couleur sur une ligne,
% Pour cela, il récupéres chaques ligne de niveau N avec génererLigne
victoireHorizontaleRec(N, Grille, Couleur):- genererLigne(N, Grille, Ligne),
                                             estSousListe([Couleur,Couleur,Couleur,Couleur],Ligne),
                                             !.

victoireHorizontaleRec(N, Grille, Couleur):- N < 7,
                                             N1 is N + 1,
                                             victoireHorizontaleRec(N1, Grille, Couleur).

victoireHorizontale(Grille, Couleur):- victoireHorizontaleRec(1, Grille, Couleur).



% On cherche le pattern H1 H2 H3 H4 dans la grille tel que ces 4 valeurs soient la tête d'une sous liste de la grille, on regarde ensuite que ces têtes soient positionnées en forme de diago de gauche vers la droite
victoireDiagonale1(Grille,Couleur):- append(_,[H1,H2,H3,H4|_],Grille),
		   append(I1,[Couleur|_],H1),
		   append(I2,[Couleur|_],H2),
		   append(I3,[Couleur|_],H3),
		   append(I4,[Couleur|_],H4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1+1, M3 is M2+1, M4 is M3+1.

% On cherche le pattern H1 H2 H3 H4 dans la grille tel que ces 4 valeurs soient la tête d'une sous liste de la grille, on regarde ensuite que ces têtes soient positionnées en forme de diago de droite vers la gauche
victoireDiagonale2(Grille,Couleur):- append(_,[H1,H2,H3,H4|_],Grille),
		   append(I1,[Couleur|_],H1),
		   append(I2,[Couleur|_],H2),
		   append(I3,[Couleur|_],H3),
		   append(I4,[Couleur|_],H4),
		   length(I1,M1), length(I2,M2), length(I3,M3), length(I4,M4),
		   M2 is M1-1, M3 is M2-1, M4 is M3-1.


victoire(Grille, Couleur) :- victoireHorizontale(Grille,Couleur).
victoire(Grille, Couleur) :- victoireVerticale(Grille,Couleur).
victoire(Grille, Couleur) :- victoireDiagonale1(Grille,Couleur).
victoire(Grille, Couleur) :- victoireDiagonale2(Grille,Couleur).

%lancement du jeu
puissance4:- afficherplateau([[],[],[],[],[],[],[]]),
             jouerTour('X',[[],[],[],[],[],[],[]]).
