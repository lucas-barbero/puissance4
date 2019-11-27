
/**
 * Renvoie le nième élément de la liste.
 * Renvoie '-' si N est
 * supérieur à la longueur de la liste.
 *
 */
niemeElement(N, Ligne, '-'):- \+ nth1(N, Ligne, _).
niemeElement(N, Ligne, Couleur):- nth1(N, Ligne, Couleur).


/**
 * Verifie que la SousListe est comprise dans la Liste.
 *
 * Appeler cette fonction avec une variable generale à la place de SousListe
 * permet de lister toutes les sous listes de Liste.
 */
estSousListe(SousListe,Liste):-append(SousListe,_,Liste).
estSousListe(SousListe,[_|Queue]):-estSousListe(SousListe,Queue).


genererLigne(N, Grille, Ligne):- maplist(niemeElement(N), Grille, Ligne).


/**
 *
 *
 */
victoireVerticale([Colonne|_],Couleur):- estSousListe([Couleur,Couleur,Couleur,Couleur],
                                                      Colonne),!.
victoireVerticale([_|SousGrille],Couleur):- victoireVerticale(SousGrille,Couleur).

victoireHorizontaleRec(N, Grille, Couleur):- genererLigne(N, Grille, Ligne),
                                             estSousListe([Couleur,Couleur,Couleur,Couleur],Ligne),
                                             !.

victoireHorizontaleRec(N, Grille, Couleur):- N < 7,
                                             N1 is N + 1,
                                             victoireHorizontaleRec(N1, Grille, Couleur).

victoireHorizontale(Grille, Couleur):- victoireHorizontaleRec(1, Grille, Couleur).
