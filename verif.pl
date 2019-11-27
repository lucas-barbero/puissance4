
/**
 * Verifie que la SousListe est comprise dans la Liste.
 *
 * Appeler cette fonction avec une variable generale à la place de SousListe
 * permet de lister toutes les sous listes de Liste.
 */
estSousListe(SousListe,Liste):-append(SousListe,_,Liste).
estSousListe(SousListe,[_|Queue]):-estSousListe(SousListe,Queue).

/**
 *
 *
 */
victoireVerticale([Colonne|_],Couleur):- estSousListe([Couleur,Couleur,Couleur,Couleur], Colonne),!.
victoireVerticale([_|SousGrille],Couleur):- victoireVerticale(SousGrille,Couleur).


/**
 *
 *
 */

%victoireHorizontale(Grille, Couleur):- victoireHorizontaleLigne(N, Grille, Couleur).


%victoireHorizontaleLigne(N, Grille, Couleur):-


genererLigne(_,[],_).
genererLigne(N,[Colonne|SousGrille],Ligne):- \+ nth1(N,Colonne,_), append(Ligne,['-'],Ligne), genererLigne(N,SousGrille,Ligne).
genererLigne(N,[Colonne|SousGrille],Ligne):- nth1(N,Colonne,Couleur), append(Ligne,[Couleur],Ligne), genererLigne(N,SousGrille,Ligne).
