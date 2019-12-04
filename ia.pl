
:- consult(puissance4).

    %adversaire(A,B)
    %Retourne la couleur de l'adversaire.
adversaire('X','O').
adversaire('O','X').

    %maxListe(Liste,X)
    %Retourne le maximum d'une liste.
maxListe([X],X) :- !, true.
maxListe([X|Y], M):- maxListe(Y, M), M >= X.
maxListe([X|Y], X):- maxListe(Y, M), X >  M.

    %minListe(Liste,X)
    %Retourne le maximum d'une liste.
minListe([X],X) :- !, true.
minListe([X|Y], M):- minListe(Y, M), M =< X.
minListe([X|Y], X):- minListe(Y, M), X <  M.

    %pair(X)
    %Retourne True si le nombre est pair, false sinon
pair(0).
pair(X) :- X>0, X2 is X-2, pair(X2).

    %Heuristiques de type 'claquée au sol'
value_col(Plateau, 7, Value) :- Plateau = [Colonne|Queue], value_col(Queue, 6, Value).
value_col(Plateau, 6, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long),value_col(Queue, 5, Value1), Value is Value1+Long .
value_col(Plateau, 5, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long),value_col(Queue, 4, Value1), Value is Value1+2*Long .
value_col(Plateau, 4, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), value_col(Queue, 3, Value1), Value is Value1+3*Long .
value_col(Plateau, 3, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), value_col(Queue, 2, Value1), Value is Value1+2*Long.
value_col(Plateau, 2, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), value_col(Queue, 1, Value1), Value is Value1+Long.
value_col(Plateau, 1, Value) :- Value = 0 .


    %testPlateau(Plateau,Couleur,Etat)
    %Evalue l'Etat dun Plateau, en fonction du joueur (Couleur) :

    % Etat -> 1000 dans ce Plateau le joueur (Couleur) gagne !
testPlateau(Plateau,Couleur,Etat):- victoire(Plateau,Couleur), Etat = 1000.
    % Etat -> -1000 dans ce Plateau l'adversaire (Adv) gagne, donc, on perd !
testPlateau(Plateau,Couleur,Etat):- adversaire(Couleur,Adv), victoire(Plateau,Adv), Etat = -1000.
    % Etat -> Dans tout les autres cas on évalue la valeur du Plateau grâce à la fonction d'évalutation.
testPlateau(Plateau,Couleur,Etat):- value_col(Plateau, 7, Value), Etat = Value.



    %testerCoup(C,Plateau,Couleur,NewPlateau)
    %Vérifie si l'on peut jouer un coup dans la Colonne C entrée en paramètre :

    %Si oui, on l'enregistre.
testerCoup(C,Plateau,Couleur,NewPlateau):- nth1(C,Plateau,L), length(L,I), I < 6, enregistrerCoup(C,Plateau,Couleur,NewPlateau).
    %Si non, on renvoie le plateau original.
testerCoup(C,Plateau,Couleur,NewPlateau):- NewPlateau = Plateau.







    %listeValeur(Plateau,Couleur,Cout,Couche,Profondeur,Heuristiques)
    %Algorithme MaxMin pour trouver le coup optimal en prévoyant les prochains coup des deux adversaires et déduire le meilleur coup a jouer maintenant.

  %Important si on atteint une situation où l'on perd, l'Heuristique de ce coup est de -1000. On ne va pas voir les coups suivants.
listeValeur(Plateau,Couleur,_,Couche,Profondeur,Heuristiques):-
                    testPlateau(Plateau,Couleur,E1), E1==1000, Heuristiques=[E1].

  %De même si on atteint une situation où l'on gagne, l'Heuristique de ce coup est de 1000. On ne va pas voir les coups suivants.
listeValeur(Plateau,Couleur,_,Couche,Profondeur,Heuristiques):-
                    testPlateau(Plateau,Couleur,E1), E1 == -1000, Heuristiques=[E1].

  %Si on atteint la Couche Maximal, on ne cherche pas à voir les coups suivants, on évalue les Plateaux les plus 'profond'.
listeValeur(Plateau,Couleur,Cout,Couche,Profondeur,Heuristiques):-
                    Couche==Profondeur,
                    testPlateau(Plateau,Couleur,E1), Heuristiques=[E1].

  %Sinon on appelle recurcivement la fonction liste valeur.
  %On test d'ajouter un pion de notre couleur durant les couches pair puis on regarde les valeurs les valeurs des Heuristiques lier a chacun de ses ajouts.
  %on descend un maximum de fois de Profondeur-Couche de départ
  %On prendra la couche 1 car on veut notre coup suivant
  %On définit que c'est nous qui jouons en suivant, donc tester coup pour les coups impairs seront avec notre couleur.
  %On prend le min des coup lié à ce que fera notre coup car l'adversaire prendra le coup qui nous est le moins favorable.
  %On créé une liste avec les Heuristiques lier à chacun des coups que l'on va jouer.
listeValeur(Plateau,Couleur,_,Couche,Profondeur,Heuristiques):-
                                                        \+pair(Couche), C1 is Couche + 1,
    testerCoup(1,Plateau,Couleur,N1), listeValeur(N1,Couleur,1,C1,Profondeur,L1), minListe(L1,M1),Li1=[M1],
    testerCoup(2,Plateau,Couleur,N2), listeValeur(N2,Couleur,2,C1,Profondeur,L2), minListe(L2,M2),append(Li1,[M2],Li2),
    testerCoup(3,Plateau,Couleur,N3), listeValeur(N3,Couleur,3,C1,Profondeur,L3), minListe(L3,M3),append(Li2,[M3],Li3),
    testerCoup(4,Plateau,Couleur,N4), listeValeur(N4,Couleur,4,C1,Profondeur,L4), minListe(L4,M4),append(Li3,[M4],Li4),
    testerCoup(5,Plateau,Couleur,N5), listeValeur(N5,Couleur,5,C1,Profondeur,L5), minListe(L5,M5),append(Li4,[M5],Li5),
    testerCoup(6,Plateau,Couleur,N6), listeValeur(N6,Couleur,6,C1,Profondeur,L6), minListe(L6,M6),append(Li5,[M6],Li6),
    testerCoup(7,Plateau,Couleur,N7), listeValeur(N7,Couleur,7,C1,Profondeur,L7), minListe(L7,M7),append(Li6,[M7],Heuristiques), !.

  %Lorsque le coup est pair ce sera le tour de l'adversaire, on définit dont la couleur joué sur celle de l'adversaire.
  %On récupère le max des valeurs de plateau possible après son jeux, car on jouera le coup qui nous est le plus favorable.
listeValeur(Plateau,Couleur,_,Couche,Profondeur,Heuristiques):-
                                                        pair(Couche), C1 is Couche + 1, adversaire(Couleur,Adv),
    testerCoup(1,Plateau,Adv,N1), listeValeur(N1,Couleur,1,C1,Profondeur,L1), maxListe(L1,M1),Li1=[M1],
    testerCoup(2,Plateau,Adv,N2), listeValeur(N2,Couleur,2,C1,Profondeur,L2), maxListe(L2,M2),append(Li1,[M2],Li2),
    testerCoup(3,Plateau,Adv,N3), listeValeur(N3,Couleur,3,C1,Profondeur,L3), maxListe(L3,M3),append(Li2,[M3],Li3),
    testerCoup(4,Plateau,Adv,N4), listeValeur(N4,Couleur,4,C1,Profondeur,L4), maxListe(L4,M4),append(Li3,[M4],Li4),
    testerCoup(5,Plateau,Adv,N5), listeValeur(N5,Couleur,5,C1,Profondeur,L5), maxListe(L5,M5),append(Li4,[M5],Li5),
    testerCoup(6,Plateau,Adv,N6), listeValeur(N6,Couleur,6,C1,Profondeur,L6), maxListe(L6,M6),append(Li5,[M6],Li6),
    testerCoup(7,Plateau,Adv,N7), listeValeur(N7,Couleur,7,C1,Profondeur,L7), maxListe(L7,M7),append(Li6,[M7],Heuristiques), !.

  %jouerCoup(N,Plateau,Couleur,NewPlateau)
  %On va jouer le coup qui a l'Heuristique la plus forte, si le coup en possible.
jouerCoup(N,Plateau,Couleur,NewPlateau) :- nth1(N,Plateau,L), length(L,I), I < 6, enregistrerCoup(N,Plateau,Couleur,NewPlateau), !.
  %Si la Colonne est deja pleine, on joue dans la deuxieme valeurs à droite
jouerCoup(N,Plateau,Couleur,NewPlateau) :- N1 is N+1, jouerCoup(N1,Plateau,Couleur,NewPlateau).
  %On revient a gauche de la liste si on arrive en fin'.
jouerCoup(N,Plateau,Couleur,NewPlateau) :- N > 7, N1 = 1.

  %L'IA va jouer le meilleur coup.
jouerCoupIA(Plateau,Heuristiques,Couleur,NewPlateau) :-  maxListe(Heuristiques,M),  nth1(N,Heuristiques,M),  jouerCoup(N,Plateau,Couleur,NewPlateau), !.



  %setProfondeur(Plateau,P,NP)
  %On peut rencontrer des problème de mémoire, si les colonnes du milieux sont pleines, on peut baisser la Profondeur des calculs.
setProfondeur(Plateau,P,NP) :-

    nth1(5,Plateau,L5), length(L5,I5), I5 > 5, nth1(4,Plateau,L3), length(L3,I3), I3 > 5, nth1(3,Plateau,L4), length(L4,I4), I4 > 5, NP is P-1.
  %Sinon on laisse la même.
setProfondeur(Plateau,P,NP) :- NP is P.




%Jeux IA vs IA

%Avant chaque début de tour, on regarde si l'un des joueurs a gagné.
jouerTourIA('X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTourIA('O',B):- victoire(B,'X'), write("Victoire du joueur X").
jouerTourIA(_,B):- egalite(B), write("Egalite").

jouerTourIA('O',B) :-
                    %Réglage sur la Profondeur
                    setProfondeur(B,5,NP),
                    %On évalue les coups possibles
                    listeValeur(B,'O',0,1,NP,Heuristiques),
                    %On joue le meilleur
                    jouerCoupIA(B,Heuristiques,'O',NB),
                    %On affiche
                    afficherplateau(NB),
                    %On fait jouer l'adversaire
                    jouerTourIA('X',NB).

jouerTourIA('X',B) :-
                    %Réglage sur la Profondeur
                    setProfondeur(B,5,NP),
                    %On évalue les coups possibles
                    listeValeur(B,'X',0,1,NP,Heuristiques),
                    %On joue le meilleur
                    jouerCoupIA(B,Heuristiques,'X',NC),
                    %On affiche
                    afficherplateau(NC),
                    %On fait jouer l'adversaire
                    jouerTourIA('O',NC).



% Jeux IA vs joueur

jouerTourIAJoueur('X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTourIAJoueur('O',B):- victoire(B,'X'), write("Victoire du joueur X").
jouerTourIAJoueur(_,B):- egalite(B), write("Egalite").

jouerTourIAJoueur('O',B) :-
                    setProfondeur(B,5,NP),
                    listeValeur(B,'O',0,1,NP,Heuristiques),
                    jouerCoupIA(B,Heuristiques,'O',NB),
                    afficherplateau(NB),
                    jouerTourJoueurIA('X',NB).
jouerTourIAJoueur('X',B) :-
                    setProfondeur(B,5,NP),
                    listeValeur(B,'X',0,1,NP,Heuristiques),
                    jouerCoupIA(B,Heuristiques,'X',NB),
                    afficherplateau(NB),
                    jouerTourJoueurIA('O',NB).

jouerTourJoueurIA('X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTourJoueurIA('O',B):- victoire(B,'X'), write("Victoire du joueur X").
jouerTourJoueurIA(_,B):- egalite(B), write("Egalite").

jouerTourJoueurIA('O',B) :- repeat,
							lireColonne('O',C),
							verifierCoup(C,B),
							enregistrerCoup(C,B,'O', NB),
							afficherplateau(NB),
							jouerTourIAJoueur('O',NB).

jouerTourJoueurIA('X',B) :- repeat,
							lireColonne('X',C),
							verifierCoup(C,B),
							enregistrerCoup(C,B,'X', NB),
							afficherplateau(NB),
							jouerTourIAJoueur('O',NB).


%lancement du jeu
puissance4IA:- afficherplateau([[],[],[],[],[],[],[]]),
               jouerTourIA('X',[[],[],[],[],[],[],[]]).

puissance4JoueurIA:- afficherplateau([[],[],[],[],[],[],[]]),
               jouerTourJoueurIA('X',[[],[],[],[],[],[],[]]),
               afficherplateau([[],[],[],[],[],[],[]]).

matrice_poids([[3,4,5,5,4,3],[4,6,8,8,6,4],[5,8,11,11,8,5],[7,10,13,13,10,7],[5,8,11,11,8,5],[4,6,8,8,6,4],[3,4,5,5,4,3]]).

heuristique(Plateau, Couleur, NumColonne, Value) :-
                   FacteurDeuxCases is 2,
                   FacteurTroisCases is 5,
                   verif2CasesVerticales(Plateau, Couleur, NumColonne, Value1),
                   verif3CasesVerticales(Plateau, Couleur, NumColonne, Value2),
                   verif2CasesHorizontales(Plateau, Couleur, NumColonne, NbVictoirePossible1),
                   verif3CasesHorizontales(Plateau, Couleur, NumColonne, NbVictoirePossible2),
                   Value is ((Value1 + NbVictoirePossible1) * FacteurDeuxCases +
                            (Value2 + NbVictoirePossible2) * FacteurTroisCases),
                   write(Value).

verif2CasesVerticales(Plateau, Couleur, NumColonne,Value) :-
                   nth1(NumColonne,Plateau,Colonne),
                   append(Colonne,[Couleur],NouvelleColonne),
                   length(NouvelleColonne,Taille),
                   Taille2 is Taille-1,
                   Taille2 > 0,
                   nth1(Taille2,NouvelleColonne,Item),
                   Item == Couleur,
                   pasJetonEnDessous(NouvelleColonne,Couleur,Taille2),
                   Taille2+3 =< 6, Value = 1, !.

verif2CasesVerticales(_,_,_,Value) :- Value =0.


verif3CasesVerticales(Plateau, Couleur, NumColonne, Value) :-
                   nth1(NumColonne,Plateau,Colonne),
                   append(Colonne,[Couleur],NouvelleColonne),
                   length(NouvelleColonne,Taille),
                   TaillePrecedente is Taille-1,
                   TaillePrecedente > 1,
                   nth1(TaillePrecedente,Colonne,Item),
                   Item == Couleur,
                   SousTailleDeux is TaillePrecedente-1,
                   nth1(SousTailleDeux,Colonne,Item2),
                   Item2 == Couleur,
                   pasJetonEnDessous(Colonne,Couleur,SousTailleDeux),
                   TaillePrecedente+2 =< 6, Value = 1, !.

verif3CasesVerticales(_,_,_,Value) :- Value =0.


pasJetonEnDessous(_,_,Taille) :- Taille == 1, !.
pasJetonEnDessous(Colonne,Couleur,Taille) :-
                   Taille >= 2,
                   EnDessous is Taille-1,
                   nth1(EnDessous,Colonne,Item2),
                   Couleur \= Item2.

verif2CasesHorizontales(Plateau, Couleur, NumColonne, NbVictoirePossible) :-
                   nth1(NumColonne,Plateau, Colonne),
                   length(Colonne, Length),
                   genererLigne(Length, Plateau, Ligne),
                   verif2CasesHorizontaleRec1(Ligne, Couleur, NbVictoirePossible).


verif2CasesHorizontaleRec1(Ligne, J, Cpt):-
                   verif2CasesHorizontaleRec2(Ligne, J, Cpt1),
                   estSousListeIncr([J,'-','-',J],Ligne, Incr),
                   Cpt is Cpt1+Incr.

verif2CasesHorizontaleRec2(Ligne, J, Cpt):-
                   verif2CasesHorizontaleRec3(Ligne, J, Cpt1),
                   estSousListeIncr([J,J,'-','-'],Ligne,Incr),
                   Cpt is Cpt1+Incr.

verif2CasesHorizontaleRec3(Ligne, J, Cpt):-
                   verif2CasesHorizontaleRec4(Ligne, J, Cpt1),
                   estSousListeIncr([J,'-',J,'-'],Ligne, Incr),
                   Cpt is Cpt1+Incr.

verif2CasesHorizontaleRec4(Ligne, J, Cpt):-
                   verif2CasesHorizontaleRec5(Ligne, J, Cpt1),
                   estSousListeIncr(['-',J,'-',J],Ligne, Incr),
                   Cpt is Cpt1+Incr.

verif2CasesHorizontaleRec5(Ligne, J, Cpt):-
                   verif2CasesHorizontaleRec6(Ligne, J, Cpt1),
                   estSousListeIncr(['-',J,J,'-'],Ligne, Incr),
                   Cpt is Cpt1+Incr.

verif2CasesHorizontaleRec6(Ligne, J, Cpt):-
                   estSousListe(['-','-',J,J],Ligne),
                   Cpt is 1.

verif2CasesHorizontaleRec6(Ligne, J, Cpt):-
                   \+estSousListe(['-','-',J,J],Ligne),
                   Cpt is 0.


estSousListeIncr(SousListe, Ligne, Incr) :-
                   estSousListe(SousListe,Ligne),
                   Incr is 1.

estSousListeIncr(SousListe, Ligne, Incr) :-
                   \+estSousListe(SousListe,Ligne),
                   Incr is 0.

verif3CasesHorizontales(Plateau, Couleur, NumColonne, NbVictoirePossible) :-
                   nth1(NumColonne,Plateau, Colonne),
                   length(Colonne, Length),
                   genererLigne(Length, Plateau, Ligne),
                   verif3CasesHorizontaleRec1(Ligne, Couleur, NbVictoirePossible).

verif3CasesHorizontaleRec1(Ligne, J, Cpt):-
                   verif2CasesHorizontaleRec2(Ligne, J, Cpt1),
                   estSousListeIncr([J,J,'-',J],Ligne, Incr),
                   Cpt is Cpt1+Incr.

verif3CasesHorizontaleRec2(Ligne, J, Cpt):-
                   verif2CasesHorizontaleRec3(Ligne, J, Cpt1),
                   estSousListeIncr([J,J,J,'-'],Ligne,Incr),
                   Cpt is Cpt1+Incr.

verif3CasesHorizontaleRec3(Ligne, J, Cpt):-
                   verif2CasesHorizontaleRec4(Ligne, J, Cpt1),
                   estSousListeIncr([J,'-',J,J],Ligne, Incr),
                   Cpt is Cpt1+Incr.

verif3CasesHorizontaleRec4(Ligne, J, Cpt):-
                   estSousListe(['-',J,J,J],Ligne),
                   Cpt is 1.

verif3CasesHorizontaleRec4(Ligne, J, Cpt):-
                   \+estSousListe(['-',J,J,J],Ligne),
                   Cpt is 0.



diagonaleEnColonne(B, NColonne, [L|LF]) :-	niemeElement(NColonne, B, ColonneInser),
						length(ColonneInser, LColonne),
						N1 is max(1, NColonne-6+LColonne),
						niemeElement(N1, B, PremiereColonne),
						length(PremiereColonne, Long),
						Row is LColonne+(NColonne-N1),
						Long >= Row,
						nth1(Row, PremiereColonne, L),
						N2 is N1+1,
						diagonaleEnColonne2(B, N2, NColonne, LF).

diagonaleEnColonne(B, NColonne, [L|LF]) :-	niemeElement(NColonne, B, ColonneInser),
						length(ColonneInser, LColonne),
						N1 is max(1, NColonne-6+LColonne),
						niemeElement(N1, B, PremiereColonne),
						length(PremiereColonne, Long),
						Row is LColonne+(NColonne-N1),
						Long < Row,
						nth1(NColonne-N1, PremiereColonne, L),
						N2 is N1+1,
						diagonaleEnColonne2(B, N2, NColonne, LF).


diagonaleEnColonne2(B, N, NColonne, [L|LF]) :-	N < NColonne+6,
						niemeElement(NColonne, B, ColonneInser),
						niemeElement(N, B, PremiereColonne),
						length(ColonneInser, LColonne),
						length(PremiereColonne,Long),
						Row is LColonne+(NColonne-N),
						Long >= Row,
						nth1(Row, PremiereColonne, L),
						N2 is N+1,
						diagonaleEnColonne2(B, N2, NColonne, LF).

diagonaleEnColonne2(B, N, _, []).



diagonaleEnColonneSO(B, NColonne, [L|LF]) :-	niemeElement(NColonne, B, ColonneInser),
						length(ColonneInser, LColonne),
						N1 is min(7, NColonne+6-LColonne),
						write(N1),
						niemeElement(N1, B, PremiereColonne),
						write("bonjour"),
						length(PremiereColonne, Long),
						Row is LColonne+(N1-NColonne),
						Long > Row,
						nth1(Row, PremiereColonne, L),
						N2 is N1-1,
						diagonaleEnColonne2(B, N2, NColonne, LF).

diagonaleEnColonneSO(B, NColonne, [L|LF]) :-	niemeElement(NColonne, B, ColonneInser),
						length(ColonneInser, LColonne),
						N1 is min(7, NColonne+6-LColonne),
						write(N1),
						niemeElement(N1, B, PremiereColonne),
						write("bonjour"),
						
						length(PremiereColonne, Long),
						Row is LColonne+(N1-NColonne),
						Long =< Row,
						K is N1-NColonne,
						nth1(K, PremiereColonne, L),
						N2 is N1-1,
						diagonaleEnColonne2SO(B, N2, NColonne, LF).


diagonaleEnColonne2SO(B, N, NColonne, [L|LF]) :-	N > NColonne-6,
						niemeElement(NColonne, B, ColonneInser),
						niemeElement(N, B, PremiereColonne),
						length(ColonneInser, LColonne),
						length(PremiereColonne,Long),
						Row is LColonne+(N-NColonne),
						Long >= Row,
						nth1(Row, PremiereColonne, L),
						N2 is N-1,
						diagonaleEnColonne2SO(B, N2, NColonne, LF).

diagonaleEnColonne2SO(B, N, _, []).




