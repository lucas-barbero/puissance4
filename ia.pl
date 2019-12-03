
:- consult(puissance4).

adversaire('X','O').
adversaire('O','X').


max(X,Y,Y) :- Y>X, !.
max(X,Y,X).

maxListe([X],X) :- !, true.
maxListe([X|Y], M):- maxListe(Y, M), M >= X.
maxListe([X|Y], X):- maxListe(Y, M), X >  M.

minListe([X],X) :- !, true.
minListe([X|Y], M):- minListe(Y, M), M =< X.
minListe([X|Y], X):- minListe(Y, M), X <  M.

pair(0).
pair(X) :- X>0, X2 is X-2, pair(X2).

eq(X,Y) :- Y==X, !.


value_col(Plateau, 7, Value) :- Plateau = [Colonne|Queue], value_col(Queue, 6, Value).
value_col(Plateau, 6, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long),value_col(Queue, 5, Value1), Value is Value1+Long .
value_col(Plateau, 5, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long),value_col(Queue, 4, Value1), Value is Value1+2*Long .
value_col(Plateau, 4, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), value_col(Queue, 3, Value1), Value is Value1+3*Long .
value_col(Plateau, 3, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), value_col(Queue, 2, Value1), Value is Value1+2*Long.
value_col(Plateau, 2, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), value_col(Queue, 1, Value1), Value is Value1+Long.
value_col(Plateau, 1, Value) :- Value = 0 .


testPlateau(Plateau,Couleur,Etat):- Plateau == [[4,0,4]], Etat = 0, !.
testPlateau(Plateau,Couleur,Etat):- victoire(Plateau,Couleur), Etat = 1000.
testPlateau(Plateau,Couleur,Etat):- adversaire(Couleur,Adv), victoire(Plateau,Adv), Etat = -1000.
testPlateau(Plateau,Couleur,Etat):- value_col(Plateau, 7, Value), Etat = Value.

testerCoup(C,Plateau,Couleur,NewPlateau):- nth1(C,Plateau,L), length(L,I), I < 6, enregistrerCoup(C,Plateau,Couleur,NewPlateau).
testerCoup(C,Plateau,Couleur,NewPlateau):- NewPlateau = [[4,0,4]].
%Donne la liste des valeurs des coups suivants

listeValeur(Plateau,Couleur,Couche,Profondeur,Heuristiques):- Plateau == [[4,0,4]], Heuristiques = [0].

listeValeur(Plateau,Couleur,Couche,Profondeur,Heuristiques):-
                    eq(Couche,Profondeur),
                    testPlateau(Plateau,Couleur,E1), Heuristiques=[E1], !.


listeValeur(Plateau,Couleur,Couche,Profondeur,Heuristiques):-
                                                        \+pair(Couche), C1 is Couche + 1, adversaire(Couleur,_),
    testerCoup(1,Plateau,Couleur,N1), listeValeur(N1,Couleur,C1,Profondeur,L1), minListe(L1,M1),Li1=[M1],
    testerCoup(2,Plateau,Couleur,N2), listeValeur(N2,Couleur,C1,Profondeur,L2), minListe(L2,M2),append(Li1,[M2],Li2),
    testerCoup(3,Plateau,Couleur,N3), listeValeur(N3,Couleur,C1,Profondeur,L3), minListe(L3,M3),append(Li2,[M3],Li3),
    testerCoup(4,Plateau,Couleur,N4), listeValeur(N4,Couleur,C1,Profondeur,L4), minListe(L4,M4),append(Li3,[M4],Li4),
    testerCoup(5,Plateau,Couleur,N5), listeValeur(N5,Couleur,C1,Profondeur,L5), minListe(L5,M5),append(Li4,[M5],Li5),
    testerCoup(6,Plateau,Couleur,N6), listeValeur(N6,Couleur,C1,Profondeur,L6), minListe(L6,M6),append(Li5,[M6],Li6),
    testerCoup(7,Plateau,Couleur,N7), listeValeur(N7,Couleur,C1,Profondeur,L7), minListe(L7,M7),append(Li6,[M7],Heuristiques), !.

listeValeur(Plateau,Couleur,Couche,Profondeur,Heuristiques):-
                                                        pair(Couche), C1 is Couche + 1, adversaire(Couleur,Adv),
    testerCoup(1,Plateau,Adv,N1), listeValeur(N1,Couleur,C1,Profondeur,L1), maxListe(L1,M1),Li1=[M1],
    testerCoup(2,Plateau,Adv,N2), listeValeur(N2,Couleur,C1,Profondeur,L2), maxListe(L2,M2),append(Li1,[M2],Li2),
    testerCoup(3,Plateau,Adv,N3), listeValeur(N3,Couleur,C1,Profondeur,L3), maxListe(L3,M3),append(Li2,[M3],Li3),
    testerCoup(4,Plateau,Adv,N4), listeValeur(N4,Couleur,C1,Profondeur,L4), maxListe(L4,M4),append(Li3,[M4],Li4),
    testerCoup(5,Plateau,Adv,N5), listeValeur(N5,Couleur,C1,Profondeur,L5), maxListe(L5,M5),append(Li4,[M5],Li5),
    testerCoup(6,Plateau,Adv,N6), listeValeur(N6,Couleur,C1,Profondeur,L6), maxListe(L6,M6),append(Li5,[M6],Li6),
    testerCoup(7,Plateau,Adv,N7), listeValeur(N7,Couleur,C1,Profondeur,L7), maxListe(L7,M7),append(Li6,[M7],Heuristiques), !.

jouerCoup(N,Plateau,Couleur,NewPlateau) :- nth1(N,Plateau,L), length(L,I), I < 6, enregistrerCoup(N,Plateau,Couleur,NewPlateau), !.
jouerCoup(N,Plateau,Couleur,NewPlateau) :- N1 is N+1, jouerCoup(N1,Plateau,Couleur,NewPlateau).
jouerCoup(N,Plateau,Couleur,NewPlateau) :- write("FIN").

%Pour chaque coup regarder listeValeur (Plateau,Couleur,1,5,HeuristiquesHeuristiques) Profondeur 5 c''est bien au dessus sest trop lent et 1 car on veut le coup d''apres

jouerCoupIA(Plateau,Heuristiques,Couleur,NewPlateau) :-  maxListe(Heuristiques,M),  nth1(N,Heuristiques,M),  jouerCoup(N,Plateau,Couleur,NewPlateau), !.

setProfondeur(Plateau,P,NP) :-
    nth1(3,Plateau,L2), length(L2,I2), I2 > 3,nth1(4,Plateau,L3), length(L3,I3), I3 > 3, NP is P-2.
setProfondeur(Plateau,P,NP) :- NP is P.    

jouerTourIA('X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTourIA('O',B):- victoire(B,'X'), write("Victoire du joueur X").
jouerTourIA(_,B):- egalite(B), write("Egalite").

jouerTourIA('O',B) :-
                
                    listeValeur(B,'O',1,3,Heuristiques),
                    jouerCoupIA(B,Heuristiques,'O',NB),
                    afficherplateau(NB),
                    jouerTourIA('X',NB).

jouerTourIA('X',B) :-
                
                    listeValeur(B,'X',1,3,Heuristiques),
                    jouerCoupIA(B,Heuristiques,'X',NC),
                    afficherplateau(NC),
                    jouerTourIA('O',NC).

jouerTourIAJoueur('X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTourIAJoueur('O',B):- victoire(B,'X'), write("Victoire du joueur X").
jouerTourIAJoueur(_,B):- egalite(B), write("Egalite").

jouerTourIAJoueur('O',B) :-
                    setProfondeur(Plateau,5,NP),
                    listeValeur(B,'O',1,NP,Heuristiques),
                    jouerCoupIA(B,Heuristiques,'O',NB),
                    afficherplateau(NB),
                    jouerTourJoueurIA('X',NB).
jouerTourIAJoueur('X',B) :-
                    setProfondeur(Plateau,5,NP),
                    listeValeur(B,'X',1,NP,Heuristiques),
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


