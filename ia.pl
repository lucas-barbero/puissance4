
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

testPlateau(Plateau,Couleur,Etat):- victoire(Plateau,Couleur), Etat = 1.
testPlateau(Plateau,Couleur,Etat):- adversaire(Couleur,Adv), victoire(Plateau,Adv), Etat = -1.
testPlateau(_,_,Etat):- Etat = 0.

testerCoup(C,Plateau,Couleur,NewPlateau):- nth1(C,Plateau,L), length(L,I), I < 8, enregistrerCoup(C,Plateau,Couleur,NewPlateau).
testerCoup(C,Plateau,Couleur,NewPlateau):- NewPlateau = [4,0,4].

%Donne la liste des valeurs des coups suivants


listeValeur(_,_,_,_,Liste):- false,
                    Liste=[-1], !.

listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):-
                   Plateau == [4,0,4], Liste=[-1], !.

listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):-
                    eq(Couche,Profondeur),
                    testPlateau(Plateau,Couleur,E1), Liste=[E1], !.

listeValeur(Plateau,Couleur,_,_,Liste):-
                    testPlateau(Plateau,Couleur,E1),
                    eq(E1,1), Liste=[1], !.

listeValeur(Plateau,Couleur,_,_,Liste):-
                    testPlateau(Plateau,Couleur,E1),
                    eq(E1,-1), Liste=[-1], !.


listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):-
                                                        \+pair(Couche), C1 is Couche + 1, adversaire(Couleur,_),
    testerCoup(1,Plateau,Couleur,N1), listeValeur(N1,Couleur,C1,Profondeur,L1), minListe(L1,M1),Li1=[M1],
    testerCoup(2,Plateau,Couleur,N2), listeValeur(N2,Couleur,C1,Profondeur,L2), minListe(L2,M2),append(Li1,[M2],Li2),
    testerCoup(3,Plateau,Couleur,N3), listeValeur(N3,Couleur,C1,Profondeur,L3), minListe(L3,M3),append(Li2,[M3],Li3),
    testerCoup(4,Plateau,Couleur,N4), listeValeur(N4,Couleur,C1,Profondeur,L4), minListe(L4,M4),append(Li3,[M4],Li4),
    testerCoup(5,Plateau,Couleur,N5), listeValeur(N5,Couleur,C1,Profondeur,L5), minListe(L5,M5),append(Li4,[M5],Li5),
    testerCoup(6,Plateau,Couleur,N6), listeValeur(N6,Couleur,C1,Profondeur,L6), minListe(L6,M6),append(Li5,[M6],Li6),
    testerCoup(7,Plateau,Couleur,N7), listeValeur(N7,Couleur,C1,Profondeur,L7), minListe(L7,M7),append(Li6,[M7],Liste), !.

listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):-
                                                        pair(Couche), C1 is Couche + 1, adversaire(Couleur,Adv),
    testerCoup(1,Plateau,Adv,N1), listeValeur(N1,Couleur,C1,Profondeur,L1), maxListe(L1,M1),Li1=[M1],
    testerCoup(2,Plateau,Adv,N2), listeValeur(N2,Couleur,C1,Profondeur,L2), maxListe(L2,M2),append(Li1,[M2],Li2),
    testerCoup(3,Plateau,Adv,N3), listeValeur(N3,Couleur,C1,Profondeur,L3), maxListe(L3,M3),append(Li2,[M3],Li3),
    testerCoup(4,Plateau,Adv,N4), listeValeur(N4,Couleur,C1,Profondeur,L4), maxListe(L4,M4),append(Li3,[M4],Li4),
    testerCoup(5,Plateau,Adv,N5), listeValeur(N5,Couleur,C1,Profondeur,L5), maxListe(L5,M5),append(Li4,[M5],Li5),
    testerCoup(6,Plateau,Adv,N6), listeValeur(N6,Couleur,C1,Profondeur,L6), maxListe(L6,M6),append(Li5,[M6],Li6),
    testerCoup(7,Plateau,Adv,N7), listeValeur(N7,Couleur,C1,Profondeur,L7), maxListe(L7,M7),append(Li6,[M7],Liste), !.


value_col(Plateau, 7, Value) :- Plateau = [Colonne|Queue], value_col(Queue, 6, Value).
value_col(Plateau, 6, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long),value_col(Queue, 5, Value1), Value is Value1+Long .
value_col(Plateau, 5, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long),value_col(Queue, 4, Value1), Value is Value1+2*Long .
value_col(Plateau, 4, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), value_col(Queue, 3, Value1), Value is Value1+3*Long .
value_col(Plateau, 3, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), value_col(Queue, 2, Value1), Value is Value1+2*Long.
value_col(Plateau, 2, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), value_col(Queue, 1, Value1), Value is Value1+Long.
value_col(Plateau, 1, Value) :- Value = 0 .


calculHeuristique(Plateau,Couleur,N,V,H) :- eq(V,-1), H = -1.
calculHeuristique(Plateau,Couleur,N,V,H) :- nth1(N,Plateau,L), length(L,I), I > 5, H = -1.
calculHeuristique(Plateau,Couleur,N,V,H) :- enregistrerCoup(N,Plateau,Couleur,NP), value_col(NP, 7,H).


listerHeuristique(Plateau,Couleur,Liste,Heuristiques) :- maxListe(Liste,M), M > 0, Heuristiques=Liste, !.
listerHeuristique(Plateau,Couleur,Liste,Heuristiques) :- minListe(Liste,M), M < 0, Heuristiques=Liste, !.
listerHeuristique(Plateau,Couleur,Liste,Heuristiques) :- niemeElement(1,Liste,V1), niemeElement(2,Liste,V2), niemeElement(3,Liste,V3),
                                                 niemeElement(4,Liste,V4), niemeElement(5,Liste,V5), niemeElement(6,Liste,V6), niemeElement(7, Liste, V7),  
        calculHeuristique(Plateau,Couleur,1,V1,E1), L1=[E1],
        calculHeuristique(Plateau,Couleur,2,V2,E2), append(L1,[E2],L2),
        calculHeuristique(Plateau,Couleur,3,V3,E3), append(L2,[E3],L3),
        calculHeuristique(Plateau,Couleur,4,V4,E4), append(L3,[E4],L4),
        calculHeuristique(Plateau,Couleur,5,V5,E5), append(L4,[E5],L5),
        calculHeuristique(Plateau,Couleur,6,V6,E6), append(L5,[E6],L6),
        calculHeuristique(Plateau,Couleur,7,V7,E7), append(L6,[E7],Heuristiques), !.


%Pour chaque coup regarder listeValeur (Plateau,Couleur,1,5,Liste) Profondeur 5 c''est bien au dessus sest trop lent et 1 car on veut le coup d''apres

jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :-  listerHeuristique(Plateau,Couleur,Liste,Heuristiques), maxListe(Heuristiques,M),  nth1(N,Heuristiques,M),  enregistrerCoup(N,Plateau,Couleur,NewPlateau), !.

setProfondeur(Plateau,Profondeur,NewProfondeur) :- nth1(4,Plateau,L), length(L,I), I > 5, nth1(3,Plateau,L1), length(L1,I1), I1 > 5,Profondeur > 4, NewProfondeur is Profondeur - 1.
setProfondeur(Plateau,Profondeur,NewProfondeur) :- nth1(4,Plateau,L), length(L,I), I > 5, nth1(3,Plateau,L1), length(L1,I1), I1 > 5,
    nth1(2,Plateau,L2), length(L2,I2), I2 > 5, nth1(6,Plateau,L3), length(L3,I3), I1 > 5, Profondeur > 3, NewProfondeur is Profondeur - 1.
setProfondeur(Plateau,Profondeur,NewProfondeur) :- NewProfondeur is Profondeur.

jouerTourIA('X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTourIA('O',B):- victoire(B,'X'), write("Victoire du joueur X").
jouerTourIA(_,B):- egalite(B), write("Egalite").

jouerTourIA('O',B) :-
                    setProfondeur(B,5,P),
                    listeValeur(B,'O',1,P,Liste),
                    jouerCoupIA(B,Liste,'O',NB),
                    afficherplateau(NB),
                    jouerTourIA('X',NB).

jouerTourIA('X',B) :-
                    setProfondeur(B,5,P),
                    listeValeur(B,'X',1,P,Liste),
                    jouerCoupIA(B,Liste,'X',NC),
                    afficherplateau(NC),
                    jouerTourIA('O',NC).

jouerTourIAJoueur('X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTourIAJoueur('O',B):- victoire(B,'X'), write("Victoire du joueur X").
jouerTourIAJoueur(_,B):- egalite(B), write("Egalite").

jouerTourIAJoueur('O',B) :-
				    setProfondeur(B,5,P),
                    listeValeur(B,'O',1,P,Liste),
                    jouerCoupIA(B,Liste,'O',NB),
                    afficherplateau(NB),
                    jouerTourJoueurIA('X',NB).
jouerTourIAJoueur('X',B) :-
				    setProfondeur(B,5,P),
                    listeValeur(B,'X',1,P,Liste),
                    jouerCoupIA(B,Liste,'X',NB),
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
               jouerTourJoueurIA('X',[[],[],[],[],[],[],[]]).

