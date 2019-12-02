
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

eq(X,Y) :- Y=:=X, !.

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


%Pour chaque coup regarder listeValeur (Plateau,Couleur,1,5,Liste) Profondeur 5 c''est bien au dessus sest trop lent et 1 car on veut le coup d''apres

jouerCoupIA(Plateau,Couleur,NewPlateau) :-  maxListe(Liste,M), eq(M,1), nth1(N,Liste,1),  enregistrerCoup(N,Plateau,Couleur,NewPlateau), !.

jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :-   nth1(4,Plateau,L), length(L,I), I < 6, maxListe(Liste,M), nth1(4,Liste,M), enregistrerCoup(4,Plateau,Couleur,NewPlateau), !.
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :-   nth1(3,Plateau,L), length(L,I), I < 6, maxListe(Liste,M), nth1(3,Liste,M), enregistrerCoup(3,Plateau,Couleur,NewPlateau), !.
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :-   nth1(5,Plateau,L), length(L,I), I < 6, maxListe(Liste,M), nth1(5,Liste,M), enregistrerCoup(5,Plateau,Couleur,NewPlateau), !.
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :-   nth1(2,Plateau,L), length(L,I), I < 6, maxListe(Liste,M), nth1(2,Liste,M), enregistrerCoup(2,Plateau,Couleur,NewPlateau), !.
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :-   nth1(6,Plateau,L), length(L,I), I < 6, maxListe(Liste,M), nth1(6,Liste,M), enregistrerCoup(6,Plateau,Couleur,NewPlateau), !.
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :-   nth1(1,Plateau,L), length(L,I), I < 6, maxListe(Liste,M), nth1(1,Liste,M), enregistrerCoup(1,Plateau,Couleur,NewPlateau), !.
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :-   nth1(7,Plateau,L), length(L,I), I < 6, maxListe(Liste,M), nth1(7,Liste,M), enregistrerCoup(7,Plateau,Couleur,NewPlateau), !.

jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :- maxListe(Liste,M), eq(M,-1), nth1(4,Liste,-1),  enregistrerCoup(4,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :- maxListe(Liste,M), eq(M,-1), nth1(3,Liste,-1),  enregistrerCoup(3,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :- maxListe(Liste,M), eq(M,-1), nth1(5,Liste,-1),  enregistrerCoup(5,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :- maxListe(Liste,M), eq(M,-1), nth1(2,Liste,-1),  enregistrerCoup(2,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :- maxListe(Liste,M), eq(M,-1), nth1(6,Liste,-1),  enregistrerCoup(6,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :- maxListe(Liste,M), eq(M,-1), nth1(1,Liste,-1),  enregistrerCoup(1,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Liste,Couleur,NewPlateau) :- maxListe(Liste,M), eq(M,-1), nth1(7,Liste,-1),  enregistrerCoup(7,Plateau,Couleur,NewPlateau), !.

setProfondeur(Plateau,Profondeur,NewProfondeur) :- nth1(4,Plateau,L), length(L,I), I > 6, nth1(3,Plateau,L1), length(L1,I1), I1 > 6, NewProfondeur is Profondeur - 1.
setProfondeur(Plateau,Profondeur,NewProfondeur) :- NewProfondeur is Profondeur.

jouerTourIA('O',B) :-
                    setProfondeur(B,5,P),
                    listeValeur(B,'O',1,P,Liste),
                    jouerCoupIA(B,Liste,'O',NB),
                    afficherplateau(NB),
                    jouerTourIA('X',NB).

jouerTourIA('X',B1) :-
                    setProfondeur(B,5,P),
                    listeValeur(B1,'X',1,P,Liste),
                    jouerCoupIA(B1,Liste,'X',NC),
                    afficherplateau(NC),
                    jouerTourIA('O',NC).
jouerTourIAJoueur('O',B) :-
				    jouerCoupIA(B,'O',NB),
					jouerTourJoueurIA('X',NB).
					afficherplateau(NB),
				    jouerCoupIA(B,'X',NB),
jouerTourIAJoueur('X',B) :-
					afficherplateau(NB),
					jouerTourJoueurIA('O',NB).
jouerTourJoueurIA('O',B) :-
							lireColonne('O',C),
							repeat,
							verifierCoup(C,B),
							enregistrerCoup(C,B,'O', NB),
							jouerTourIAJoueur('X',NB).
							afficherplateau(NB),
							jouerTourIAJoueur('O',NB).
							afficherplateau(NB),
							enregistrerCoup(C,B,'X', NB),
							verifierCoup(C,B),
							lireColonne('X',C),
jouerTourJoueurIA('X',B) :-
							repeat,


%lancement du jeu
puissance4IA:- afficherplateau([[],[],[],[],[],[],[]]),
               jouerTourIA('X',[[],[],[],[],[],[],[]]).
			   
puissance4JoueurIA:- afficherplateau([[],[],[],[],[],[],[]]),
               jouerTourJoueurIA('X',[[],[],[],[],[],[],[]]),
               afficherplateau([[],[],[],[],[],[],[]]).


