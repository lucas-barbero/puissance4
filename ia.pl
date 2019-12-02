
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
testPlateau(Plateau,Couleur,Etat):- Etat = 0.


%Donne la liste des valeurs des coups suivants


listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):- false,
                    Liste=[-1].

listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):-
                    eq(Couche,Profondeur),
                    testPlateau(Plateau,Couleur,E1), Liste=[E1].

listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):-
                    testPlateau(Plateau,Couleur,E1),
                    eq(E1,1), Liste=[1].

listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):-
                    testPlateau(Plateau,Couleur,E1),
                    eq(E1,-1), Liste=[-1].



listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):-
                                                        \+pair(Couche), C1 is Couche + 1, adversaire(Couleur,Adv),
    enregistrerCoup(1,Plateau,Couleur,N1), listeValeur(N1,Couleur,C1,Profondeur,L1), minListe(L1,M1),Li1=[M1],
    enregistrerCoup(2,Plateau,Couleur,N2), listeValeur(N2,Couleur,C1,Profondeur,L2), minListe(L2,M2),append(Li1,[M2],Li2),
    enregistrerCoup(3,Plateau,Couleur,N3), listeValeur(N3,Couleur,C1,Profondeur,L3), minListe(L3,M3),append(Li2,[M3],Li3),
    enregistrerCoup(4,Plateau,Couleur,N4), listeValeur(N4,Couleur,C1,Profondeur,L4), minListe(L4,M4),append(Li3,[M4],Li4),
    enregistrerCoup(5,Plateau,Couleur,N5), listeValeur(N5,Couleur,C1,Profondeur,L5), minListe(L5,M5),append(Li4,[M5],Li5),
    enregistrerCoup(6,Plateau,Couleur,N6), listeValeur(N6,Couleur,C1,Profondeur,L6), minListe(L6,M6),append(Li5,[M6],Li6),
    enregistrerCoup(7,Plateau,Couleur,N7), listeValeur(N7,Couleur,C1,Profondeur,L7), minListe(L7,M7),append(Li6,[M7],Liste).

listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):-
                                                        pair(Couche), C1 is Couche + 1, adversaire(Couleur,Adv),
    enregistrerCoup(1,Plateau,Adv,N1), listeValeur(N1,Couleur,C1,Profondeur,L1), maxListe(L1,M1),Li1=[M1],
    enregistrerCoup(2,Plateau,Adv,N2), listeValeur(N2,Couleur,C1,Profondeur,L2), maxListe(L2,M2),append(Li1,[M2],Li2),
    enregistrerCoup(3,Plateau,Adv,N3), listeValeur(N3,Couleur,C1,Profondeur,L3), maxListe(L3,M3),append(Li2,[M3],Li3),
    enregistrerCoup(4,Plateau,Adv,N4), listeValeur(N4,Couleur,C1,Profondeur,L4), maxListe(L4,M4),append(Li3,[M4],Li4),
    enregistrerCoup(5,Plateau,Adv,N5), listeValeur(N5,Couleur,C1,Profondeur,L5), maxListe(L5,M5),append(Li4,[M5],Li5),
    enregistrerCoup(6,Plateau,Adv,N6), listeValeur(N6,Couleur,C1,Profondeur,L6), maxListe(L6,M6),append(Li5,[M6],Li6),
    enregistrerCoup(7,Plateau,Adv,N7), listeValeur(N7,Couleur,C1,Profondeur,L7), maxListe(L7,M7),append(Li6,[M7],Liste).


% Pour chaque coup regarder listeValeur (Plateau,Couleur,1,6,Liste)
% Profondeur 6 c''est bien au dessus sest trop lent et 1 car on veut le
% coup d'apres

jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,1), numeroelement,  enregistrerCoup(N,Plateau,Couleur,N1).

jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,0), nth1(4,Liste,0),  enregistrerCoup(4,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,0), nth1(3,Liste,0),  enregistrerCoup(3,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,0), nth1(5,Liste,0),  enregistrerCoup(5,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,0), nth1(2,Liste,0),  enregistrerCoup(2,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,0), nth1(6,Liste,0),  enregistrerCoup(6,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,0), nth1(1,Liste,0),  enregistrerCoup(1,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,0), nth1(7,Liste,0),  enregistrerCoup(7,Plateau,Couleur,NewPlateau).

jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,-1), nth1(4,Liste,-1),  enregistrerCoup(4,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,-1), nth1(3,Liste,-1),  enregistrerCoup(3,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,-1), nth1(5,Liste,-1),  enregistrerCoup(5,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,-1), nth1(2,Liste,-1),  enregistrerCoup(2,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,-1), nth1(6,Liste,-1),  enregistrerCoup(6,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,-1), nth1(1,Liste,-1),  enregistrerCoup(1,Plateau,Couleur,NewPlateau).
jouerCoupIA(Plateau,Couleur,NewPlateau) :- listeValeur(Plateau,Couleur,1,6,Liste), maxListe(Liste,M), eq(M,-1), nth1(7,Liste,-1),  enregistrerCoup(7,Plateau,Couleur,NewPlateau).



% place le pion sur la colonne
move(Colonne,Plateau, NewPlateau) :- enregistrerCoup(Colonne,Plateau,'X', NewPlateau).
% vrai si placement possible sur la colonne (TODO)
move(Plateau,Colonne) :- true.

% choix du mouvement
chose_move(Plateau, 'X', Move) :-
    setof(M, move(Plateau,M), Moves),
    evaluate_and_choose(Moves,Plateau,(nil,-1000),Move).


% Coup permettant de gagner
coupGagnant(C,Plateau,Couleur,NewPlateau):- enregistrerCoup(1,Plateau,Couleur,NewPlateau), victoire(NewPlateau,Couleur), C=1.
coupGagnant(C,Plateau,Couleur,NewPlateau):- enregistrerCoup(2,Plateau,Couleur,NewPlateau), victoire(NewPlateau,Couleur), C=2.
coupGagnant(C,Plateau,Couleur,NewPlateau):- enregistrerCoup(3,Plateau,Couleur,NewPlateau), victoire(NewPlateau,Couleur), C=3.
coupGagnant(C,Plateau,Couleur,NewPlateau):- enregistrerCoup(4,Plateau,Couleur,NewPlateau), victoire(NewPlateau,Couleur), C=4.
coupGagnant(C,Plateau,Couleur,NewPlateau):- enregistrerCoup(5,Plateau,Couleur,NewPlateau), victoire(NewPlateau,Couleur), C=5.
coupGagnant(C,Plateau,Couleur,NewPlateau):- enregistrerCoup(6,Plateau,Couleur,NewPlateau), victoire(NewPlateau,Couleur), C=6.
coupGagnant(C,Plateau,Couleur,NewPlateau):- enregistrerCoup(7,Plateau,Couleur,NewPlateau), victoire(NewPlateau,Couleur), C=7.

% Si un coup direct permettrait à l''adversaire de gagner on le fait
coupDefensif(C,Plateau,Couleur,NewPlateau):- adversaire(Couleur,A), coupGagnant(C,Plateau,A,P), enregistrerCoup(C,Plateau,Couleur,NewPlateau).

% Verifie que le coup qu''on fait ne va pas faire gagner l''adversaire
%C : Colonne dans laquelle on joue X:Colonne qui fait gagner l''adversaire si on joue en C
coupPerdant(C,Plateau,Couleur,NewPlateau,X):- adversaire(Couleur,A), enregistrerCoup(1,Plateau,Couleur,NewPlateau), coupGagnant(X,NewPlateau,A,P), C=1.
coupPerdant(C,Plateau,Couleur,NewPlateau,X):- adversaire(Couleur,A), enregistrerCoup(2,Plateau,Couleur,NewPlateau), coupGagnant(X,NewPlateau,A,P), C=2.
coupPerdant(C,Plateau,Couleur,NewPlateau,X):- adversaire(Couleur,A), enregistrerCoup(3,Plateau,Couleur,NewPlateau), coupGagnant(X,NewPlateau,A,P), C=3.
coupPerdant(C,Plateau,Couleur,NewPlateau,X):- adversaire(Couleur,A), enregistrerCoup(4,Plateau,Couleur,NewPlateau), coupGagnant(X,NewPlateau,A,P), C=4.
coupPerdant(C,Plateau,Couleur,NewPlateau,X):- adversaire(Couleur,A), enregistrerCoup(5,Plateau,Couleur,NewPlateau), coupGagnant(X,NewPlateau,A,P), C=5.
coupPerdant(C,Plateau,Couleur,NewPlateau,X):- adversaire(Couleur,A), enregistrerCoup(6,Plateau,Couleur,NewPlateau), coupGagnant(X,NewPlateau,A,P), C=6.
coupPerdant(C,Plateau,Couleur,NewPlateau,X):- adversaire(Couleur,A), enregistrerCoup(7,Plateau,Couleur,NewPlateau), coupGagnant(X,NewPlateau,A,P), C=7.













/*
 * choisit le BestMove dans l'ensemble des Moves à partir de la Position
 * courante;
 * Moves : colonnes ou il est possible de jouer
 * Position : Plateau de jeu
 * Record enregistre le meilleur mouvement courant : paire (Move, Value)
 * BestMove : renvoie la colonne qui correspond au Record final
 */

evaluate_and_choose([Move|Moves], Plateau, Record, BestMove) :-
    move(Move,Plateau, newPlateau),
    value(newPlateau, Value),
    update(Move , Value, Record, newRecord),
    evaluate_and_choose(Moves, Plateau, newRecord, BestMove).

evaluate_and_choose([], Position, (Move,Value), Move) .

% n'update pas si value est inf a celle du Record
update(Move , Value, Record, newRecord) :-
    Record = (MoveRecord, ValueRecord),
    Value =< ValueRecord,
    newRecord = Record.

% update si supérieur
update(Move , Value, Record, newRecord) :-
    Record = (MoveRecord, ValueRecord),
    Value > ValueRecord,
    newRecord = (Move,Value).

value(Plateau, Value) :- victoire(Plateau, ('X')), Value=1000.
value(Plateau, Value) :- victoire(Plateau, ('O')), Value='-1000'.
value(Plateau, Value) :- Value=0.

% renvoie le nombre de pions sur les colonnes ponderes par la position
% (plus fort au milieu)
value_col(Plateau, 7, Value) :- Plateau = [Colonne|Queue], Value=0, value_col(Queue, 6, Value), !.

value_col(Plateau, 6, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), Value1 is Value+Long, value_col(Queue, 5, Value1), ! .
value_col(Plateau, 5, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), Value1 is Value+2*Long,value_col(Queue, 4, Value1), ! .

value_col(Plateau, 4, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), Value1 is Value+3*Long, value_col(Queue, 3, Value1),! .

value_col(Plateau, 3, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), Value1 is Value+2*Long, value_col(Queue, 2, Value1), ! .

value_col(Plateau, 2, Value) :- Plateau = [Colonne|Queue], length(Colonne, Long), Value1 is Value+Long, value_col(Queue, 1, Value1), ! .

value_col(Plateau, 1, Value) .
