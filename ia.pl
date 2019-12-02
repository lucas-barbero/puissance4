
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



listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):- eq(Couche,Profondeur), 
                        enregistrerCoup(1,Plateau,Couleur,N1), testPlateau(N1,Couleur,E1), L1=[E1],
                        enregistrerCoup(2,Plateau,Couleur,N2), testPlateau(N2,Couleur,E2),append(L1,[E2],L2),
                        enregistrerCoup(3,Plateau,Couleur,N3), testPlateau(N3,Couleur,E3),append(L2,[E3],L3),
                        enregistrerCoup(4,Plateau,Couleur,N4), testPlateau(N4,Couleur,E4),append(L3,[E4],L4),
                        enregistrerCoup(5,Plateau,Couleur,N5), testPlateau(N5,Couleur,E5),append(L4,[E5],L5),
                        enregistrerCoup(6,Plateau,Couleur,N6), testPlateau(N6,Couleur,E6),append(L5,[E6],L6),
                        enregistrerCoup(7,Plateau,Couleur,N7), testPlateau(N7,Couleur,E7),append(L6,[E7],Liste), !.

listeValeur(Plateau,Couleur,Couche,Profondeur,L1):- 
                                                        \+pair(Couche), C1 is Couche + 1, adversaire(Couleur,Adv),
    enregistrerCoup(1,Plateau,Couleur,N1), listeValeur(N1,Couleur,C1,Profondeur,L1), minListe(L1,M1),Li1=[M1],
    enregistrerCoup(2,Plateau,Couleur,N2), listeValeur(N2,Couleur,C1,Profondeur,L2), minListe(L2,M2),append(Li1,[M2],Li2),
    enregistrerCoup(3,Plateau,Couleur,N3), listeValeur(N3,Couleur,C1,Profondeur,L3), minListe(L3,M3),append(Li2,[M3],Li3),
    enregistrerCoup(4,Plateau,Couleur,N4), listeValeur(N4,Couleur,C1,Profondeur,L4), minListe(L4,M4),append(Li3,[M4],Li4),
    enregistrerCoup(5,Plateau,Couleur,N5), listeValeur(N5,Couleur,C1,Profondeur,L5), minListe(L5,M5),append(Li4,[M5],Li5),
    enregistrerCoup(6,Plateau,Couleur,N6), listeValeur(N6,Couleur,C1,Profondeur,L6), minListe(L6,M6),append(Li5,[M6],Li6),
    enregistrerCoup(7,Plateau,Couleur,N7), listeValeur(N7,Couleur,C1,Profondeur,L7), minListe(L7,M7),append(Li7,[M7],Liste).


listeValeur(Plateau,Couleur,Couche,Profondeur,Liste):- 
                                                        pair(Couche), C1 is Couche + 1, adversaire(Couleur,Adv), 
    enregistrerCoup(1,Plateau,Adv,N1), listeValeur(N1,Couleur,C1,Profondeur,L1), maxListe(L1,M1),Li1=[M1],
    enregistrerCoup(2,Plateau,Adv,N2), listeValeur(N2,Couleur,C1,Profondeur,L2), maxListe(L2,M2),append(Li1,[M2],Li2),
    enregistrerCoup(3,Plateau,Adv,N3), listeValeur(N3,Couleur,C1,Profondeur,L3), maxListe(L3,M3),append(Li2,[M3],Li3),
    enregistrerCoup(4,Plateau,Adv,N4), listeValeur(N4,Couleur,C1,Profondeur,L4), maxListe(L4,M4),append(Li3,[M4],Li4),
    enregistrerCoup(5,Plateau,Adv,N5), listeValeur(N5,Couleur,C1,Profondeur,L5), maxListe(L5,M5),append(Li4,[M5],Li5),
    enregistrerCoup(6,Plateau,Adv,N6), listeValeur(N6,Couleur,C1,Profondeur,L6), maxListe(L6,M6),append(Li5,[M6],Li6),
    enregistrerCoup(7,Plateau,Adv,N7), listeValeur(N7,Couleur,C1,Profondeur,L7), maxListe(L7,M7),append(Li6,[M7],Liste).
     

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




