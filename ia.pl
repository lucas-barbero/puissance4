
:- consult(puissance4).

adversaire('X','O').
adversaire('O','X').

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
coupPerdant(C,Plateau,Couleur,NewPlateau,X):- adversaire(Couleur,A), enregistrerCoup(1,Plateau,Couleur,NewPlateau), coupGagnant(X,NewPlateau,A,P).












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




