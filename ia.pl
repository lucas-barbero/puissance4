
:- consult(puissance4).

% place le pion sur la colonne
move(Colonne,Plateau, NewPlateau) :- enregistrerCoup(Colonne,Plateau,'X', NewPlateau).
% vrai si placement possible sur la colonne (TODO)
move(Plateau,Colonne) :- true.

% choix du mouvement
chose_move(Plateau, 'X', Move) :-
    setof(M, move(Plateau,M), Moves),
    evaluate_and_choose(Moves,Plateau,(nil,-1000),Move).


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




