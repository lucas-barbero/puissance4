
:- consult(ia).

% VerifierCoup sans affichage
verifierCoupBis(C,B) :-     (
    nth1(C,B,L),
        length(L,I),
        I < 6
     -> true
     ;
        fail
    ).


choisirColonneAleatoireValide(C,B) :- random(1,7,C), verifierCoupBis(C,B), !.
choisirColonneAleatoireValide(C,B) :- choisirColonneAleatoireValide(C,B).


% Jeux IA vs random

jouerTourIARand('X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTourIARand('O',B):- victoire(B,'X'), write("Victoire du joueur X").
jouerTourIARand(_,B):- egalite(B), write("Egalite").

jouerTourIARand('O',B) :-
                    setProfondeur(B,5,NP),
                    listeValeur(B,'O',0,1,NP,Heuristiques),
                    jouerCoupIA(B,Heuristiques,'O',NB),
                    afficherplateau(NB),
                    jouerTourIARand('X',NB).

jouerTourIARand('X',B) :-
                    choisirColonneAleatoireValide(C,B),
                    enregistrerCoup(C,B,'X',NB),
                    afficherplateau(NB),
                    jouerTourIARand('O',NB).



%lancement du jeu
puissance4IAvsIARand:- afficherplateau([[],[],[],[],[],[],[]]),
               jouerTourIARand('X',[[],[],[],[],[],[],[]]).

%-------- plusieurs jeux sans affichage pour les statistiques ----------


jouerTourIARandSansAffichage('X',B,NbVicX,NbVicO,NbVicX,NewNbO):- victoire(B,'O'), write("Victoire du joueur O\n"), NewNbO is NbVicO + 1.
jouerTourIARandSansAffichage('O',B,NbVicX,NbVicO,NewNbX,NbVicO):- victoire(B,'X'), write("Victoire du joueur X\n"), NewNbX is NbVicX + 1.
jouerTourIARandSansAffichage(_,B,NbVicX,NbVicO,NbVicX,NbVicO):- egalite(B), write("Egalite\n").

jouerTourIARandSansAffichage('O',B, NbVicX,NbVicO,NbVicX,NbVicO) :-
                    setProfondeur(B,5,NP),
                    listeValeur(B,'O',0,1,NP,Heuristiques),
                    jouerCoupIA(B,Heuristiques,'O',NB),
                    jouerTourIARandSansAffichage('X',NB,NbVicX,NbVicO,NewX,NewO).

jouerTourIARandSansAffichage('X',B, NbVicX,NbVicO,NbVicX,NbVicO) :-
                    choisirColonneAleatoireValide(C,B),
                    enregistrerCoup(C,B,'X',NB),
                    jouerTourIARandSansAffichage('O',NB,NbVicX,NbVicO,NewX,NewO).



%lancement du jeu
puissance4IAvsIARandStat:- write('Début des parties\n'),
                          joueJeux(0,0,ResVicX,ResVicO),
                          write('--- Resultats ---\n'),
                          write('Nb victoires X : '),
                          write(ResVicX),
                          write(', Nb victoires O : \n').

joueJeux(NbVicX,NbVicO,NbVicX,NbVicO) :- NbVicX > 49.
joueJeux(NbVicX,NbVicO,NbVicX,NbVicO) :- NbVicO > 49.
joueJeux(NbVicX,NbVicO,ResVicX,ResVicO) :- jouerTourIARandSansAffichage('X',[[],[],[],[],[],[],[]],NbVicX,NbVicO,NewX,NewO),
                                          joueJeux(NewX,NewO,ResVicX,ResVicO).
