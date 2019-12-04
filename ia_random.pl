
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

jouerTourIARand(_,'X',B):- victoire(B,'O'), write("Victoire du joueur O").
jouerTourIARand(_,'O',B):- victoire(B,'X'), write("Victoire du joueur X").
jouerTourIARand(_,_,B):- egalite(B), write("Egalite").

jouerTourIARand(TypeHeuristique,'O',B) :-
                    setProfondeur(B,5,NP),
                    jouerCoupIA(TypeHeuristique,B,NP,'O',NB),
                    afficherplateau(NB),
                    jouerTourIARand(TypeHeuristique,'X',NB).

jouerTourIARand(TypeHeuristique,'X',B) :-
                    choisirColonneAleatoireValide(C,B),
                    enregistrerCoup(C,B,'X',NB),
                    afficherplateau(NB),
                    jouerTourIARand(TypeHeuristique,'O',NB).

%lancement du jeu typeHeuristique : 1 : avancee - 2: milieu defensive
%Joueur X : Random Joueur O : MinMax
puissance4IAvsIARand(TypeHeuristique):- afficherplateau([[],[],[],[],[],[],[]]),
               jouerTourIARand(TypeHeuristique,'X',[[],[],[],[],[],[],[]]).

%         IA VS RAND
%-------- plusieurs jeux sans affichage pour les statistiques ----------


jouerTourIARandSansAffichage(TypeHeuristique,'X',B,NbVicX,NbVicO,NbVicX,NewNbO):- victoire(B,'O'), write("Victoire du joueur O\n"), NewNbO is NbVicO + 1.
jouerTourIARandSansAffichage(TypeHeuristique,'O',B,NbVicX,NbVicO,NewNbX,NbVicO):- victoire(B,'X'), write("Victoire du joueur X\n"), NewNbX is NbVicX + 1.
jouerTourIARandSansAffichage(TypeHeuristique,_,B,NbVicX,NbVicO,NbVicX,NbVicO):- egalite(B), write("Egalite\n").

jouerTourIARandSansAffichage(TypeHeuristique,'O',B, NbVicX,NbVicO,NbVicX,NbVicO) :-
                    setProfondeur(B,5,NP),
                    jouerCoupIA(TypeHeuristique,B,NP,'O',NB),
                    jouerTourIARandSansAffichage(TypeHeuristique,'X',NB,NbVicX,NbVicO,NewX,NewO).

jouerTourIARandSansAffichage(TypeHeuristique,'X',B, NbVicX,NbVicO,NbVicX,NbVicO) :-
                    choisirColonneAleatoireValide(C,B),
                    enregistrerCoup(C,B,'X',NB),
                    jouerTourIARandSansAffichage(TypeHeuristique,'O',NB,NbVicX,NbVicO,NewX,NewO).



%lancement du jeu
puissance4IAvsIARandStat(TypeHeuristique):- write('Début des parties\n'),
                          joueJeuxIAvsRand(TypeHeuristique,0,0,ResVicX,ResVicO),
                          write('--- Resultats ---\n'),
                          write('Nb victoires X : '),
                          write(ResVicX),
                          write(', Nb victoires O : \n').


joueJeuxIAvsRand(TypeHeuristique,NbVicX,NbVicO,ResVicX,ResVicO) :- NbVicX < 10, NbVicO < 10, jouerTourIARandSansAffichage(TypeHeuristique,'X',[[],[],[],[],[],[],[]],NbVicX,NbVicO,NewX,NewO),
                                          joueJeuxIAvsRand(TypeHeuristique,NewX,NewO,ResVicX,ResVicO),!.
joueJeuxIAvsRand(TypeHeuristique,NbVicX,NbVicO,NbVicX,NbVicO).
%         IA VS IA
%-------- plusieurs jeux sans affichage pour les statistiques ----------
jouerTourIAvsIASansAffichage('X',B,NbVicX,NbVicO,NbVicX,NewNbO):- victoire(B,'O'), write("Victoire du joueur O\n"), NewNbO is NbVicO + 1.
jouerTourIAvsIASansAffichage('O',B,NbVicX,NbVicO,NewNbX,NbVicO):- victoire(B,'X'), write("Victoire du joueur X\n"), NewNbX is NbVicX + 1.
jouerTourIAvsIASansAffichage(_,B,NbVicX,NbVicO,NbVicX,NbVicO):- egalite(B), write("Egalite\n").

jouerTourIAvsIASansAffichage('O',B, NbVicX,NbVicO,NbVicX,NbVicO) :-
                    setProfondeur(B,5,NP),
                    jouerCoupIA(1,B,NP,'O',NB),
                    jouerTourIAvsIASansAffichage('X',NB,NbVicX,NbVicO,NewX,NewO).

jouerTourIAvsIASansAffichage('X',B, NbVicX,NbVicO,NbVicX,NbVicO) :-
                    setProfondeur(B,5,NP),
                    jouerCoupIA(2,B,NP,'X',NB),
                    jouerTourIAvsIASansAffichage('O',NB,NbVicX,NbVicO,NewX,NewO).



%lancement du jeu
puissance4IAvsIA():- write('Début des parties\n'),
                          joueJeuxIAvsIA(0,0,ResVicX,ResVicO),
                          write('--- Resultats ---\n'),
                          write('Nb victoires X : '),
                          write(ResVicX),
                          write(', Nb victoires O : \n').


joueJeuxIAvsIA(NbVicX,NbVicO,ResVicX,ResVicO) :- NbVicX < 10, NbVicO < 10, jouerTourIAvsIASansAffichage('X',[[],[],[],[],[],[],[]],NbVicX,NbVicO,NewX,NewO),
                                          joueJeuxIAvsIA(NewX,NewO,ResVicX,ResVicO),!.
joueJeuxIAvsIA(NbVicX,NbVicO,NbVicX,NbVicO).
