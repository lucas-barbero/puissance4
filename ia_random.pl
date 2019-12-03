
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
