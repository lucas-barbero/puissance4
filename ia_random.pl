
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
