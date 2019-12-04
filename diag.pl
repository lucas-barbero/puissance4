:- consult(puissance4).


completerTrousMatriceParTirets([],[]).
completerTrousMatriceParTirets([Tete|Queue], NB) :- completerListeAvecTirets(Tete,NouvelleListe),
                                                    completerTrousMatriceParTirets(Queue, BoardComplete),
                                                    append([NouvelleListe],BoardComplete,NB).


completerListeAvecTirets(Liste,NouvelleListe) :- length(Liste,Taille),
                                      Taille < 6,
                                      append(Liste,['-'],ListeCompletee),
                                      completerListeAvecTirets(ListeCompletee,NouvelleListe), !.
completerListeAvecTirets(Liste,Liste) :- !.

test() :- completerTrousMatriceParTirets([['X','O','X','O','X','O'],['X','X','O','O','X','X'],['X','O','O','X','O'],['X','O','X','O'],['O','X','O'],['X','O'],['X']],NB),
          afficherplateau(NB).
