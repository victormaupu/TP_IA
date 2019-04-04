:- lib(listut).

/*********************************
DESCRIPTION DU JEU DU TIC-TAC-TOE
*********************************/

/*
Une situation est decrite par une matrice 3x3.
Chaque case est soit un emplacement libre, soit contient le symbole d'un des 2 joueurs (o ou x)

Contrairement a la convention du tp pr�c�dent, pour mod�liser une case libre
dans une matrice on n'utilise pas une constante sp�ciale (ex : nil, 'vide', 'libre','inoccupee' ...);
On utilise plut�t une variable libre (_), c'est�-dire un terme non instanci� ('_').
La situation initiale est donc une matrice 3x3 composee uniquement de variables libres (_).
Ceci est possible car le jeu consiste � instancier la grille avec des symboles et non � d�placer les symbles d�j� affect�s.



Jouer un coup, c-a-d placer un symbole dans une grille S1 ne consiste pas � g�n�rer une nouvelle grille S2 obtenue
en copiant d'abord S1 puis en remplacant le symbole de case libre par le symbole du joueur, mais plus simplement
� INSTANCIER (au sens Prolog) la variable libre qui repr�sentait la case libre par la valeur associ�e au joueur, ex :
Case = Joueur, ou a realiser indirectement cette instanciation par unification via un pr�dicat comme member/2, select/3, nth1/3 ...

Ainsi si on joue un coup en S, S perd une variable libre, mais peut continuer � s'appeler S (on n'a pas besoin de la d�signer
par un nouvel identificateur).
La situation initiale est une "matrice" 3x3 (liste de 3 listes de 3 termes chacune)
o� chacun des 9 termes est une variable libre.
*/

situation_initiale([ [_,_,_],
                   [_,_,_],
                   [_,_,_] ]).

situation_test([ [o,_,_],
                   [_,_,_],
                   [_,_,x] ]).

% Convention (arbitraire) : c'est x qui commence

joueur_initial(x).


% Definition de la relation adversaire/2

adversaire(x,o).
adversaire(o,x).


/****************************************************
 DEFINIR ICI � l'aide du pr�dicat ground/1 comment
 reconnaitre une situation terminale dans laquelle il
 n'y a aucun emplacement libre : aucun joueur ne peut
 continuer � jouer (quel qu'il soit).
 ****************************************************/

situation_terminale(_Joueur, Situation) :-
  Situation = [X,Y,Z],
  ground(X),
  ground(Y),
  ground(Z).

/***************************
DEFINITIONS D'UN ALIGNEMENT
***************************/

alignement(L, Matrix) :- ligne(    L,Matrix).
alignement(C, Matrix) :- colonne(  C,Matrix).
alignement(D, Matrix) :- diagonale(D,Matrix).

/********************************************
 DEFINIR ICI chaque type d'alignement maximal
 existant dans une matrice carree NxN.
 ********************************************/


ligne(L, M) :-
  nth1(_, M, L).


colonne(C,M) :-
  length(M, L),
  colonne(C,M,I),
  I < L + 1.

colonne([],[], _).

colonne([C1|C2], [M1|M2], I) :-
  nth1(I, M1, C1),
  colonne(C2,M2,I).

/*
colonne(C,M) :-
   length(M,L),
   colonnebis(C,M,L,L).

colonnebis([],[],_).

colonnebis(C, [M1|M2], L) :-
  L>0,
  nth1(NC,M,T),
  colonnebis([T|C],M,L-1,NC).
*/

/* D�finition de la relation liant une diagonale D � la matrice M dans laquelle elle se trouve.
  il y en a 2 sortes de diagonales dans une matrice carree(https://fr.wikipedia.org/wiki/Diagonale) :
  - la premiere diagonale (principale) (descendante) : (A I)
  - la seconde diagonale  (ascendante)  : (R Z)
  A . . . . . . . Z
  . \ . . . . . / .
  . . \ . . . / . .
  . . . \ . / . . .
  . . . . X . . .
  . . . / . \ . . .
  . . / . . . \ . .
  . / . . . . . \ .
  R . . . . . . . I
*/

diagonale(D, M) :- premiere_diag(1,D,M).

diagonale(D,M) :-
   length(M,L),
   seconde_diag(L,D,M).
% ??? 2eme clause A COMPLETER

premiere_diag(_,[],[]).

premiere_diag(K,[E|D],[Ligne|M]) :-
  nth1(K,Ligne,E),
  K1 is K+1,
  premiere_diag(K1,D,M).

% definition de la seconde diagonale A COMPLETER

seconde_diag(_,[],[]).

seconde_diag(K,[E|D],[Ligne|M]) :-
	nth1(K,Ligne,E),
	K1 is K-1,
	seconde_diag(K1,D,M).


/***********************************
 DEFINITION D'UN ALIGNEMENT POSSIBLE
 POUR UN JOUEUR DONNE
 **********************************/

possible([X|L], J) :- unifiable(X,J), possible(L,J).
possible([   ], _).

/* Attention
il faut juste verifier le caractere unifiable
de chaque emplacement de la liste, mais il ne
faut pas realiser l'unification.
*/

% A FAIRE
unifiable(X,J) :- X = J.

/**********************************
 DEFINITION D'UN ALIGNEMENT GAGNANT
 OU PERDANT POUR UN JOUEUR DONNE J
 **********************************/

/*
Un alignement gagnant pour J est un alignement
possible pour J qui n'a aucun element encore libre.
Un alignement perdant pour J est gagnant
pour son adversaire.
*/

% A FAIRE

alignement_gagnant(Ali, J) :-
  possible(Ali,J).

alignement_perdant(Ali, J) :-
  adversaire(J,K),
  alignement_gagant(Ali,K).


/******************************
DEFINITION D'UN ETAT SUCCESSEUR
*******************************/

   /*Il faut definir quelle op�ration subitune matrice M representant la situation courante
lorsqu'un joueur J joue en coordonnees [L,C]
   */

% A FAIRE
successeur(J,Etat,[L,C]) :-
  nth1(L,Etat,Ligne),
  nth1(C,Ligne,Element),
  Element = J.

/**************************************
   EVALUATION HEURISTIQUE D'UNE SITUATION
   **************************************/

/*
1/ l'heuristique est +infini si la situation J est gagnante pour J
2/ l'heuristique est -infini si la situation J est perdante pour J
3/ sinon, on fait la difference entre :
   le nombre d'alignements possibles pour J
moins
   le nombre d'alignements possibles pour l'adversaire de J
*/

/*
heuristique(J,Situation,H) :-		% cas 1
 H = 10000,				% grand nombre approximant +infini
 alignement(Alig,Situation),
 alignement_gagnant(Alig,J), !.

heuristique(J,Situation,H) :-		% cas 2
 H = -10000,				% grand nombre approximant -infini
 alignement(Alig,Situation),
 alignement_perdant(Alig,J),!.
*/

% on ne vient ici que si les cut precedents n'ont pas fonctionne,
% c-a-d si Situation n'est ni perdante ni gagnante.

% A FAIRE 					cas 3
heuristique(J,Situation,H) :-
  findall(X,(alignement(Alig,Situation),possible(Alig,J)),T),
  findall(X,(adversaire(J,K),alignement(Alig,Situation),possible(Alig,K)),U),
  length(T,LP),
  length(U,LPP),
  H is LP - LPP.
