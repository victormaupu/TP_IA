%*******************************************************************************
%                                    AETOILE
%*******************************************************************************

/*
Rappels sur l'algorithme

- structures de donnees principales = 2 ensembles : P (etat pendants) et Q (etats clos)
- P est dedouble en 2 arbres binaires de recherche equilibres (AVL) : Pf et Pu

   Pf est l'ensemble des etats pendants (pending states), ordonnes selon
   f croissante (h croissante en cas d'egalite de f). Il permet de trouver
   rapidement le prochain etat a developper (celui qui a f(U) minimum).

   Pu est le meme ensemble mais ordonne lexicographiquement (selon la donnee de
   l'etat). Il permet de retrouver facilement n'importe quel etat pendant

   On gere les 2 ensembles de fa�on synchronisee : chaque fois qu'on modifie
   (ajout ou retrait d'un etat dans Pf) on fait la meme chose dans Pu.

   Q est l'ensemble des etats deja developpes. Comme Pu, il permet de retrouver
   facilement un etat par la donnee de sa situation.
   Q est modelise par un seul arbre binaire de recherche equilibre.

Predicat principal de l'algorithme :

   aetoile(Pf,Pu,Q)

   - reussit si Pf est vide ou bien contient un etat minimum terminal
   - sinon on prend un etat minimum U, on genere chaque successeur S et les valeurs g(S) et h(S)
	 et pour chacun
		si S appartient a Q, on l'oublie
		si S appartient a Ps (etat deja rencontre), on compare
			g(S)+h(S) avec la valeur deja calculee pour f(S)
			si g(S)+h(S) < f(S) on reclasse S dans Pf avec les nouvelles valeurs
				g et f
			sinon on ne touche pas a Pf
		si S est entierement nouveau on l'insere dans Pf et dans Ps
	- appelle recursivement etoile avec les nouvelles valeurs NewPF, NewPs, NewQs

*/

%*******************************************************************************

:- ['avl.pl'].       % predicats pour gerer des arbres bin. de recherche
:- ['taquin.pl'].    % predicats definissant le systeme a etudier

%*******************************************************************************

main :-
  %On initialise les valeurs
  initial_state(S0),
  heuristique(S0,H0),
  G0 is 0,
  F0 is G0 + H0,
  empty(Pf),
  empty(Pu),
  empty(Q),
  insert([ [F0, H0, G0], S0],Pf,Pf1),
  insert( [ S0, [F0,H0,G0], nil, nil ] ,Pu, Pu1),
  aetoile(Pf1,Pu1,Q).



%*******************************************************************************
%Si Pf et Pu sont vides, il n'y a pas de solution
aetoile([], [], _) :-
  writeln("Pas de solution !").

aetoile(_,_,Q) :-
	% Si l'état final est dans Q, alors on a trouvé la solution
	final_state(Sf),
	belongs([Sf,_,_,_], Q),
  affiche_solution(Q, Sf).

%Cas principal
aetoile(Pf,Pu,Q) :-
  %Pour éviter que l'algorithme continue son exécution après avoir trouvé la solution
  final_state(Fs),
  not(belongs([Fs,_,_,_],Q)),
  %On récupère et supprime le noeud de coût minimal dans Pf
  suppress_min(NoeudCourant,Pf,Pf1),
  %On détaille la structure du noeud courant
  NoeudCourant = [[F,H,G],Etat],
  %Le noeud à supprimer de Pu
  NoeudDetail = [Etat,[F,H,G],_,_],
  %On retire le noeud courant de l'arbre Pu et on récupère le Père et l'action
  %qui a mené à ce noeud
  suppress(NoeudDetail,Pu,Pu1),
  %On récupère tous les successeurs
  expand(NoeudCourant, ListeS),
  %On traite les successeurs
  loop_successors(ListeS,Q,Pu1,Pf1,PuNew,PfNew),
  %On sauvegarde le noeud courant dans Q
  insert(NoeudDetail,Q, Qnew),
  %Itération suivante
  aetoile(PfNew,PuNew,Qnew).

%Trouve tous les successeurs de l'état courant et leur coût
expand(U,ListeS) :-
  U = [[_,_,G],Pere],
  findall( [Fils,C,Pere,Action],
           (rule(Action,Cout,Pere,Fils),
            cost(Fils,G,Cout,C)
  ),
  ListeS).

%Calcule le coût d'un successeur sous forme de triplet :
%[F : somme de H et G, H : estimation du coût restant, G : coût depuis le début]
%Cout est le cout d'une action (ici 1 pour le taquin)
cost(U,G,Cout,C):-
  heuristique(U,Hs),
  Gs is G + Cout,
  Fs is Gs + Hs,
  C = [Fs,Hs,Gs].

%On a parcouru tous les successeurs
%PuNew et PfNew prennent les valeurs de Pu et Pf
loop_successors([],_,Pu,Pf,Pu,Pf).

%On traite chaque successeur 1 par 1
loop_successors([S|ListeS],Q,Pu,Pf,PuNew,PfNew) :-
  proceed_successor(S,Q,Pu,Pf,Pu1,Pf1),
  loop_successors(ListeS,Q,Pu1, Pf1, PuNew,PfNew) .

%Cas simple, le successeur est déjà dans Q, dans ce cas on passe
proceed_successor(S,Q,_,_,_,_) :-
  belongs(S,Q).

%Cas où le successeur est déjà dans Pu
proceed_successor(S,_,Pu,Pf,PuNew,PfNew) :-
  %Version actuelle du successeur
  S = [Etat,C,_,_],
  %ancienne version du successeur dans Pu
  X = [Etat,Cx,_,_],
  belongs(X,Pu),
  %C est le nouveau coût, Cx est l'ancien
  %Si le nouveau est inférieur à l'ancien, on met à jour Pu et Pf
  (C @< Cx ->
    %On supprime l'ancienne version
    suppress(X,Pu,Pu1),
    suppress([Cx,Etat],Pf,Pf1),
    %On insère la nouvelle
    insert(S,Pu1,PuNew),
    insert([C,Etat],Pf1,PfNew)
    ;
    %Sinon on garde les versions actuelles de Pu et Pf
    PuNew=Pu,
    PfNew=Pf
  ).

%Cas où le successeur courant n'est ni dans Q ni dans Pu et Pf
proceed_successor(S,_,Pu,Pf,PuNew,PfNew) :-
  S = [Etat,C,_,_],
  insert(S,Pu,PuNew),
  insert([C,Etat],Pf,PfNew).

%Affichage de la solution, on s'arette quand on est sur l'état initial
affiche_solution(_,S) :-
  initial_state(S),
  writeln("SOLUTION :"),
  write("Initial State : "),
  writeln(S).

%Affichage de la solution, on s'arette quand on est sur l'état initial
affiche_solution(Q,Etat) :-
  belongs([Etat,_,P,A],Q),
  affiche_solution(Q,P),
  write(" + "),
  write(A),
  write(" -> "),
  writeln(Etat),
  (final_state(Etat) ->
    writeln("--- FIN ---");write("")).
