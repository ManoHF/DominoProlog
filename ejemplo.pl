
ejemplo:-
	nb_setval(lista, [1,2,3,4,5,6,7,8,9,0]),
	nb_getval(lista, K),
	write(K),
	nb_setval(lista, [28,2,3,4,5,6,7,8,9,0]). % el nuevo valor de tablero es [28,2,3,4,5,6,7,8,9,0]


main:- 
	nb_setval(numFichas, 7),
	nb_setval(numFichasOp, 7),
	nb_setval(tablero, [[1,2],[2,5],[5,0]]),
	nb_setval(miTurno, 1),
	write("ingresa tus fichas"), nl,
	read(A), nl,
	nb_setval(fichas, A),
	nb_getval(fichas, A),                  
	write(A), nl,
	nb_getval(miTurno, T),
	jugar(T).

jugar(1):-
	nb_getval(fichas, Fichas),
	nb_getval(tablero, [A|B]),
	last(B, C),
	write([A, C]), nl,
	write(Fichas), nl,
	tirar(Fichas, [A,C]),
	nb_setval(miTurno, 0).

tirar(Fichas, [A|B]):-
	tirarCabeza(Fichas, A, C, D),
	% tirarCola(Fichas, B, D, PF),
	write("Posibles tiros: "), write(D).

tirarCabeza(A, H, C, PF):-
	encontrar(A, H, C, PF).

encontrar([[A|B]|F], [H1|T1], C, PF):-
	write("cabeza: "), write([H1|T1]), nl,
	write("fichas: "), write(F), nl,
	(A =:= H1; A =:= T1; B =:= H1; B =:= T1),
	write("encontré: "), write([A|B]), nl,
	combina(C, [[A|B]], N),
	write("FH: "), write(N), nl,
	encontrar(F,[H1|T1], N, PF).

encontrar([], _, PF, PF):- 
	write(PF),!.

comer(A, L):-
	write("come"), nl,
	read(B), nl,
	nb_setval(fichas, [B|A]),
	LN is L + 1,
	nb_setval(numFichas, LN),
	nb_getval(fichas, N),
	nb_getval(numFichas, D),
	write(N),
	write(D).

len([], 0).
len([_|A], L):-
	len(A, L2),
  	L is L2 + 1.


combina([], L, L):-
	!.
combina([X|L1], L2, [X|L3]):-
	combina(L1, L2, L3).







