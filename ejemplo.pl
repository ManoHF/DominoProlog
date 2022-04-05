
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
	write("Ingresa tus fichas: "), nl,
	read(A), nl,
	nb_setval(fichas, A),
	nb_getval(miTurno, T),
	jugar(T).

jugar(1):-
	nb_getval(tablero, Tab),
	write("el tablero se ve así: "), write(Tab), nl,
	nb_getval(fichas, Fichas),
	nb_getval(tablero, [A|B]),
	last(B, C),
	write("tablero: "), write([A, C]), nl,
	write("fichas: "), write(Fichas), nl,
	tirar(), nl,
	jugar(0).

jugar(0):-
	nb_getval(tablero, Tab),
	write("el tablero se ve así: "), write(Tab), nl,
	write("¿Cuántas comió el oponente?"), nl,
	read(N),
	write("Escribe -1 si el oponente tiró en la cabeza, 0 si no tiró o 1 si tiró en la cola"), nl,
	read(U),
	oponenteTiro(N, U), nl,
	jugar(1).

oponenteTiro(N, 1):-
	nb_getval(tablero, Tab),
	write("¿Qué tiró el oponente? "), nl,
	read(C),
	combina(Tab,[C], NT),
	nb_setval(tablero, NT),
	nb_getval(numFichasOp, M),
	NuevoNum = N + M - 1,
	nb_setval(numFichasOp, NuevoNum).

oponenteTiro(N, -1):-
	nb_getval(tablero, Tab),
	write("¿Qué tiró el oponente? "), nl,
	read(C),
	nb_setval(tablero, [C|Tab]),
	nb_getval(numFichasOp, M),
	NuevoNum = N + M - 1,
	nb_setval(numFichasOp, NuevoNum).

oponenteTiro(N, 0):-
	nb_getval(numFichasOp, M),
	NuevoNum = N + M,
	nb_setval(numFichasOp, NuevoNum).

tirar():-
	nb_getval(fichas, Fichas),
	nb_getval(tablero, [[A1|_]|B]),
	encontrar(Fichas, A1, [], D),
	last(B, L),
	last(L, L1),
	encontrar(Fichas, L1, D, PF),
	((PF == [], comer(Fichas, 7));
	(PF \= [],
	write("Posibles tiros: "), write(PF), nl)).

	% minimax con posibles tiros (encuentra la mejor pieza)
	% actualizar tablero


encontrar([[A|B]|F], Val, C, PF):-
	((A =:= Val; B =:= Val),
	write("encontré: "), write([A|B]), nl,
	combina(C, [[A|B]], N),
	encontrar(F, Val, N, PF));
	encontrar(F, Val, C, PF).

encontrar([], _, PF, PF):- !.

comer(A, L):-
	write("¿Qué ficha comiste?"), nl,
	read(B),
	nb_setval(fichas, [B|A]),
	LN is L + 1,
	nb_setval(numFichas, LN),
	tirar().

len([], 0).
len([_|A], L):-
	len(A, L2),
  	L is L2 + 1.


combina([], L, L):-
	!.
combina([X|L1], L2, [X|L3]):-
	combina(L1, L2, L3).






