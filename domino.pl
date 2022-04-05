ejemplo:-
	nb_setval(lista, [1,2,3,4,5,6,7,8,9,0]),
	nb_getval(lista, K),
	write(K),
	nb_setval(lista, [28,2,3,4,5,6,7,8,9,0]). % el nuevo valor de tablero es [28,2,3,4,5,6,7,8,9,0]


main:- 
	nb_setval(fichasOp, [[0,0],[0,1],[0,2],[0,3],[0,4],[0,5],[0,6],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[2,2],[2,3],[2,4],[2,5],[2,6],[3,3],[3,4],[3,5],[3,6],[4,4],[4,5],[4,6],[5,5],[5,6],[6,6]]),
	nb_setval(numFichas, 7),
	nb_setval(numFichasOp, 7),
	nb_setval(tablero, []), % el tablero empieza vací
	nb_setval(miTurno, 1),
	write("Ingresa tus fichas: "), nl,
	read(A), nl,
	nb_setval(fichas, A),
	nb_getval(fichasOp, Op),
	write("fichas que no han salido: "), write(Op), nl,
	eliminarLista(A, Op, NOp),
	nb_setval(fichasOp, NOp),
	write("fichas que no han salido: "), write(NOp), nl,
	mula(A, 6, T), nl,
	jugar(T).

mula(Fichas, N, T):-
	(	
		(
			buscar([N, N], Fichas), 
			T is 0,
			write("tu pusiste la mula de "), write([N,N]), write(": "), nl,
			nb_setval(tablero, [[N,N], [N, N]])
		);
		(
			write("Escribe 1 si el oponente puso la mula de "), write([N,N]), write(": "), nl,
			read(Mula), Mula == 1,
			T is 1,
			write("la puso el oponente "), nl,
			nb_setval(tablero, [[N,N], [N,N]])
		)
	);
	(M is (N - 1), mula(Fichas, M, T)).

mula(_, 0, _):- !.
	

buscar(X, [X|_]):-
	!.
buscar(X, [_|Z]):-
	buscar(X, Z).	

jugar(1):-
	nb_getval(tablero, Tab),
	write("el tablero se ve así: "), write(Tab), nl,
	nb_getval(fichas, Fichas),
	nb_getval(tablero, [A|B]),
	last(B, C),
	write("primera y ultima ficha del tablero: "), write([A, C]), nl,
	write("tus fichas: "), write(Fichas), nl,
	tirar(), nl,
	jugar(0).

jugar(0):-
	nb_getval(tablero, Tab),
	write("el tablero se ve así: "), write(Tab), nl,
	write("¿Cuántas comió el oponente?"), nl,
	read(N),
	write("Escribe -1 si el oponente tiró en la cabeza, 0 si pasó, o 1 si tiró en la cola"), nl,
	read(U),
	oponenteTiro(N, U), nl,
	jugar(1).

oponenteTiro(N, V):-
	write("¿Qué tiró el oponente? "), nl,
	read(F),
	acomodar(V, F),
	nb_getval(fichasOp, Op),
	eliminar(F, Op, NOp),
	nb_setval(fichasOp, NOp),
	write("fichas que no han salido: "), write(NOp), nl,
	nb_getval(numFichasOp, M),
	NuevoNum = (N + M - 1),
	nb_setval(numFichasOp, NuevoNum).

oponenteTiro(N, 0):-
	nb_getval(numFichasOp, M),
	NuevoNum = N + M,
	nb_setval(numFichasOp, NuevoNum).

acomodar(-1, F):- % acomodar en la cabeza
	nb_getval(tablero, Tab),
	nb_setval(tablero, [F|Tab]).

acomodar(1, F):- % acomodar en la cola
	nb_getval(tablero, Tab),
	union(Tab,[F], NT),
	nb_setval(tablero, NT).

acomodar(2, F):-
	nb_getval(tablero, [[A1|_]|B]),
	cabeza(F, HF), last(F, TF),
	last(B, TT), % cola del tablero.
	HF =:= A1, write([TF,HF]),nl, acomodar(-1,[TF,A1]);
	!.




tirar():-
	nb_getval(fichas, Fichas),
	nb_getval(tablero, [[A1|_]|B]),
	encontrar(Fichas, A1, [], D), % D es la lista de las fichas que se pueden acomodar en la cabeza
	last(B, L),
	last(L, L1),
	encontrar(Fichas, L1, D, PF), % PF es posibles fichas
	((PF == [], 
		comer(Fichas, 7));
	(length(PF, 1), write("solo puedes tirar: "),
	 	cabeza(PF, F), eliminar(F, Fichas, NuevasFichas), nb_setval(fichas, NuevasFichas), acomodar(2, F));
	(write("Posibles tiros: "), write(PF), nl)).

	% minimax con posibles tiros (encuentra la mejor pieza)
	% actualizar tablero

encontrar([], _, PF, PF):- !.
encontrar([[A|B]|F], Val, C, PF):-
	((A =:= Val; B =:= Val),
	write("encontré: "), write([A|B]), nl,
	union(C, [[A|B]], N),
	encontrar(F, Val, N, PF));
	encontrar(F, Val, C, PF).


comer(A, L):-
	write("¿Qué ficha comiste?"), nl,
	read(F),
	nb_setval(fichas, [F|A]),
	nb_getval(fichasOp, Op),
	eliminar(F, Op, NOp),
	nb_setval(fichasOp, NOp),
	write("fichas que no han salido: "), write(NOp), nl,
	LN is (L + 1),
	nb_setval(numFichas, LN),
	tirar().

eliminar(F, L1, L):-
	(cabeza(F, A),
	last(F, B),
	B < A,
	delete(L1, [B,A], L),!);
	delete(L1, F ,L).

eliminarLista([H|T], L, NL):-
	eliminar(H, L, L3),
	eliminarLista(T, L3, NL).
eliminarLista([], L, L):- !.



cabeza([A|_], A).


len([], 0).
len([_|A], L):-
	len(A, L2),
  	L is L2 + 1.


miembro(X, [X|_]):-
	!.
miembro(X, [_|Z]):-
	miembro(X, Z).
