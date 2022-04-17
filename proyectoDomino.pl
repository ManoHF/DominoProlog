

main:- 
	nb_setval(fichasOp, [[0,0],[0,1],[0,2],[0,3],[0,4],[0,5],[0,6],[1,1],[1,2],[1,3],[1,4],[1,5],[1,6],[2,2],[2,3],[2,4],[2,5],[2,6],[3,3],[3,4],[3,5],[3,6],[4,4],[4,5],[4,6],[5,5],[5,6],[6,6]]),
	nb_setval(numFichas, 7),
	nb_setval(numFichasOp, 7),
	nb_setval(tablero, []), % el tablero empieza vacío
	write("Ingresa tus fichas: "), nl,
	read(A), nl,
	nb_setval(fichas, A),
	nb_getval(fichasOp, Op),
	eliminarLista(A, Op, NOp),
	nb_setval(fichasOp, NOp),
	write("Escribe 1 si el oponente inició el juego o 0 si tu lo iniciaste: "), nl,
	read(Prim),
	primerTiro(Prim, T),
	nb_getval(tablero, Tab),
	write(Tab), nl,
	jugar(T).

primerTiro(1, T):-
	write("Ingresa la ficha que tiró el oponente: "), nl,
	read(F),
	nb_setval(tablero, [F,F]),
	nb_setval(numFichasOp, 6),
	T is 1.

primerTiro(0, T):-
	write("Ingresa la ficha que tiraste: "), nl,
	read(F),
	nb_setval(tablero, [F,F]),
	nb_setval(numFichasOp, 6),
	T is 0.


buscar(X, [X|_]):-
	!.
buscar(X, [_|Z]):-
	buscar(X, Z).	

jugar(1):-
	nb_getval(numFichas, Num),
	((Num < 1, write("GANASTE!!!!!!!! :)"), nl, !);
	(nb_getval(tablero, Tab),
	write("el tablero se ve así: "), nl,
	write("-----------------------------------------"),nl,
	write("|"), write(Tab), write("|"), nl,
	write("-----------------------------------------"),nl,
	nb_getval(fichas, Fichas),
	nb_getval(tablero, [A|B]),
	last(B, C),
	nl, write("Primera y ultima ficha del tablero: "), write([A, C]), nl,
	write("Tus fichas: "), write(Fichas), nl,
	tirar(), nl,
	jugar(0))).

jugar(0):-
	nb_getval(numFichasOp, Num),
	((Num < 1, write("GANÓ EL OPONENTE!!!!!!!! :("), nl, !);
	(nb_getval(tablero, Tab),
	write("el tablero se ve así: "), nl,
	write("-----------------------------------------"),nl,
	write("|"), write(Tab), write("|"), nl,
	write("-----------------------------------------"), nl,
	write("¿Cuántas comió el oponente?"), nl,
	read(N),
	write("Escribe -1 si el oponente tiró en la cabeza, 0 si pasó, o 1 si tiró en la cola"), nl,
	read(U),
	oponenteTiro(N, U), nl,
	jugar(1))).

oponenteTiro(N, 0):-
	nb_getval(numFichasOp, M),
	NuevoNum = N + M,
	nb_setval(numFichasOp, NuevoNum).

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


acomodar(-1, F):- % acomodar en la cabeza
	nb_getval(tablero, Tab),
	nb_setval(tablero, [F|Tab]).

acomodar(1, F):- % acomodar en la cola
	nb_getval(tablero, Tab),
	union(Tab,[F], NT),
	nb_setval(tablero, NT).

acomodar(2, F):-
	nb_getval(tablero, [[HT|_]|B]),
	cabeza(F, HF), last(F, TF), % Head Ficha y Tail Ficha
	last(B, UF), last(UF, TT), % ultima ficha y Tail de la ultima ficha
	(
		(HF =:= HT, HF =:= TT, minimax(0, F));
		(HF =:= HT, acomodar(-1,[TF,HF]));
		(TF =:= HT, acomodar(-1, F));
		(HF =:= TT, acomodar(1, F));
		(TF =:= TT, acomodar(1, [TF,HF])),!).


minimax(0, F):-
	write("FALTA HACER EL CASO EN QUE SE PONGA EN LOS DOS LADOS"),
	acomodar(1, F).

tirar:-
	nb_getval(fichas, Fichas),
	nb_getval(tablero, [[A1|_]|B]),
	encontrar(Fichas, A1, [], D), % D es la lista de las fichas que se pueden acomodar en la cabeza
	last(B, L),
	last(L, L1),
	encontrar(Fichas, L1, D, PF), % PF es posibles fichas
	((PF == [], 
		comer(Fichas, 7));
	(length(PF, 1),
	 	cabeza(PF, F), eliminar(F, Fichas, NuevasFichas), nb_setval(fichas, NuevasFichas), acomodar(2, F));
	(write("Posibles tiros: "), write(PF), nl,
		nb_getval(tablero, Tablero),
		% nb_getval(numFichas, Num),
		% nb_getval(numFichasOp, NumOp),
		% Profundidad is Num + NumOp,
		nb_getval(fichasOp, FichasOp),
		podaAlfaBeta(Tablero, 2, FichasOp, Fichas, Fichas, 1, -10000,  10000, MejorTiro, Valor), 
		write("Valor de la rama: "), write(Valor), nl,
		eliminar(MejorTiro, Fichas, NuevasFichas), nb_setval(fichas, NuevasFichas),
		acomodar(2, MejorTiro),!
		)).


encontrar([], _, PF, PF):- !.
encontrar([[A|B]|F], Val, C, PF):-
	((A =:= Val; B =:= Val),
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
	write("fichas que no han salido: "), write(NOp), nl, nl,
	LN is (L + 1),
	nb_setval(numFichas, LN),
	tirar().

% ponerFicha(i, i, i, o, o): funcion que recibe las fichas de un jugador, el tablero actual, y un tiro, se encarga de:
% - actualizar el tablero con la ficha dada en Tiro llamando a completarTablero(i, i, o)
% - eliminar el Tiro de las Fichas del jugador en turno llamando a eliminar(i, i, o)
% - regresa un tablero (NuevoTablero) y un set de fichas (NuevasFichas) actualizados
ponerFicha(Fichas, Tiro, Tablero, NuevasFichas, NuevoTablero):-
	completarTablero(Tiro, Tablero, NuevoTablero),
	eliminar(Tiro, Fichas, NuevasFichas).

% completarTablero(i, i, o): recibe un ficha a jugar (Tiro), el tablero (Tablero) y devuelve un NuevoTablero
% - primero recupera la cabeza de la primera ficha del tablero: HT
% - recupera la cola de la última ficha del tablero: TT
% - obtiene la cabeza (HF) y cola (TF) del tiro a realizar
% - compara distintas cabezas y colas de la ficha y el tablero, y con eso asigna la ficha en alguna posición y la invierte de ser necesario
completarTablero(Tiro, Tablero, NuevoTablero):-
	nb_setval(tab, Tablero),
	nb_getval(tab, [[HT|_]|B]),
	cabeza(Tiro, HF), last(Tiro, TF),
	last(B, UF), last(UF, TT), 
	((HF =:= HT, acomoda(-1,[TF,HF], Tablero, NuevoTablero));
	(TF =:= HT, acomoda(-1, Tiro, Tablero, NuevoTablero));
	(HF =:= TT, acomoda(1, Tiro, Tablero, NuevoTablero));
	(TF =:= TT, acomoda(1, [TF,HF], Tablero, NuevoTablero)), !).

% acomoda(i, i, i, o): asigna un Tiro al tablero en una cierta posición y devuelve el tablero con la ficha insertada en él

% Caso 1: si se recibe -1, se acomoda en la cabeza, por lo que la función union(i, i, o) recibe primero la ficha y luego el tablero
acomoda(-1, Tiro, Tablero, NuevoTablero):- 
	union([Tiro], Tablero, NuevoTablero).

% Caso 2: se se recibe 1, se acomoda en la cola, por lo que union, recibe primero el tablero y luego la ficha
acomoda(1, Tiro, Tablero, NuevoTablero):- 
	union(Tablero, [Tiro], NuevoTablero). 

% podaALfaBeta(i,i,i,i,i,i,i,i,o,o): funcion que recibe el estado de un juego de domino (tablero, fichas del jugador, fichas del oponente) y utiliza
% el algoritmo minimax con poda alfa beta para determinar el mejor tiro posible

% Caso de corte 1: la lista de fichas del jugador está vacía, por lo que este camino termina en una victoria
% obtiene el número de fichas sobrantes del oponente y lo asigna como valor del tiro
podaAlfaBeta(_, _, FichasOcultas, [], _, _, _, _, _, Valor):-
	length(FichasOcultas, N1),
	Valor is N1, !.

% Caso de corte 2: el jugador en turno no tiene opciones de tiro, por lo que tendría que pasar o comer
% El valor se asigna restando ambos números de fichas y se multiplica por negativo, ya que es positivo para nosotros si el otro jugador (-1) ya no
% tiene tiro
podaAlfaBeta(MaxMin, _, FichasOcultas, MisFichas, [], _, _, _, _, Valor):-
	length(FichasOcultas, N1), length(MisFichas, N2),
	Valor is (N1 - N2) * (-MaxMin), !.

% Caso de corte 3: se llega a la profundidad máxima, es decir, el número de turnos jugados rebasó el máximo, ya que no queremos tener un árbol
% con muchísimas combinaciones, el valor depende de quién tenga menos fichas
podaAlfaBeta(_, 0, FichasOcultas, MisFichas, _, _, _, _, _, Valor):-
	length(FichasOcultas, N1), length(MisFichas, N2), 
	Valor is (N1 - N2), !.

% Caso de corte 4: la lista de fichas del oponente está vacía, por lo que este camino termina en una derrota
% obtiene el número de fichas sobrantes del jugador en negativo y lo asigna como valor del tiro
podaAlfaBeta(_, _, [], MisFichas, _, _, _, _, _, Valor):-
	length(MisFichas, N2),
	write("Perdiste"), nl,
	Valor is -N2, write(Valor), nl, !.

% Recibe distintos parámetros 
% - el estado actual del tablero, una profundidad delimitada por el usuario, 
% - el jugador en turno (MaxMin): puede ser 1 si es el usuario y -1 si es el oponente
% - un alfa y una beta dada por el usuario
% - Las fichas del jugador (MisFichas) y las del oponente (FichasOcultas)
% - Buscar: son las fichas del jugador en turno que se utilizarán para buscar posibles tiros
% - Regresa el mejor tiro (Movimiento) con su valor asignado (Valor)
podaAlfaBeta(Tablero, Profundidad, FichasOcultas, MisFichas, Buscar, MaxMin, Alfa, Beta, Movimiento, Valor):-
	Profundidad > 0,
	nb_setval(tabProv, Tablero),
	nb_getval(tabProv, [[A1|_]|B]),
	encontrar(Buscar, A1, [], D), 
	last(B, L),
	last(L, L1),
	encontrar(Buscar, L1, D, Posibilidades),
	Alfa1 is -Beta,
	Beta1 is -Alfa,	
	NuevaP is Profundidad - 1,
	movimiento(MaxMin, Posibilidades, Tablero, FichasOcultas, MisFichas, Buscar, NuevaP, Alfa1, Beta1, _, (Movimiento, Valor)), !.

% movimiento(i,i,i,i,i,i,i,i,i,i,o): para cada una de las posibilidades de tiro crea una rama en la que va asignando la ficha al tablero con
% ponerficha(i,i,i,o,o), cambia el turno con cambioJugador(i,o) y asigna las nuevas fichas con asignarNuevasFichas(i,i,i,i,i,i,i,i,i,i,i,i,i),
% en donde se llama a podaAlfaBeta hasta que se llega a uno de los casos de corte

% Caso de corte: no hay posibilidades de tiro, por lo que ya no se continua la búsqueda y se regresa a terminar lo pendiente
movimiento(_, [], _, _, _, _, _, Alfa, _, Tiro, (Tiro, Alfa)).

% Recibe el jugador (MaxMin), un lista con un Tiro actual y otras opciones (Restantes), también recibe el estado del juego (Tablero, MisFichas,
% FichasOcultas, Buscar) junto a los valores que sirven para asignar un puntaje al movimiento.
movimiento(MaxMin, [Tiro | Restantes], Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Alfa, Beta, Record, MejorTiro):-
	ponerFicha(Buscar, Tiro, Tablero, NuevasFichas, NuevoTablero),
	cambioJugador(MaxMin, OtroMaxMin),
	asignarNuevasFichas(Tiro, Restantes, OtroMaxMin, NuevoTablero, Profundidad, FichasOcultas, MisFichas, NuevasFichas, Alfa, Beta, Record, MejorTiro, _), !.

% asignarNuevasFichas(i,i,i,i,i,i,i,i,i,i,i,i,i): tiene la función de determinar que jugador debe recibir sus fichas de manera actualizada,
% además, va cambiando las fichas usadas para buscar los posibles tiros

% Caso 1: se recibe un 1, por lo que se viene del turno del jugador oponente, entonces se llama podaAlfaBeta con las fichas del oponentes como
% Nuevas Fichas y las fichas con las que buscar son las del jugador
asignarNuevasFichas(Tiro, Restantes, 1, Tablero, Profundidad, _, MisFichas, NuevasFichas, Alfa, Beta, Record, MejorTiro, Valor):-
	podaAlfaBeta(Tablero, Profundidad, NuevasFichas, MisFichas, MisFichas, 1, Alfa, Beta, MejorTiro, Valor),
	Valor1 is -Valor,
	poda(1, Tiro, Valor1, Profundidad, Alfa, Beta, Restantes, Tablero, NuevasFichas, MisFichas, MisFichas, Record, MejorTiro).

% Caso 2: se recibe un .1, por lo que se viene del turno del jugador, entonces se llama podaAlfaBeta con las fichas del jugador como
% Nuevas Fichas y las fichas con las que buscar son las del oponente
asignarNuevasFichas(Tiro, Restantes, -1, Tablero, Profundidad, FichasOcultas, _, NuevasFichas, Alfa, Beta, Record, MejorTiro, Valor):-
	podaAlfaBeta(Tablero, Profundidad, FichasOcultas, NuevasFichas, FichasOcultas, -1, Alfa, Beta, MejorTiro, Valor),
	Valor1 is -Valor,
	poda(-1, Tiro, Valor1, Profundidad, Alfa, Beta, Restantes, Tablero, FichasOcultas, NuevasFichas, FichasOcultas, Record, MejorTiro).

% poda(i, i, i, i, i, i, i, i, i, i, i, i, o): función que se encargar de verificar los valores actuales asignados a las ramas y con base en
% condiciones actualiza los valores de alfa y beta, hace la poda de ciertas ramas innecesarias y continua la búsqueda con tiros y records}
% provisionales
%  - MejorTiro o (Tiro, Valor): se encuentran en conjunto, ya que cada tiro tiene un valor específico que debe ser comparado a lo largo de la
%    búsqueda
%  - Las demás variables vienen de podaALfaBeta y movimiento, y representan las mismas cosas

% Caso 1: El valor de la rama es mayor igual a beta, por lo que se hace un corte que borra los puntos de retroceso
poda(_, Tiro, Valor, _, _, Beta , _ , _, _, _, _, _, (Tiro, Valor)) :- 
   Valor >= Beta, !.

% Caso 2:  Valor es menor a beta, pero mayor a alfa, se llama a movimiento con las fichas posibles restantes y se le asigna
% el valor a nuestra variable alfa
poda(MaxMin, Tiro, Valor, Profundidad, Alfa, Beta, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, _, MejorTiro) :- 
   Alfa < Valor, Valor < Beta, !, 
   movimiento(MaxMin, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Valor, Beta, Tiro, MejorTiro).

% Caso 3: Valor es menor igual a alfa, se borran los puntos de retroceso, se llama a movimiento con las fichas posibles restantes
poda(MaxMin, _, Valor, Profundidad, Alfa, Beta, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, Record, MejorTiro) :- 
   Valor =< Alfa, !, 
   movimiento(MaxMin, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Alfa, Beta, Record, MejorTiro).

% cambioJugador(i, o): se encarga de hacer el cambio de turno
% si recibe 1, te devuelve -1, y viceversa
cambioJugador(1, -1).
cambioJugador(-1, 1).


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

% cabeza(i,o): recibe una lista de la cual saca su cabeza y la devuelve como resultado
cabeza([A|_], A).
