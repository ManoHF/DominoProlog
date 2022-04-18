
% ----------------------------------------------------- INTERFAZ DE JUEGO ------------------------------------------------------------------------------------
% main: interfaz de inicio de juego

% Inicializa el tablero en vacío
% Inicializa las fichas que no han salido con tadas las fichas del juego
% Inicializa número de fichas por jugador en 7
% Asigna las fichas del jugador con una lista leída 
% Elimina de la lista de fichas que no han salido (fichasOp) las fichas del jugador(fichas)
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
	write("Escribe 0 si el oponente inició el juego o 1 si tu lo iniciaste: "), nl,
	read(Prim),
	primerTiro(Prim, T),
	nb_getval(tablero, Tab),
	write(Tab), nl,
	jugar(T).

% primerTiro(i, o): asigna el tiro inicial y cambia el jugador 

% caso 0: cuando el oponente tira la primera ficha
% Inicializa el tablero con la ficha dada
% Actualiza las fichas que no han salido (fichasOp)
% Resta 1 la cantidad de fichas del oponente (numFichasOp)
primerTiro(0, T):-
	write("Ingresa la ficha que tiró el oponente: "), nl,
	read(F),
	nb_setval(tablero, [F,F]),
	nb_setval(numFichasOp, 6),
	nb_getval(fichasOp, FichasOp),
	eliminar(F, FichasOp, NuevasFichas),
	nb_setval(fichasOp, NuevasFichas),
	T is 1.

% caso 1: cuando el jugador tira la primera ficha
% Inicializa el tablero con la ficha dada
% Actualiza las fichas del jugador (fichas)
% Resta 1 la cantidad de fichas del jugador (numFichas)
primerTiro(1, T):-
	write("Ingresa la ficha que tiraste: "), nl,
	read(F),
	nb_setval(tablero, [F,F]),
	nb_setval(numFichas, 6),
	nb_getval(fichas, Fichas),
	eliminar(F, Fichas, NuevasFichas),
	nb_setval(fichas, NuevasFichas),
	T is 0.

% jugar(i): interfaz de turno de jugador/oponente

% caso 0: turno del oponente
% Comprueba si el jugador ganó
% Imprime el tablero
% Pregunta si el oponente comió fichas
% Pregunta si el oponente tiró en la cabeza o cola del tablero
% Llama al predicado oponenteTiro(i, i)
% Llama al predicado jugar(1) para cambiar de turno
jugar(0):-
	nb_getval(numFichas, Num),
	write("Tienes "), write(Num), write(" fichas"), nl,
	((Num < 1, write("GANASTE!!!!!!!! :)"), nl, !);
	(nb_getval(tablero, Tab),
	length(Tab, Len),
	write("el tablero se ve así: "), nl,
	write("--"), imprimirLinea(Len), write("--"), nl,
	write("|"), write(Tab), write("|"), nl,
	write("--"), imprimirLinea(Len), write("--"), nl,
	nl, write("¿Cuántas comió el oponente?"), nl,
	read(N),
	write("Escribe -1 si el oponente tiró en la cabeza, 0 si pasó, o 1 si tiró en la cola"), nl,
	read(U),
	oponenteTiro(N, U), nl,
	jugar(1))).

% caso 1: turno del jugador
% Comprueba si el oponente ganó
% Imprime el tablero
% Imprime fichas del jugador 
% Llama al predicado tirar
% Llama al predicado jugar(0) para cambiar de turno
jugar(1):-
	nb_getval(numFichasOp, Num),
	write("El oponente tiene "), write(Num), write(" fichas"), nl,
	((Num < 1, write("GANÓ EL OPONENTE!!!!!!!! :("), nl, !);
	(nb_getval(tablero, Tab),
	length(Tab, Len),
	write("el tablero se ve así: "), nl,
	write("--"), imprimirLinea(Len), write("--"), nl,
	write("|"), write(Tab), write("|"), nl,
	write("--"), imprimirLinea(Len), write("--"), nl,
	nb_getval(fichas, Fichas),
	nl, write("Tus fichas: "), write(Fichas), nl,
	tirar(), nl,
	jugar(0))).


% oponenteTiro(i, i): actualiza el número de fichas del oponente y actualiza el tablero en caso de que el oponente haya tirado

% Caso 0: Cuando el oponente no tira (pasa)
% Actualiza el número de fichas del oponente
oponenteTiro(N, 0):-
	nb_getval(numFichasOp, M),
	NuevoNum is N + M,
	nb_setval(numFichasOp, NuevoNum).

% Caso -1 y 1: Cuando el oponente tira una ficha en la cabeza(-1) o cola(1) del tablero
% Pide la ficha que tiró el oponente
% Llama al predicado acomodar(i, i) con la ficha tirada y el valor (-1 o 1)
% Actualiza el número de fichas del oponente
oponenteTiro(N, V):-
	write("¿Qué tiró el oponente? "), nl,
	read(F),
	cambiarFicha(F, V, NF),
	acomodar(V, NF),
	nb_getval(fichasOp, Op),
	eliminar(NF, Op, NOp),
	nb_setval(fichasOp, NOp),
	nb_getval(numFichasOp, M),
	NuevoNum is (N + M - 1),
	nb_setval(numFichasOp, NuevoNum).

cambiarFicha(F, 1, NF):-
	cabeza(F, HF), last(F, TF),
	nb_getval(tablero, Tab),
	last(Tab, Ult), last(Ult, TT),
	((HF =:= TT, NF = F);
		(NF = [TF, HF])).

cambiarFicha(F, -1, NF):-
	cabeza(F, HF), last(F, TF),
	nb_getval(tablero, Tab),
	cabeza(Tab, Prim), cabeza(Prim, HT),
	((TF =:= HT, NF = F);
		(NF =[TF, HF])).

% acomodar(i, i): acomoda la ficha la cabeza o cola del tablero 

% Caso -1: acomoda la ficha (F) en la cabeza del tablero
acomodar(-1, F):- 
	nb_getval(tablero, Tab),
	nb_setval(tablero, [F|Tab]).

% Caso 1: acomoda la ficha (F) en la cola del tablero
acomodar(1, F):- 
	nb_getval(tablero, Tab),
	union(Tab,[F], NT),
	nb_setval(tablero, NT).

% Caso 2: Busca dónde se puede acomodar la ficha (F) y llama a acomodar() según corresponda
acomodar(2, F):-
	nb_getval(tablero, [[HT|_]|B]),
	cabeza(F, HF), last(F, TF), % Head Ficha y Tail Ficha
	last(B, UF), last(UF, TT), % ultima ficha y Tail de la ultima ficha
	(
		(HF =:= HT, acomodar(-1,[TF,HF]));
		(TF =:= HT, acomodar(-1, F));
		(HF =:= TT, acomodar(1, F));
		(TF =:= TT, acomodar(1, [TF,HF])),!).

% tirar: interfaz de tiro del jugador
% Llama al predicado encontrar(i, i, i, o) para encontrar los posibles tiros (PF)
% - Llama al predicado comer(i, i) si la lista de posibles tiros (PF) está vacía
% - Llama al predicado acomodar(2, F) si la lisya de posibles tiros (PF) tiene un único tiro posible
% - Llama al predicado podaAlfaBeta(i,i,i,i,i,i,i,i,o,o) si la lista de posibles tiros (PF) tiene más de un elemento
% Actualiza la cantidad de fichas del jugador (numFichas)
tirar:-
	nb_getval(fichas, Fichas),
	nb_getval(tablero, [[A1|_]|B]),
	nb_getval(numFichas, NumFichas),
	NuevoNum is NumFichas - 1,
	encontrar(Fichas, A1, [], D), % D es la lista de las fichas que se pueden acomodar en la cabeza
	last(B, L),
	last(L, L1),
	encontrar(Fichas, L1, D, PF), % PF es posibles fichas
	((PF == [], 
		comer(Fichas, 7));
	(length(PF, 1),
	 	cabeza(PF, F), 
	 	eliminar(F, Fichas, NuevasFichas), 
	 	nb_setval(fichas, NuevasFichas), 
	 	acomodar(2, F), 
	 	nb_setval(numFichas, NuevoNum));
	(write("Posibles tiros: "), write(PF), nl,
		nb_getval(tablero, Tablero),
		% nb_getval(numFichas, Num),
		% nb_getval(numFichasOp, NumOp),
		% Profundidad is Num + NumOp,
		nb_getval(fichasOp, FichasOp),
		podaAlfaBeta(Tablero, 3, FichasOp, Fichas, Fichas, 1, -10000,  10000, MejorTiro, Valor), 
		write("Valor de la rama: "), write(Valor), nl,
		write("MejorTiro: "), write(MejorTiro), nl,
		eliminar(MejorTiro, Fichas, NuevasFichas), nb_setval(fichas, NuevasFichas),
		nb_setval(numFichas, NuevoNum),
		acomodar(2, MejorTiro),!
		)).


% comer(i,i): interfaz para agregar fichas nuevas a las fichas del jugador
% A: Lista de fichas del jugador
% L: Número de fichas del jugador 
% Pregunta si hay fichas para comer
% Si no hay para comer, cambia de turno al del oponente
% Lee la la nueva ficha(F)
% Agrega la ficha a las fichas del jugador (A)
% elimina la ficha (F) de las fichas que no han salido (fichasOp)
% Actualiza la cantidad de fichas del jugador 
% Llama al predicado tirar 
comer(A, L):-
	write("Escribe 1 si hay fichas para comer o 0 si ya se acabaron las fichas"), nl,
	read(Valor),
	((Valor =:= 0, write("Vas a pasar"), nl, jugar(0));
	(write("¿Qué ficha comiste?"), nl,
	read(F),
	nb_setval(fichas, [F|A]),
	nb_getval(fichasOp, Op),
	eliminar(F, Op, NOp),
	nb_setval(fichasOp, NOp),
	LN is (L + 1),
	nb_setval(numFichas, LN),
	tirar())).

% ----------------------------------------------------- MINIMAX Y FUNCIÓN HEURÍSTICA ------------------------------------------------------------------------------------

% ponerFicha(i, i, i, o, o): predicado que recibe las fichas de un jugador, el tablero actual, y un tiro, se encarga de:
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

% Caso 1: si se recibe -1, se acomoda en la cabeza, por lo que el predicado union(i, i, o) recibe primero la ficha y luego el tablero
acomoda(-1, Tiro, Tablero, NuevoTablero):- 
	union([Tiro], Tablero, NuevoTablero).

% Caso 2: se se recibe 1, se acomoda en la cola, por lo que union, recibe primero el tablero y luego la ficha
acomoda(1, Tiro, Tablero, NuevoTablero):- 
	union(Tablero, [Tiro], NuevoTablero). 

% podaALfaBeta(i,i,i,i,i,i,i,i,o,o): predicado que recibe el estado de un juego de domino (tablero, fichas del jugador, fichas del oponente) y utiliza
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

% asignarNuevasFichas(i,i,i,i,i,i,i,i,i,i,i,i,i): tiene el predicado de determinar que jugador debe recibir sus fichas de manera actualizada,
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

% poda(i, i, i, i, i, i, i, i, i, i, i, i, o): predicado que se encargar de verificar los valores actuales asignados a las ramas y con base en
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

% ----------------------------------------------------- FUNCIONES AUXILIARES ------------------------------------------------------------------------------------

% encontrar(i, i, i, o): predicado auxiliar para listas
% Encuentra la lista de fichas que coinciden con el valor en la cabeza o cola del tablero (Val)

% Caso base: cuando la lista está vacía hace un corte
encontrar([], _, PF, PF):- !.
% [[A|B]|F]: lista de fichas del jugador
% Val: último valor de la cabeza o cola del tablero que se busca en la lista de fichas 
% C: lista anterior
% PF: argumento de salida, lista de fichas encontradas
encontrar([[A|B]|F], Val, C, PF):-
	((A =:= Val; B =:= Val),
	union(C, [[A|B]], N),
	encontrar(F, Val, N, PF));
	encontrar(F, Val, C, PF).

% eliminar(i, i, o): predicado auxiliar para listas
% F: Elemento que se quiere eliminar
% L1: Lista de la que se elimina el elemento (F)
% L: argumento de salida, Lista actualizada sin el elemento (F)
% Elimina la ficha (F) de la lista (L1) 
% Elimina la ficha si está de la forma [A,B] o [B,A] 
% Llama al predicado delete(i, i, o) de la biblioteca library(lists)
eliminar(F, L1, L):-
	(cabeza(F, A),
	last(F, B),
	B < A,
	delete(L1, [B,A], L),!);
	delete(L1, F ,L).

% eliminarLista(i, i, o): predicado auxiliar para listas
% [H|T]: Lista que se quiere eliminar
% L: Lista de la que se elimina la lista ([H|T])
% NL: Lista actulaizada sin la lista ([H|T])
% Para cada elemento de lista [H|T] llama al predicado eliminar(i, i, o) 
eliminarLista([H|T], L, NL):-
	eliminar(H, L, L3),
	eliminarLista(T, L3, NL).
eliminarLista([], L, L):- !.

% cabeza(i,o): recibe una lista de la cual saca su cabeza y la devuelve como resultado
cabeza([A|_], A).

imprimirLinea(0):- !.
imprimirLinea(N):-
	write("-----"),
	NN is N - 1,
	imprimirLinea(NN).

