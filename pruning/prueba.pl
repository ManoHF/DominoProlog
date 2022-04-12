cabeza([A|_], A).

encontrar([], _, PF, PF):- !.
encontrar([[A|B]|F], Val, C, PF):-
	((A =:= Val; B =:= Val),
	write("encontré: "), write([A|B]), nl,
	union(C, [[A|B]], N),
	encontrar(F, Val, N, PF));
	encontrar(F, Val, C, PF).

ponerFicha(Fichas, Tiro, Tablero, NuevasFichas, NuevoTablero):-
	completarTablero(Tiro, Tablero, NuevoTablero),
	eliminar(Tiro, Fichas, NuevasFichas),
	write(NuevasFichas).

eliminar(F, L1, L):-
	(cabeza(F, A),
	last(F, B),
	B < A,
	delete(L1, [B,A], L),!);
	delete(L1, F ,L).

% falta el caso de dos opciones
completarTablero(Tiro, Tablero, NuevoTablero):-
	nb_setval(tab, Tablero),
	nb_getval(tab, [[HT|_]|B]),
	cabeza(Tiro, HF), last(Tiro, TF), % Head Ficha y Tail Ficha
	last(B, UF), last(UF, TT), % ultima ficha y Tail de la ultima ficha
	((HF =:= HT, acomoda(-1,[TF,HF], Tablero, NuevoTablero));
	(TF =:= HT, acomoda(-1, Tiro, Tablero, NuevoTablero));
	(HF =:= TT, acomoda(1, Tiro, Tablero, NuevoTablero));
	(TF =:= TT, acomoda(1, [TF,HF], Tablero, NuevoTablero))).

acomoda(-1, Tiro, Tablero, NuevoTablero):- % acomodar en la cabeza
	union([Tiro], Tablero, NuevoTablero), 
	write(NuevoTablero).

acomoda(1, Tiro, Tablero, NuevoTablero):- % acomodar en la cola
	union(Tablero, [Tiro], NuevoTablero), 
	write(NuevoTablero).

podaAlfaBeta(_, _, _, _, _, NumFichas, NumFichasOp, _, _, _, Valor):-
	evalua(NumFichas, NumFichasOp, 0, 0, Valor).

podaAlfaBeta(_, _, _, _, [], NumFichas, NumFichasOp, _, _, _, Valor):-
	evalua(NumFichas, NumFichasOp, 0, 0, Valor).

podaAlfaBeta(_, _, _, [], _, NumFichas, NumFichasOp, _, _, _, Valor):-
	evalua(NumFichas, NumFichasOp, 0, 0, Valor).

podaAlfaBeta(_, _, [], _, _, NumFichas, NumFichasOp, _, _, _, Valor):-
	evalua(NumFichas, NumFichasOp, 0, 0, Valor).

podaAlfaBeta(Tablero, Profundidad, FichasOcultas, MisFichas, Buscar, MaxMin, NumFichas, NumFichasOp, Alfa, Beta, MejorTiro, Valor):-
	Profundidad > 0,
	nb_setval(tabProv, Tablero),
	nb_getval(tabProv, [[A1|_]|B]),
	nl, nl, write("Uso   "), write(Buscar), nl, nl,
	encontrar(Buscar, A1, [], D), % D es la lista de las fichas que se pueden acomodar en la cabeza
	last(B, L),
	last(L, L1),
	encontrar(Buscar, L1, D, Posibilidades), % PF es posibles fichas
	Alfa1 is -Beta,
	Beta1 is -Alfa,
	NuevaP is Profundidad - 1,
	movimiento(MaxMin, Posibilidades, Tablero, FichasOcultas, MisFichas, Buscar, NuevaP, Alfa1, Beta1, Record, (Movimiento, Valor)).

movimiento(MaxMin, [Tiro | Restantes], Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Alfa, Beta, Record, MejorTiro):-
	ponerFicha(Buscar, Tiro, Tablero, NuevasFichas, NuevoTablero), 
	asignarNuevasFichas(Tiro, Restantes, MaxMin, NuevoTablero, Profundidad, FichasOcultas, MisFichas, NuevasFichas, 7, 7, Alfa, Beta, nil, MejorTiro, Valor).
movimiento(MaxMin, [], Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Alfa, Beta, Tiro, (Tiro, Alfa)).

asignarNuevasFichas(Tiro, Restantes, -1, Tablero, Profundidad, FichasOcultas, MisFichas, NuevasFichas, 7, 7, Alfa, Beta, Record, MejorTiro, Valor):-
	podaAlfaBeta(Tablero, Profundidad, NuevasFichas, MisFichas, MisFichas, 1, 7, 7, Alfa, Beta, MejorTiro, Valor), 
	Valor1 is -Valor, 
	poda(-1, Tiro, Valor1, Profundidad, Alfa, Beta, Restantes, Tablero, Record, MejorTiro).

asignarNuevasFichas(Tiro, Restantes, 1, Tablero, Profundidad, FichasOcultas, MisFichas, NuevasFichas, 7, 7, Alfa, Beta, Record, MejorTiro, Valor):-
	podaAlfaBeta(Tablero, Profundidad, FichasOcultas, NuevasFichas, FichasOcultas, -1, 7, 7, Alfa, Beta, MejorTiro, Valor), 
	Valor1 is -Valor,
	poda(1, Tiro, Valor1, Profundidad, Alfa, Beta, Restantes, Tablero, Record, MejorTiro),
	!.

poda(_,Tiro,Valor, _, _, Beta , _ , _, _, (Tiro, Valor)) :- 
   Valor >= Beta, !.
poda(MaxMin,Tiro,Valor, Profundidad, Alfa, Beta, Restantes, Tablero , _, MejorTiro) :- 
   Alfa < Valor, Valor < Beta, !, 
   movimiento(MaxMin, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Valor, Beta, Tiro, MejorTiro).
poda(MaxMin, _, Valor , D, Alfa, Beta, Restantes, Tablero, Record, MejorTiro) :- 
   Valor =< Alfa, !, 
   movimiento(MaxMin, Restantes, Tablero, FichasOcultas, MisFichas, Buscar, Profundidad, Alfa, Beta, Record, MejorTiro).

evalua(Profundidad, MisFichas, FichasOcultas, Valor):-
	Valor is (1/Profundidad),
    write("Valor Asignado: "), write(Valor).

evalua(_, [], _, Valor):-
	Valor is 10000,
    write("Valor Asignado: "), write(Valor).

evalua(_, _, [], Valor):-
	Valor is -10000,
    write("Valor Asignado: "), write(Valor).

alpha_beta(Jugador, 0, Tablero, MisFichas, FichasOcultas, _Alpha, _Beta, _NoMove, Value):- 
   evalua(0, MisFichas, FichasOcultas, Valor).

alpha_beta(Jugador, Profundidad, Tablero, MisFichas, FichasOcultas, Alfa, Beta, Movimiento, Valor) :- 
    Profundidad > 0,
    nb_setval(tabProv, Tablero),
    nb_getval(tabProv, [[A1|_]|B]),
    nl, nl, write("Uso   "), write(Buscar), nl, nl,
    encontrar(Buscar, A1, [], D), % D es la lista de las fichas que se pueden acomodar en la cabeza
    last(B, L),
    last(L, L1),
    encontrar(Buscar, L1, D, Movimientos), % PF es posibles fichas
    Alfa1 is -Beta,
    Beta1 is -Alfa,
    NuevaP is Profundidad - 1,
    evaluate_and_choose(Jugador, Movimientos, Tablero, NuevaP, Alfa1, Beta1, nil, (Movimiento, Valor)).

evaluate_and_choose(Jugador, [Ficha | Restantes], Tableros, Profundidad, Alfa, Beta, Record, MejorFicha) :-
   ponerFicha(Buscar, Ficha, Tablero, NuevasMisFichas, NuevasFichasOcultas, NuevoTablero),, 
   other_player(Jugador, OtroJugador),
   alpha_beta(OtroJugador, Profundidad, NuevoTablero, NuevasMisFichas, NuevasFichasOcultas, Alfa, Beta, _OtherMove,Value),
   Value1 is -Value,
   cutoff(Player,Move,Value1,D,Alpha,Beta,Moves,Position,Record,BestMove).
evaluate_and_choose(_Player,[],_Position,_D,Alpha,_Beta,Move,(Move,Alpha)).

cutoff(_Player,Move,Value,_D,_Alpha,Beta,_Moves,_Position,_Record,(Move,Value)) :- 
   Value >= Beta, !.
cutoff(Player,Move,Value,D,Alpha,Beta,Moves,Position,_Record,BestMove) :- 
   Alpha < Value, Value < Beta, !, 
   evaluate_and_choose(Player,Moves,Position,D,Value,Beta,Move,BestMove).
cutoff(Player,_Move,Value,D,Alpha,Beta,Moves,Position,Record,BestMove) :- 
   Value =< Alpha, !, 
   evaluate_and_choose(Player,Moves,Position,D,Alpha,Beta,Record,BestMove).

other_player(o,x).
other_player(x,o).

