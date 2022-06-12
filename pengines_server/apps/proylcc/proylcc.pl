:- module(proylcc, 
	[  
		flick/5
	]).

:- dynamic visitado/1.

%
% adyCStar(Origin, +Grid, -Res)
% Calcula el conjunto de celdas adyacentesC* de la celda Origin en la grilla Grid
% siguiendo una estrategia de propagación o expansión.
%

adyCStar(Origin, Grid, Res) :-
    adyCStarSpread([Origin], [], Grid, Res).

%
% adyCStarSpread(+Pend, +Vis, +Grid, -Res)
% Pend: por "pendientes", inicialmente es la lista [Origin], y en general es 
% el conjunto de celdas adyacentesC* a Origin que aún no fueron consideradas.
% Vis: por "visitados", inicialmente [], son las celdas adyacentesC* a la Origen 
% que ya fueron consideradas.
% Grid: idem adyCStar
% Res: idem adyCStar
% En cada paso se selecciona una celda de las pendientes, se pasa a visitados, y
% se agregan a pendientes todas aquellas adyacentes a la celda, del mismo color, que no estén
% ya ni en pendientes ni visitados.
%

adyCStarSpread([], Vis, _Grid, Vis).

adyCStarSpread(Pend, Vis, Grid, Res):-
    Pend = [P|Ps],
    findall(A, 
	        (
    	        adyC(P, Grid, A),
        	    not(member(A, Pend)),
            	not(member(A, Vis))
	        ), 
            AdyCP),
    append(AdyCP, Ps, NPend),
    adyCStarSpread(NPend, [P|Vis], Grid, Res).

%
% getColor(+Grid,+[X,Y],-C)
%
% C es el color que se encuentra en la grilla en la posicion [X,Y] pasada por parametro
%

getColor(Grid,[X,Y],C) :-nth0(X, Grid, F),
    nth0(Y, F, C).
% 
% adyC(+P, +Grid, -A)
%

adyC(P, Grid, A):-
    ady(P, Grid, A),
    getColor(Grid,P, C),
    getColor(Grid,A, C).

% 
% ady(+P, +Grid, -A)
%

ady([X, Y], Grid, [X1, Y]):-
    length(Grid, L),
    X < L - 1,
    X1 is X + 1.

ady([X, Y], _Grid, [X1, Y]):-
    X > 0,
    X1 is X - 1.

ady([X, Y], Grid, [X, Y1]):-
    Grid = [F|_],
    length(F, L),
    Y < L - 1,
    Y1 is Y + 1.

ady([X, Y], _Grid, [X, Y1]):-
    Y > 0,
    Y1 is Y - 1.

%
% flickColor(+Grid, +ListaPos, +C, -GridNew)
%
% GridNew es la nueva grilla resultante de recorrer la lista de posiciones, ListaPos, en Grid, y cambiandoles el color a C.
%

flickColor(Grid, [] , _, Grid).
flickColor(Grid, ListaPos , C, GridNew) :- ListaPos = [Coord| L1], setColor(Grid, Coord, C, GridAux),
    										flickColor(GridAux, L1, C,GridNew).

%
% setColor(+Grid,+Coord,+C,-GridNew)
%
% GridNew es el resultado de ir a las coordenadas en Coords de la grilla Grid, y cambiarle el color a C.
%

setColor(Grid,Coord,C,GridNew):- replaceInMatrix(Grid,Coord,[0,0],C,GridNew).

%
% replaceInMatrix(+M,+[X,Y],+[I,J],+E, -MatrixNew)
%
% MatrixNew es el resultado de recorrer la matriz M , llegar a la posicion [X,Y] y cambiar su elemento por E.
%

replaceInMatrix([M|Ms],[X,Y],[I,0],E,MatrixNew):- X =\= I, I1 is I+1, replaceInMatrix(Ms,[X,Y],[I1,0],E,AuxMatrix), MatrixNew = [M|AuxMatrix].
replaceInMatrix([M|Ms],[X,Y],[X,0],E,MatrixNew):- replaceInList(M,Y,0,E,AuxMatrix), MatrixNew = [AuxMatrix|Ms].
replaceInMatrix([_|Ms],[X,Y],[X,Y],E,MatrixNew):- MatrixNew = [E|Ms].

%
% replaceInList(+M,+Y,+J,+E,-L)
%
% L es la lista resultante de ir a la componente en la posicion Y e intercambiarla con el elemento E.
%

replaceInList([M|Ms],Y,J,E,L):- J1 is J+1 , replaceInList(Ms,Y,J1,E,ListaAux), L = [M|ListaAux].
replaceInList([_|Ms],Y,Y,E,L):- L = [E|Ms].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% flick(+Grid, +Color, -FGrid)
%
% FGrid es el resultado de hacer 'flick' de la grilla Grid con el color Color.
% Retorna false si Color coincide con el color de la celda Origen de la grilla. 
% 

flick(Grid, Origen, Color, FGrid,Capturados):-
	getColor(Grid,Origen,C),
	C \= Color,
	adyCStar(Origen,Grid,LAdyacentesC),
	flickColor(Grid,LAdyacentesC,Color,FGrid),
    calcularCapturados(FGrid,Origen,Capturados), !.

%
% gridComplete(+Grid,+Capturados)
%
% Consulta si se gano el juego, hace esto comparando las dimensiones de la grilla con la cantidad de celdas capturadas
% pasadas por parametro.
%

gridComplete(Grid,Capturados) :- Grid = [X|_], length(Grid,LimiteX), length(X, LimiteY), Capturados is LimiteX * LimiteY.                                                                            

%
% calcularCapturados(+Grid,+Origen,-Capturados)
%
% Dada una grilla Grid y una celda origen Origen calcula la cantidad de celdas capturadas, Capturados.
%

calcularCapturados(Grid,Origen,Capturados):-
    adyCStar(Origen,Grid,AdyacentesC),
    length(AdyacentesC,Capturados),!.

%
% buscarMasCapturas(Xs,Ys,Zs)
%
% Xs lista con pares (numero,lista).
% Ys maximo par encontrado al momento.
% Zs par maximo de la lista.
% 
% Dada una lista de pares (numero,lista) retorna el par con numero mayor.
%

buscarMasCapturas([],Ys,Ys):- !.
buscarMasCapturas([X|Xs],[MaxCap|_],Zs):-
    X=[Y|Ys], Y > MaxCap,!, buscarMasCapturas(Xs,[Y|Ys],Zs).
buscarMasCapturas([_|Xs],[MaxCap|MaxSec],Zs):- buscarMasCapturas(Xs,[MaxCap|MaxSec],Zs).

%
% color(X).
%
% X es un color que pertence a la grilla

color(r). color(g). color(b). color(y). color(v). color(p).

%
% Path(+Grid,+Origen,+PE,+OC,+NC,-Cap,-Sec).
%
% Dado una grilla Grid, una celda Origen, un numerode movimientos PE, el color de la celda origen OC y el color
% al que se quiere cambiar NC calcula el camino que captura mas celdas en los movimientos permitidos PE, 
% retornando asi el mayor capturado , Cap, con su secuencia, Sec, notemos que tambien retorna el que inicio, OC.
%

path(Grid,Origen,PE,OC,NC,Cap,[OC|Sec]):- PE \= 0 , PE1 is PE - 1, flick(Grid,Origen,NC,FGrid,_), !,
    try_path(FGrid,Origen,PE1,Cap,Sec).


%
% try_path(+Grid,+Origen,+PE,-Capturados,-Secuencia)
%
% Dada una Grilla ,Grid, una celda de origen, Origen, y una cantidad de movimientos PE , prueba las combinaciones
% posibles de caminos desde esa celda y expande dependiendo de la cantidad de PE, retorna la secuencia que se uso
% que logro capturar mas celdas
%

try_path(Grid,Origen,PE,Capturados,Secuencia):-
    PE \=0,!, color(NC),
    getColor(Grid,Origen,OC), 
    NC \= OC,
    findall([C,S],(path(Grid,Origen,PE,OC,NC,C,S)),R),
    buscarMasCapturas(R,[0,0],[Capturados|S]), S=[Secuencia|_].

try_path(Grid,Origen,0,Capturados,Secuencia):-
    calcularCapturados(Grid,Origen,Capturados), !,
    getColor(Grid,Origen,C),
    Secuencia=[C].

%
% optimal_path(+Grid,+Origen,+PE,-Capturados,-Secuencia)
%
% Dado una grilla Grid, una celda de origen Origen y un numero de movimiento PE calcula en esa grilla
% cual es la secuencia ,Secuencia, que captura la mayor cantidad de celdas, Capturados.
% Lo hace utilizando el metodo greedy solamente descartando que no se cambie al mismo color consecutivamente.
% 
% TOFIX: retorna el color de la celda inicial tambien
%

optimal_path(Grid,Origen,PE,Capturados,Secuencia):- PE > 0, findall([Capturados,Secuencia], try_path(Grid,Origen,PE,Capturados,Secuencia), R),
    buscarMasCapturas(R,[0,0],[Capturados|S]), S=[Secuencia|_].
