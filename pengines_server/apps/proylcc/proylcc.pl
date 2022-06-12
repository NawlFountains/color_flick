:- module(proylcc, 
	[  
		flick/5
	]).

:- dynamic visitado/1.

%
% adyacentes([+X,+Y],+LimiteX,+LimiteY -L)
%
% L es un la lista con las posiciones adyacentes a la posicion pasada dentro de los limites de la grilla
% que en nuestro caso es de LmiteX x LimiteY.

adyacentes([X,Y],LimiteX,LimiteY,L):- X >= 0 , X < LimiteX , Y >= 0 , Y < LimiteY ,
    X1 is X-1,getCoord(X1,Y,LimiteX,LimiteY,E1),
    X2 is X+1, getCoord(X2,Y,LimiteX,LimiteY,E2),
    Y1 is Y-1 ,getCoord(X,Y1,LimiteX,LimiteY,E3),
    Y2 is Y+1 ,getCoord(X,Y2,LimiteX,LimiteY,E4),
    combineCoords(E1,E2,E3,E4,Lm),
    clear_outs(Lm,L).


%
% getCoords(+X,+Y,+LimiteX,+LimiteY-E)
%
% E es un par ordenado de coordenadas que evalua si la coordenada pasada por parametro pertenece o no a la grilla y retornar
% el par correspondiente, si esta dentro retorna el mismo valor ingresado pero en un par, si alguna componente
% escapa de lo LimiteX o LimiteY, se le asigna "out".

getCoord(X,Y,LimiteX,LimiteY,E) :- ( Em = [out] , agregarALista(out,Em,E), ((X < 0) ; (X > LimiteX)), ((Y <0);(Y >= LimiteY)));
                    (Em = [out], agregarALista(Y,Em,E), (((X < 0) ; (X >= LimiteX)), ((Y >= 0),(Y < LimiteY))));
                    ( Em = [X] , agregarALista(out,Em,E) ,(((X >= 0) , (X < LimiteX)), ((Y < 0);(Y >= LimiteY))));
                    (  Em = [X] , agregarALista(Y,Em,E), (((X >= 0) , (X < LimiteX)), ((Y >= 0),(Y < LimiteY)))).

%
% clear_outs(+X,-L)
%
% L es la lista pasada como parametro con pares ordenadas y borra los pares que contengan "out".
%
clear_outs([],[]).
clear_outs([X|Xs],L) :- (not(pertenece(out,X)),clear_outs(Xs,Lm), L= [X|Lm]) ;
                        (pertenece(out,X),clear_outs(Xs,L)).

%
% adyacentesC(+Grid,+X,+Y)
%
% Evalua si dado dos pares de coordenadas, son adyacenteC, es decir, son del mismo color y son adyacentes.
%

adyacentesC(Grid,X,Y,LimiteX,LimiteY) :- esAdyacente(X,Y,LimiteX,LimiteY),mismoColor(Grid,X,Y).

%
% generateAdyacentesCTransitiva(+Grid,+X,-L,+LimiteX,+LimiteY)
%
% L es la lista con los adyacentesC de la celda X pasada por parametro en la grilla Grid
%

generateAdyacentesCTransitiva(Grid,X,L,LimiteX,LimiteY) :- retractall(visitado(_)), adyacentesCTransitiva(Grid,[X],L,LimiteX,LimiteY).

%
% adyacentesCTransitiva(+Grid,+ListaPos,-L,+LimiteX,+LimiteY)
%
% L es la clasula transitiva de adyacentesC de las posicion pasda por parametro, acepta multiples posiciones ya que cuando
% halla un adjacenteC sin ver intenta expandirse, llamandose recursivamente. Se pasa por parametro los limites de la grilla
%

adyacentesCTransitiva(Grid,[X|Xs],L,LimiteX,LimiteY) :- (   not(visitado(X)), assert(visitado(X)),findall(Y, (adyacentesC(Grid,X,Y,LimiteX,LimiteY), not(visitado(Y))),T),
    (   (   Xs \=[] , T \= [], adyacentesCTransitiva(Grid,Xs,La,LimiteX,LimiteY), adyacentesCTransitiva(Grid,T,Ls,LimiteX,LimiteY), append(La,Ls,Lp), L = [X|Lp]);
    (   Xs = [], T \= [], adyacentesCTransitiva(Grid,T,Ls,LimiteX,LimiteY), L = [X|Ls]); 
    (    Xs = [], T = [], L = [X] );
    (   Xs \= [], T = [], adyacentesCTransitiva(Grid,Xs,La,LimiteX,LimiteY), L = [X|La]))); (   visitado(X), L = []).

%
% esAdyacente(+X,+Y,+LimiteX,+LimiteY)
%
% Evalua si dado dos pares de ordenados de coordenadas son adyacentes o no.
% Retorna false, si no son adyacentes

esAdyacente(X,Y,LimiteX,LimiteY) :- adyacentes(X,LimiteX,LimiteY,L), pertenece(Y,L).

%
% mismoColor (+Grid,+X,+Y)
%
% Evalua si dada una Grilla y dos posiciones, en esas posiciones se encuentran elementos que hacen referencia al mismo color, 
% el caso de este proyecto, que el caracter que representa al color sea el mismo.
% Retorna False si no son del mismo color.

mismoColor(Grid,X,Y) :-	getColor(Grid,X,C), getColor(Grid,Y,C).

%
% getColor(+Grid,+X,-C)
%
% C es el color que se encuentra en la grilla en la posicion X pasada por parametro
%

getColor(Grid,X,C) :- nth0(0,X,X1), nth0(1,X,Y1), nth0(X1,Grid,Fila), nth0(Y1,Fila,C).

%
% combineCoords(+X,+Y,+Z,+W,-L)
%
% L es el resultado de combinar en una lista los pares ordenados que representan coordenadas , X , Y, Z y W.
%

combineCoords(X,Y,Z,W,L) :- agregarALista(X,[],L1), agregarALista(Y,L1,L2), agregarALista(Z,L2,L3), agregarALista(W,L3,L).

%
% pertenece(+X,+L)
%
% Dado una lista L y una elemento X, verifica si X pertenece a la lista L.
% Retorna false si no pertenece.

pertenece(X,[X|_Ys]).
pertenece(X,[_Y|Ys]) :- pertenece(X,Ys).

%
% agregarALista(+X,+L,-Lout)
%
% Lout es el resultado de agregar a la lista L el elemento X.
%

agregarALista(X,[],[X]).
agregarALista(X,[Y|Ys],[Y|L]) :- agregarALista(X,Ys,L).

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
% TODO : se guardan los assert por eso hay retract, deberia haber un predicado que
% aisle eso y ademas el convertir en una lista a origen, [Origen].

flick(Grid, Origen, Color, FGrid,Capturados):-
	getColor(Grid,Origen,C),
	C \= Color,
    length(Grid,LimiteX),
    Grid = [X|_],
    length(X,LimiteY),
	generateAdyacentesCTransitiva(Grid,Origen,LAdyacentesC,LimiteX,LimiteY),
	flickColor(Grid,LAdyacentesC,Color,FGrid),
    generateAdyacentesCTransitiva(FGrid,Origen,NewAdyacents,LimiteX,LimiteY),
    length(NewAdyacents,Capturados), !.

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
    length(Grid,LimiteX),
    Grid= [X|_],
    length(X,LimiteY),
    generateAdyacentesCTransitiva(Grid,Origen,AdyacentesC,LimiteX,LimiteY),
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
% optimal_path(+Grid,+Origen,+PE,-Capturados,-Secuencia)
%
% Dado una grilla Grid, una celda de origen Origen y un numero de movimiento PE calcula en esa grilla
% cual es la secuencia ,Secuencia, que captura la mayor cantidad de celdas, Capturados.
% Lo hace utilizando el metodo greedy solamente descartando que no se cambie al mismo color consecutivamente.
% 
% TOFIX: retorna el color de la celda inicial tambien
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

optimal_path(Grid,Origen,PE,Capturados,Secuencia):- findall([Capturados,Secuencia], try_path(Grid,Origen,PE,Capturados,Secuencia), R),
    buscarMasCapturas(R,[0,0],[Capturados|S]), S=[Secuencia|_].
