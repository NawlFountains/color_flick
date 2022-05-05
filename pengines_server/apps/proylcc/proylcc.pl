:- module(proylcc, 
	[  
		flick/5
	]).

:- dynamic visitado/1.

%
% adyacentes([+X,+Y], -L)
%
% L es un la lista con las posiciones adyacentes a la posicion pasada dentro de los limites de la grilla
% que en nuestro caso es de 14x14.

adyacentes([X,Y],L):- X >= 0 , X < 14 , Y >= 0 , Y < 14 ,
    X1 is X-1,getCoord(X1,Y,E1),
    X2 is X+1, getCoord(X2,Y,E2),
    Y1 is Y-1 ,getCoord(X,Y1,E3),
    Y2 is Y+1 ,getCoord(X,Y2,E4),
    combineCoords(E1,E2,E3,E4,Lm),
    clear_outs(Lm,L).


%
% getCoords(+X,+Y,-E)
%
% E es un par ordenado de coordenadas que evalua si la coordenada pasada por parametro pertenece o no a la grilla y retornar
% el par correspondiente, si esta dentro retorna el mismo valor ingresado pero en un par, si alguna componente
% escapa de lo limites, se le asigna "out".

getCoord(X,Y,E) :- ( Em = [out] , agregarALista(out,Em,E), ((X < 0) ; (X > 13)), ((Y <0);(Y > 13)));
                    (Em = [out], agregarALista(Y,Em,E), (((X < 0) ; (X > 13)), ((Y >= 0),(Y =< 13))));
                    ( Em = [X] , agregarALista(out,Em,E) ,(((X >= 0) , (X =< 13)), ((Y < 0);(Y > 13))));
                    (  Em = [X] , agregarALista(Y,Em,E), (((X >= 0) , (X =< 13)), ((Y >= 0),(Y =< 13)))).

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

adyacentesC(Grid,X,Y) :- esAdyacente(X,Y),mismoColor(Grid,X,Y).

%
% generateAdyacentesCTransitiva(+Grid,+X,-L)
%
% L es la lista con los adyacentesC de la celda X pasada por parametro en la grilla Grid
%

generateAdyacentesCTransitiva(Grid,X,L) :- retractall(visitado(_)), adyacentesCTransitiva(Grid,[X],L).

%
% adyacentesCTransitiva(+Grid,+ListaPos,-L)
%
% L es la clasula transitiva de adyacentesC de las posicion pasda por parametro, acepta multiples posiciones ya que cuando
% halla un adjacenteC sin ver intenta expandirse, llamandose recursivamente.
%

adyacentesCTransitiva(Grid,[X|Xs],L) :- (   not(visitado(X)), assert(visitado(X)),findall(Y, (adyacentesC(Grid,X,Y), not(visitado(Y))),T),
    (   (   Xs \=[] , T \= [], adyacentesCTransitiva(Grid,Xs,La), adyacentesCTransitiva(Grid,T,Ls), append(La,Ls,Lp), L = [X|Lp]);
    (   Xs = [], T \= [], adyacentesCTransitiva(Grid,T,Ls), L = [X|Ls]); 
    (    Xs = [], T = [], L = [X] );
    (   Xs \= [], T = [], adyacentesCTransitiva(Grid,Xs,La), L = [X|La]))); (   visitado(X), L = []).

%
% esAdyacente(X,Y)
%
% Evalua si dado dos pares de ordenados de coordenadas son adyacentes o no.
% Retorna false, si no son adyacentes

esAdyacente(X,Y) :- adyacentes(X,L), pertenece(Y,L).

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
	generateAdyacentesCTransitiva(Grid,Origen,LAdyacentesC),
	flickColor(Grid,LAdyacentesC,Color,FGrid),
    generateAdyacentesCTransitiva(FGrid,Origen,NewAdyacents),
    length(NewAdyacents,Capturados).

                                                                                   
