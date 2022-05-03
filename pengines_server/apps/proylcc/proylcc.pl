:- module(proylcc, 
	[  
		flick/3
	]).

:- dynamic visitado/1.

adyacentes([X,Y],L):- X >= 0 , X < 14 , Y >= 0 , Y < 14 ,
    X1 is X-1,getCoord(X1,Y,E1),
    X2 is X+1, getCoord(X2,Y,E2),
    Y1 is Y-1 ,getCoord(X,Y1,E3),
    Y2 is Y+1 ,getCoord(X,Y2,E4),
    combineCoords(E1,E2,E3,E4,Lm),
    clear_outs(Lm,L).

getCoord(X,Y,E) :- ( Em = [out] , agregarALista(out,Em,E), ((X < 0) ; (X > 13)), ((Y <0);(Y > 13)));
                    (Em = [out], agregarALista(Y,Em,E), (((X < 0) ; (X > 13)), ((Y >= 0),(Y =< 13))));
                    ( Em = [X] , agregarALista(out,Em,E) ,(((X >= 0) , (X =< 13)), ((Y < 0);(Y > 13))));
                    (  Em = [X] , agregarALista(Y,Em,E), (((X >= 0) , (X =< 13)), ((Y >= 0),(Y =< 13)))).
clear_outs([],[]).
clear_outs([X|Xs],L) :- (not(pertenece(out,X)),clear_outs(Xs,Lm), L= [X|Lm]) ;
                        (pertenece(out,X),clear_outs(Xs,L)).

adyacentesC(Grid,X,X).
adyacentesC(Grid,X,Y) :- esAdyacente(X,Y),mismoColor(Grid,X,Y), assert(visitado(X)).
adyacentesC(Grid,X,Y) :- esAdyacente(X,Z), not(visitado(Z)), mismoColor(Grid,X,Z),adyacentesC(Grid,Z,Y) , assert(visitado(Z)).

generateAdyacentesC(Grid,X,Lista) :- retractall(visitado(_)), findall(Y, (adyacentesC(Grid,X,Y)), L), limpiarRepetidos(L,Lista).

esConjunto([]).
esConjunto([X|Xs]) :- not(pertenece(X,Xs)), esConjunto(Xs).

agregarAConjunto(X,[],[X]).
agregarAConjunto(X,[X|Ys],[X|Ys]).
agregarAConjunto(X,[Y|Ys],[Y|L]) :- esConjunto([Y|Ys]),agregarAConjunto(X,Ys,L).

limpiarRepetidos([],[]).
limpiarRepetidos([X|Xs],Zs) :- (not(pertenece(X,Xs)), limpiarRepetidos(Xs,Z),agregarAConjunto(X,Z,Zs)); limpiarRepetidos(Xs,Zs).

esAdyacente(X,Y) :- adyacentes(X,L), pertenece(Y,L).

mismoColor(Grid,X,Y) :-	getColor(Grid,X,C), getColor(Grid,Y,C).

getColor(Grid,X,C) :- nth0(0,X,X1), nth0(1,X,Y1), nth0(X1,Grid,Fila), nth0(Y1,Fila,C).

combineCoords(X,Y,Z,W,L) :- agregarALista(X,[],L1), agregarALista(Y,L1,L2), agregarALista(Z,L2,L3), agregarALista(W,L3,L).

pertenece(X,[X|_Ys]).
pertenece(X,[_Y|Ys]) :- pertenece(X,Ys).

agregarALista(X,[],[X]).
agregarALista(X,[Y|Ys],[Y|L]) :- agregarALista(X,Ys,L).

flickColor(Grid, [] , _, Grid).
flickColor(Grid, ListaPos , C, GridNew) :- ListaPos = [Coord| L1], setColor(Grid, Coord, C, GridAux),
    										flickColor(GridAux, L1, C,GridNew).

setColor(Grid,Coord,C,GridNew):- replaceInMatrix(Grid,Coord,[0,0],C,GridNew).

replaceInMatrix([M|Ms],[X,Y],[I,0],E,MatrixNew):- X =\= I, I1 is I+1, replaceInMatrix(Ms,[X,Y],[I1,0],E,AuxMatrix), MatrixNew = [M|AuxMatrix].
replaceInMatrix([M|Ms],[X,Y],[X,0],E,MatrixNew):- replaceInList(M,Y,0,E,AuxMatrix), MatrixNew = [AuxMatrix|Ms].
replaceInMatrix([_|Ms],[X,Y],[X,Y],E,MatrixNew):- MatrixNew = [E|Ms].

replaceInList([M|Ms],Y,J,E,L):- J1 is J+1 , replaceInList(Ms,Y,J1,E,ListaAux), L = [M|ListaAux].
replaceInList([_|Ms],Y,Y,E,L):- L = [E|Ms].

flick(Grid, Color, FGrid):-
	generateAdyacentesC(Grid,[0,0],LAdyacentesC),
	flickColor(Grid,LAdyacentesC,Color,FGrid).

                                                                                   
