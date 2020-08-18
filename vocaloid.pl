%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%Cantantes%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%

% Base de conocimiento

%canta(Nombre,Cancion).
%cancion(Cancion,Duracion).
%kaito no canta nada

canta(megurineLuka,cancion(nightFever,4)).
canta(megurineLuka,cancion(foreverYoung,5)).
canta(hatsuneMiku,cancion(tellYourWorld,4)).
canta(gumi,cancion(foreverYoung,4)).
canta(gumi,cancion(tellYourWorld,5)).
canta(seeU,cancion(novemberRain,6)).
canta(seeU,cancion(nightFever,5)).
canta(elPity,cancion(senorKiosquero,5)).
canta(elPity,cancion(homero,5)).
canta(elPity,cancion(meGustaElRock,5)).
canta(kaito,cancion(nada,0)).


% Punto 1
cantaMasDeUna(Vocaloid):-
    canta(Vocaloid,Cancion1),
    canta(Vocaloid,Cancion2),
    Cancion1 \= Cancion2.

esNovedoso(Vocaloid):-
    cantaMasDeUna(Vocaloid),
    findall(Tiempo,canta(Vocaloid,cancion(_,Tiempo)), Duraciones),
    sum_list(Duraciones, TiempoDeCanto),
    TiempoDeCanto < 15.

% Punto 2
duracionesAceleradas([Duracion1|RestoDeDuraciones]):-
    Duracion1 =< 4,
    duracionesAceleradas(RestoDeDuraciones).
duracionesAceleradas([]).

esAcelerado(Vocaloid):-
    canta(Vocaloid,_),
    findall(Duracion,canta(Vocaloid,cancion(_,Duracion)),Duraciones),
    duracionesAceleradas(Duraciones).

%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%Conciertos%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%

% Base de conocimiento

%concierto(Nombre,Pais,fama,tipo).
%tipo -----> gigante(CancionesSabidasMinimas,DuracionMinina)
%            mediano(DuracionMaxima)
%            pequeño(DuracionMininaDeAlgunaCancion)

concierto(mikuExpo, eeuu, 2000, gigante(2,6)).
concierto(magicalMirai, japon, 3000, gigante(3,10)).
concierto(vocalektVisions, eeuu, 1000, mediano(9)).
concierto(mikuFest, argentina, 100, pequeno(4)).

conoce(megurineLuka, hatsuneMiku).
conoce(megurineLuka, gumi).
conoce(gumi, seeU).
conoce(seeU, kaito).

% Punto 1
%Agregado a la base de conocimiento

% Punto 2
cantidadDeCanciones(Vocaloid,Cantidad):-
    canta(Vocaloid,_),
    findall(Cancion, canta(Vocaloid,Cancion), TodasLasCanciones),
    length(TodasLasCanciones, Cantidad).

duracionTotalDeCancionesDe(Vocaloid,DuracionTotal):-
    findall(Duracion, canta(Vocaloid,cancion(_,Duracion)),Duraciones),
    sum_list(Duraciones,DuracionTotal).

cumpleCondicion(Vocaloid,gigante(CancionesSabidasMinimas,DuracionMinina)):-
    duracionTotalDeCancionesDe(Vocaloid,DuracionTotal),
    cantidadDeCanciones(Vocaloid,CantidadQueSabe),
    DuracionTotal > DuracionMinina,
    CantidadQueSabe >= CancionesSabidasMinimas.
cumpleCondicion(Vocaloid,mediano(DuracionMaxima)):-
    duracionTotalDeCancionesDe(Vocaloid,DuracionTotal),
    DuracionTotal < DuracionMaxima.
cumpleCondicion(Vocaloid,pequeno(DuracionMininaDeAlgunaCancion)):-
    canta(Vocaloid,cancion(_,Duracion)),
    Duracion > DuracionMininaDeAlgunaCancion.
    
puedeParticipar(Concierto,hatsuneMiku):-
    concierto(Concierto,_,_,_).   
puedeParticipar(Concierto,Vocaloid):-
    concierto(Concierto,_,_,Condicion),
    canta(Vocaloid,_),
    cumpleCondicion(Vocaloid,Condicion),
    Vocaloid \= hatsuneMiku.

% Punto 3

famaPorConciertos(Vocaloid,FamaPorConcierto):-
    puedeParticipar(Concierto,Vocaloid),
    findall(Fama,concierto(Concierto,_,Fama,_),FamaObtenida),
    sum_list(FamaObtenida, FamaPorConcierto).
famaTotalDeConciertos(Vocaloid,FamaTotal):-
    findall(FamaPorConcierto, famaPorConciertos(Vocaloid,FamaPorConcierto),ListaDeFama),
    sum_list(ListaDeFama, FamaTotal).

famaDeUnVocaloid(Vocaloid,FamaTotal):-
    canta(Vocaloid,_),
    famaTotalDeConciertos(Vocaloid,FamaConciertos),
    cantidadDeCanciones(Vocaloid,CantidadDeCanciones),
    FamaTotal is FamaConciertos*CantidadDeCanciones.

elMasFamoso(Vocaloid):-
    famaDeUnVocaloid(Vocaloid,FamaDelFamoso),
    forall((famaDeUnVocaloid(OtroVocaloid,Fama),OtroVocaloid \= Vocaloid), Fama < FamaDelFamoso).

% Punto 4
/*
conoce(megurineLuka, hatsuneMiku).
conoce(megurineLuka, gumi).
conoce(gumi, seeU).
conoce(seeU, kaito).
*/
conocido(Vocaloid,OtroVocaloid):-
    conoce(Vocaloid,OtroVocaloid).
conocido(Vocaloid,OtroVocaloid):-
    conoce(Vocaloid,UnVocaloid),
    conocido(UnVocaloid,OtroVocaloid).

participaConConocidos(Vocaloid):-
    puedeParticipar(Concierto,Vocaloid),
    puedeParticipar(Concierto,OtroVocaloid),
    conocido(Vocaloid,OtroVocaloid).

unicoEnConcierto(Vocaloid):-
    canta(Vocaloid,_),
    not(participaConConocidos(Vocaloid)).

% Punto 5

/*
En la solución planteada habría que agregar una claúsula en el predicado cumpleRequisitos/2  
que tenga en cuenta el nuevo functor con sus respectivos requisitos 
El concepto que facilita los cambios para el nuevo requerimiento es el polimorfismo,
 que nos permite dar un tratamiento en particular a cada uno de los conciertos en la cabeza de la cláusula.
*/
    




    
    



    




    


    

    

