% CON REPETICION
%Plata
denominacion(1000).
denominacion(500).
denominacion(200).
denominacion(100).
denominacion(50).
denominacion(20).
denominacion(10).
denominacion(5).
denominacion(2).
denominacion(1).

vuelto(Importe, Pago, Vuelto):-
    TotalVuelto is Pago - Importe,
    denominaciones(TotalVuelto, Vuelto).

denominaciones(0,[]).
denominaciones(Total,[Denominacion|Denominaciones]):-
    Total>0,
    denominacion(Denominacion),
    Resto is Total - Denominacion,
    denominaciones(Resto,Denominaciones).

%Salidas
salida(recital, 100).
salida(cine, 10).
salida(restaurante, 30).
salida(parqueDeDiversiones, 50).
salida(teatro, 35).
salida(museo, 5).

salidasPosibles(_,[]).
salidasPosibles(Presupuesto,[UnaSalida|Salidas]):-
    salida(UnaSalida,Costo),
    Presupuesto >= Costo,
    Resto is Presupuesto - Costo,
    salidasPosibles(Resto,Salidas).

%SIN REPETICION
/* Combinatoria sin repetición 
Un pirata quiere armar la tripulación para su barco, él solo quiere llevar:
- cocineros
- piratas bravos
- piratas de más de 40 años (no pagan impuestos)
*/
cocinero(donato).
cocinero(pietro).
pirata(felipe, 27).
pirata(marcos, 39).
pirata(facundo, 45).
pirata(tomas, 20).
pirata(betina, 26).
pirata(gonzalo, 22).

bravo(tomas).
bravo(felipe).
bravo(marcos).
bravo(betina).

tripulacionBarco(Tripulantes):-
    candidatos(Candidatos),
    tripulantes(Candidatos,Tripulantes),
    Tripulantes \= [].

candidatos(Candidatos):-
    findall(Candidato,(cocinero(Candidato);bravo(Candidato),pirata(Candidato,_)
    ;pirata(Candidato,Edad),Edad >= 40),Cands),
    list_to_set(Cands,Candidatos).

tripulantes(_,[]).
tripulantes(Candidatos,[UnCandidato|Tripulantes]):-
    select(UnCandidato,Candidatos,OtrosCandidatos),
    tripulantes(OtrosCandidatos,Tripulantes).

% EJERCICIOS INTEGRADORES

%Saltos
puntajes(hernan,[3,5,8,6,9]).
puntajes(julio,[9,7,3,9,10,2]).
puntajes(ruben,[3,5,3,8,3]).
puntajes(roque,[7,10,10]).

puntaje(Persona,NroSalto,PuntajeEnSalto):-
    puntajes(Persona,Saltos),
    nth0(NroSalto,Saltos,PuntajeEnSalto).

estaDescalificado(Persona):-
    puntajes(Persona,Saltos),
    length(Saltos,NroSaltos),
    NroSaltos > 5.

clasificaALaFinal(Persona):-
    puntajes(Persona,Saltos),
    sumlist(Saltos,PuntajeTotal),
    PuntajeTotal >= 28.
clasificaALaFinal(Persona):-
    distinct(Persona, (puntajes(Persona,Puntajes),
    select(Puntaje,Puntajes,OtrosPuntajes),
    Puntaje >= 8,
    select(Punto,OtrosPuntajes,_),
    Punto >= 8)).

% SUBTES
linea(a,[plazaMayo,peru,lima,congreso,miserere,rioJaneiro,primeraJunta,nazca]).
linea(b,[alem,pellegrini,callao,pueyrredonB,gardel,medrano,malabia,lacroze,losIncas,urquiza]).
linea(c,[retiro,diagNorte,avMayo,independenciaC,plazaC]).
linea(d,[catedral,nueveJulio,medicina,pueyrredonD,plazaItalia,carranza,congresoTucuman]).
linea(e,[bolivar,independenciaE,pichincha,jujuy,boedo,varela,virreyes]).
linea(h,[lasHeras,santaFe,corrientes,once,venezuela,humberto1ro,inclan,caseros]).
combinacion([lima, avMayo]).
combinacion([once, miserere]).
combinacion([pellegrini, diagNorte, nueveJulio]).
combinacion([independenciaC, independenciaE]).
combinacion([jujuy, humberto1ro]).
combinacion([santaFe, pueyrredonD]).
combinacion([corrientes, pueyrredonB]).

estaEn(Estacion,Linea):-
    linea(Linea,Estaciones),
    member(Estacion,Estaciones).

distancia(Estacion1,Estacion2,Distancia):-
    estaEn(Estacion1,Linea),
    estaEn(Estacion2,Linea),
    linea(Linea,Estaciones),
    nth1(PosicionEstacion1,Estaciones,Estacion1),
    nth1(PosicionEstacion2,Estaciones,Estacion2),
    Distancia is abs(PosicionEstacion1 - PosicionEstacion2).

mismaAltura(Estacion1,Estacion2):-
    estaEn(Estacion1,Linea1),
    estaEn(Estacion2,Linea2),
    Linea1 \= Linea2,
    linea(Linea1,EstacionesLinea1),
    linea(Linea2,EstacionesLinea2),
    nth1(Posicion,EstacionesLinea1,Estacion1),
    nth1(Posicion,EstacionesLinea2,Estacion2).

viajeFacil(Estacion1,Estacion2):-
    estaEn(Estacion1,Linea),
    estaEn(Estacion2,Linea).

viajeFacil(Estacion1,Estacion2):-
    combinacion([Estacion1,Estacion2]).
viajeFacil(Estacion1,Estacion2):-
    combinacion([Estacion2,Estacion1]).

% VIAJES
/*vuelo(Codigo de vuelo, capacidad en toneladas, [lista de destinos]).
Esta lista de destinos está compuesta de la siguiente manera:
escala(ciudad, tiempo de espera)
tramo(tiempo en vuelo)
*/

vuelo(aRG845, 30, [escala(rosario,0), tramo(2), escala(buenosAires,0)]).

vuelo(mH101, 95, [escala(kualaLumpur,0), tramo(9), escala(capeTown,2),
tramo(15), escala(buenosAires,0)]).

vuelo(dLH470, 60, [escala(berlin,0), tramo(9), escala(washington,2), tramo(2), escala(nuevaYork,0)]).

vuelo(aAL1803, 250, [escala(nuevaYork,0), tramo(1), escala(washington,2),
tramo(3), escala(ottawa,3), tramo(15), escala(londres,4), tramo(1),
escala(paris,0)]).

vuelo(bLE849, 175, [escala(paris,0), tramo(2), escala(berlin,1), tramo(3),
escala(kiev,2), tramo(2), escala(moscu,4), tramo(5), escala(seul,2), tramo(3), escala(tokyo,0)]).

vuelo(nPO556, 150, [escala(kiev,0), tramo(1), escala(moscu,3), tramo(5),
escala(nuevaDelhi,6), tramo(2), escala(hongKong,4), tramo(2), escala(shanghai,5), tramo(3), escala(tokyo,0)]).

vuelo(dSM3450, 75, [escala(santiagoDeChile,3), tramo(1), escala(buenosAires,3), tramo(1), escala(washington,4), tramo(1), escala(berlin,3), tramo(1), escala(tokyo,3)]).

tiempo(escala(_,Tiempo),Tiempo).
tiempo(tramo(Tiempo),Tiempo).

tiempoCiudad(escala(_,Tiempo),Tiempo).
tiempoTramo(tramo(Tiempo),Tiempo).

esTramo(tramo(_)).
esCiudad(escala(_,_)).

ciudades(Vuelo,Ciudades):-
    vuelo(Vuelo,_,Escalas),
    findall(Escala,(esCiudad(Escala),member(Escala,Escalas)),Ciudades).

% 1.
tiempoTotalVuelo(Vuelo,TiempoDeVuelo):-
    vuelo(Vuelo,_,Lugares),
    maplist(tiempo,Lugares,TiemposDeVuelos),
    sumlist(TiemposDeVuelos,TiempoDeVuelo).

% 2.
escalaAburrida(Vuelo,EscalasAburridas):-
    vuelo(Vuelo,_,Escalas),
    findall(Escala,(member(Escala,Escalas),vuelo(Vuelo,_,Escalas),tiempo(Escala,Tiempo),Tiempo>3),EscalasAburridas).

% 3.
ciudadesAburridas(Vuelo,CiudadesAburridas):-
    vuelo(Vuelo,_,Escalas),
    findall(Escala,(member(Escala,Escalas),vuelo(Vuelo,_,Escalas),tiempoCiudad(Escala,Tiempo),Tiempo>=3),CiudadesAburridas).

% 4.
vueloLargo(Vuelo):-
    vuelo(Vuelo,_,Escalas),
    findall(TramoEnElAire,(esTramo(TramoEnElAire),member(TramoEnElAire,Escalas)),TramosEnElAire),
    maplist(tiempo,TramosEnElAire,TiempoEnElAire),
    sumlist(TiempoEnElAire,Tiempo),
    Tiempo >= 10.

conectados(Vuelo1,Vuelo2):-
    vuelo(Vuelo1,_,Escalas),
    vuelo(Vuelo2,_,Esc),
    findall(Ciudad,(esCiudad(Ciudad),member(Ciudad,Escalas)),Ciudades1),
    findall(Ciu,(esCiudad(Ciu),member(Ciu,Esc)),Ciudades2),
    findall(CiudadConectada,(member(CiudadConectada,Ciudades1),member(CiudadConectada,Ciudades2)),CiudadesConectadas),
    CiudadesConectadas \= [].

% 5.
bandaDeTres(Vuelo1,Vuelo2,Vuelo3):-
    conectados(Vuelo1,Vuelo2),
    conectados(Vuelo2,Vuelo3).

% 6.
/*distanciaEnEscalas(Ciudad1,Ciudad2,CantEscalas):-
    vuelo(Vuelo,_,Escalas),
    ciudades(Vuelo,Ciudades),
    member(Ciudad1,Ciudades),
    member(Ciudad2,Ciudades),
    nth1(Posicion,Escalas,escala(Ciudad1,_)),
    nth1(Posicion2,Escalas,escala(Ciudad2,_)),
*/


% 7.
vueloLento(Vuelo):-
    vuelo(Vuelo,_,_),
    ciudades(Vuelo,Ciudades),
    ciudadesAburridas(Vuelo,Ciudades),
    not(vueloLargo(Vuelo)).

% COSAS Y PAISES
%buscar(cosa,ciudad)
tarea(basico,buscar(libro,jartum)).
tarea(basico,buscar(arbol,patras)).
tarea(basico,buscar(roca,telaviv)).
tarea(intermedio,buscar(arbol,sofia)).
tarea(intermedio,buscar(arbol,bucarest)).
tarea(avanzado,buscar(perro,bari)).
tarea(avanzado,buscar(flor,belgrado)).

nivelActual(pepe,basico).
nivelActual(lucy,intermedio).
nivelActual(juancho,avanzado).

idioma(alejandria,arabe).
idioma(jartum,arabe).
idioma(patras,griego).
idioma(telaviv,hebreo).
idioma(sofia,bulgaro).
idioma(bari,italiano).
idioma(bucarest,rumano).
idioma(belgrado,serbio).

habla(pepe,bulgaro).
habla(pepe,griego).
habla(pepe,italiano).
habla(juancho,arabe).
habla(juancho,griego).
habla(juancho,hebreo).
habla(lucy,griego).


capital(pepe,1200).
capital(lucy,3000).
capital(juancho,500).

vivas(arbol).
vivas(perro).
vivas(flor).

% 1.
destinoPosible(Persona,Ciudades):-
    nivelActual(Persona,_),
    findall(Ciudad,(nivelActual(Persona,Nivel),tarea(Nivel,buscar(_,Ciudad))),Ciudades).

idiomaUtil(Nivel,IdiomasUtiles):-
    nivelActual(_,Nivel),
    findall(Idioma,(tarea(Nivel,buscar(_,Ciudad)),idioma(Ciudad,Idioma)),IdiomasUtiles).

% 2.
excelenteCompaniero(P1,P2):-
    habla(P2,_),
    destinoPosible(P1,CiudadesPosibles),
    maplist(idioma,CiudadesPosibles,IdiomasQueNecesita),
    forall(member(Idioma,IdiomasQueNecesita),habla(P2,Idioma)).

% 3.
interesante(Nivel):-
    nivelActual(_,Nivel),
    findall(Cosa,tarea(Nivel,buscar(Cosa,_)),Cosas),
    forall(member(Cosa,Cosas),vivas(Cosa)).
interesante(Nivel):-
    nivelActual(_,Nivel),
    findall(Ciudad,tarea(Nivel,buscar(_,Ciudad)),CiudadesDelNivel),
    maplist(idioma,CiudadesDelNivel,IdiomasDelNivel),
    member(italiano,IdiomasDelNivel).
interesante(Nivel):-
    nivelActual(_,Nivel),
    findall(Persona,nivelActual(Persona,Nivel),Personas),
    maplist(capital,Personas,Capitales),
    sumlist(Capitales,CapitalTotal),
    CapitalTotal >= 10000.

% 4.
complicado(Participante):-
    not(excelenteCompaniero(Participante,Participante)),
    nivelActual(Participante,Nivel),
    capital(Participante,Capital),
    ((Nivel \= basico,
    Capital =< 1500);
    (Nivel == basico,
    Capital < 500)).

% 5.
homogeneo(Nivel):-
    nivelActual(_,Nivel),
    findall(Cosa,tarea(Nivel,buscar(Cosa,_)),CosasDelNivel),
    listaEsHomogenea(CosasDelNivel).

listaEsHomogenea([]).
listaEsHomogenea([_]).
listaEsHomogenea([Elemento1,Elemento2|Elementos]):-
    Elemento1 == Elemento2,
    listaEsHomogenea(Elementos).

% 6.
poliglota(Persona):-
    nivelActual(Persona,_),
    findall(Idioma,habla(Persona,Idioma),Idiomas),
    length(Idiomas,X),
    X >= 3.

% TEG!
/* distintos paises */
paisContinente(americaDelSur, argentina).
paisContinente(americaDelSur, bolivia).
paisContinente(americaDelSur, brasil).
paisContinente(americaDelSur, chile).
paisContinente(americaDelSur, ecuador).
paisContinente(europa, alemania).
paisContinente(europa, espania).
paisContinente(europa, francia).
paisContinente(europa, inglaterra).
paisContinente(asia, aral).
paisContinente(asia, china).
paisContinente(asia, gobi).
paisContinente(asia, india).
paisContinente(asia, iran).

/*países importantes*/
paisImportante(argentina).
paisImportante(kamchatka).
paisImportante(alemania).

/*países limítrofes*/
limitrofes([argentina,brasil]).
limitrofes([bolivia,brasil]).
limitrofes([bolivia,argentina]).
limitrofes([argentina,chile]).
limitrofes([espania,francia]).
limitrofes([alemania,francia]).
limitrofes([nepal,india]).
limitrofes([china,india]).
limitrofes([nepal,china]).
limitrofes([afganistan,china]).
limitrofes([iran,afganistan]).

/*distribución en el tablero */
ocupa(argentina, azul, 4).
ocupa(bolivia, rojo, 1).
ocupa(brasil, verde, 4).
ocupa(chile, negro, 3).
ocupa(ecuador, rojo, 2).
ocupa(alemania, azul, 3).
ocupa(espania, azul, 1).
ocupa(francia, azul, 1).
ocupa(inglaterra, azul, 2). 
ocupa(aral, negro, 2).
ocupa(china, verde, 1).
ocupa(gobi, verde, 2).
ocupa(india, rojo, 3).
ocupa(iran, verde, 1).

/*continentes*/
continente(americaDelSur).
continente(europa).
continente(asia).

/*objetivos*/
objetivo(rojo, ocuparContinente(asia)).
objetivo(azul, ocuparPaises([argentina, bolivia, francia, inglaterra, china])).
objetivo(verde, destruirJugador(rojo)).
objetivo(negro, ocuparContinente(europa)).

% 1.
estaEnContinente(Jugador,Continente):-
    ocupa(Pais,Jugador,_),
    paisContinente(Continente,Pais).

% 2.
ocuparPaises(Jugador,CantPaisesQueOcupa):-
    objetivo(Jugador,_),
    findall(Pais,ocupa(Pais,Jugador,_),PaisesQueOcupa),
    length(PaisesQueOcupa,CantPaisesQueOcupa).

% 3.
ocuparContinente(Jugador,Continente):-
    todosLosPaisesDeUnContinente(Continente,PaisesDelContinente),
    paisesQueOcupa(Jugador,Paises),
    forall(member(Pais,PaisesDelContinente),member(Pais,Paises)).

paisesQueOcupa(Jugador,Paises):-
    objetivo(Jugador,_),
    findall(Pais,ocupa(Pais,Jugador,_),Paises).

todosLosPaisesDeUnContinente(Continente,PaisesDelContinente):-
    continente(Continente),
    findall(Pais,paisContinente(Continente,Pais),PaisesDelContinente).

% 4.
leFaltaMucho(Jugador,Continente):-
    paisesQueOcupa(Jugador,_),
    continente(Continente),
    findall(Territorio,(paisContinente(Continente,Territorio),not(ocupa(Territorio,Jugador,_))),PaisesQueNoOcupa),
    length(PaisesQueNoOcupa,X),
    X >= 2.

% 5.
sonLimitrofes(Pais1,Pais2):-
    limitrofes(Limitrofes),
    member(Pais1,Limitrofes),
    member(Pais2,Limitrofes),
    Pais1 \= Pais2.

% 6.
esGroso(Jugador):-
    todosLosPaisesImportantes(Importante),
    paisesQueOcupa(Jugador,Paises),
    forall(member(Pais,Importante),member(Pais,Paises)),
    ocuparPaises(Jugador,X),
    X >= 10,
    ejercitosDelJugador(Jugador,Y),
    Y >= 50.

ejercitosDelJugador(Jugador,CantEjercitos):-
    objetivo(Jugador,_),
    findall(Ejercito,ocupa(_,Jugador,Ejercito),Ejercitos),
    sumlist(Ejercitos,CantEjercitos).

todosLosPaisesImportantes(Importantes):-
    findall(Pais,paisImportante(Pais),Importantes).    

% 7.
/*estaEnElHorno(Pais):-
    ocupa(Pais,Jugador,_),
    todosLosPaisesLimitrofes(Pais,Limitrofes),
    forall(member(Lugar,Limitrofes),(ocupa(Lugar,OtroJugador,_),OtroJugador \= Jugador)).
*/
estaEnElHorno(Pais):-
    ocupa(Pais, Jugador, _),
    ocupa(_,OtroJugador,_),
    OtroJugador \= Jugador,
    forall(sonLimitrofes(Pais, PaisLimitrofe),ocupa(PaisLimitrofe, OtroJugador, _)).

todosLosPaisesLimitrofes(Pais,Limitrofes):-
    ocupa(Pais,Jugador,_),
    findall(Territorio,(sonLimitrofes(Pais,Territorio), ocupa(Pais,Jugador,_)),Limitrofes).

% 8.
esCaotico(Continente):-
    continente(Continente),
    findall(Jugador,(paisContinente(Continente,Pais),ocupa(Pais,Jugador,_)),JugadoresRepetidos),
    list_to_set(JugadoresRepetidos,Jugadores),
    length(Jugadores,X),
    X > 3.

% 9.
capoCannonierre(Jugador):-
    todosLosJugadores(Jugadores),
    maplist(ocuparPaises,Jugadores,CantidadPaisesQueOcupancu),
    max_member(ElMasOcupa,CantidadPaisesQueOcupancu),
    nth1(X,CantidadPaisesQueOcupancu,ElMasOcupa),
    nth1(X,Jugadores,Jugador).

todosLosJugadores(Jugadores):-
    findall(Jugador,ocupa(_,Jugador,_),Jugadores).

% 10.
ganadooor(Jugador):-
    objetivo(Jugador, destruirJugador(OtroJugador)),
    paisesQueOcupa(OtroJugador,PaisesOtroJugador),
    paisesQueOcupa(Jugador,PaisesJugador),
    forall(member(Pais,PaisesOtroJugador),member(Pais,PaisesJugador)).
ganadooor(Jugador):-
    objetivo(Jugador,ocuparContinente(Continente)),
    ocuparContinente(Jugador,Continente).
ganadooor(Jugador):-
    objetivo(Jugador, ocuparPaises(Paises)),
    paisesQueOcupa(Jugador,Paises).
