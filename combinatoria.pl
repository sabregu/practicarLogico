%  MODULO 7 
%  ACTIVIDADES
actividad(cine).
actividad(arjona).
actividad(princesas_on_ice).
actividad(pool).
actividad(bowling).
costo(cine, 400).
costo(arjona, 1750).
costo(princesas_on_ice, 2500).
costo(pool, 350).
costo(bowling, 300).

actividades(Plata,ActividadesPosibles):-
    findall(Actividad,actividad(Actividad),Actividades),
    actividadesPosibles(Plata,Actividades,ActividadesPosibles).

actividadesPosibles(_,[],[]).

actividadesPosibles(Plata,[Actividad|Actividades],[Actividad|Posibles]):-
    costo(Actividad,Costo),
    Costo =<  Plata,
    Resto is Plata - Costo,
    actividadesPosibles(Resto,Actividades,Posibles).

actividadesPosibles(Plata, [_|Actividades], Posibles):-
    actividadesPosibles(Plata,Actividades, Posibles).

% BARCO PIRATA
%Combinatoria sin repetición 
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
    tripulantes(Candidatos, Tripulantes),
    Tripulantes \= [].

candidato(UnTripulante):- cocinero(UnTripulante).
candidato(UnTripulante):- bravo(UnTripulante), pirata(UnTripulante, _).
candidato(UnTripulante):- pirata(UnTripulante, Edad), Edad > 40.

candidatos(Candidatos):-
    findall(UnCandidato, candidato(UnCandidato), CandidatosConRepetidos),
    list_to_set(CandidatosConRepetidos, Candidatos).


% En esta versión, no hay cláusula en la cual se omita a un candidato específico,
% así que el corte se hace en cualquier momento.
tripulantes(_, []).
tripulantes(Candidatos, [UnCandidato|Tripulantes]):-
    select(UnCandidato, Candidatos, OtrosCandidatos),
    tripulantes(OtrosCandidatos, Tripulantes).

% SIMULACRO
% puedeCumplir(Persona, Rol): relaciona una persona con un rol que puede cumplir
puedeCumplir(jorge, instrumentista(guitarra)).
puedeCumplir(daniel, instrumentista(guitarra)).
puedeCumplir(daniel, actor(narrador)).
puedeCumplir(daniel, instrumentista(tuba)).
puedeCumplir(daniel, actor(paciente)).
puedeCumplir(marcos, actor(narrador)).
puedeCumplir(marcos, actor(psicologo)).
puedeCumplir(marcos, instrumentista(percusion)).
puedeCumplir(daniel, instrumentista(percusion)).
puedeCumplir(carlos, instrumentista(violin)).
puedeCumplir(carlitos, instrumentista(piano)).
puedeCumplir(daniel, actor(canto)).
puedeCumplir(carlos, actor(canto)).
puedeCumplir(carlitos, actor(canto)).
puedeCumplir(marcos, actor(canto)).
puedeCumplir(jorge, actor(canto)).
puedeCumplir(jorge, instrumentista(bolarmonio)).

% necesita(Sketch, Rol): relaciona un sketch con un rol necesario para interpretarlo.
necesita(payadaDeLaVaca, instrumentista(guitarra)).
necesita(malPuntuado, actor(narrador)).
necesita(laBellaYGraciosaMozaMarchoseALavarLaRopa, actor(canto)).
necesita(laBellaYGraciosaMozaMarchoseALavarLaRopa, instrumentista(violin)).
necesita(laBellaYGraciosaMozaMarchoseALavarLaRopa, instrumentista(tuba)).
necesita(lutherapia, actor(paciente)).
necesita(lutherapia, actor(psicologo)).
necesita(cantataDelAdelantadoDonRodrigoDiazDeCarreras, actor(narrador)).
necesita(cantataDelAdelantadoDonRodrigoDiazDeCarreras, instrumentista(percusion)).
necesita(cantataDelAdelantadoDonRodrigoDiazDeCarreras, actor(canto)).
necesita(rhapsodyInBalls, instrumentista(bolarmonio)).
necesita(rhapsodyInBalls, instrumentista(piano)).

% duracion(Sketch, Duracion):. relaciona un sketch con la duración (aproximada, pero la vamos a tomar como fija) que se necesita para interpretarlo.
duracion(payadaDeLaVaca, 9).
duracion(malPuntuado, 6).
duracion(laBellaYGraciosaMozaMarchoseALavarLaRopa, 8).
duracion(lutherapia, 15).
duracion(cantataDelAdelantadoDonRodrigoDiazDeCarreras, 17).
duracion(rhapsodyInBalls, 7).

% 1.
interprete(Persona,SketchEnElquePuedeParticipar):-
    necesita(SketchEnElquePuedeParticipar,Rol),
    puedeCumplir(Persona,Rol).

% 2.
duracionTotal(Sketches,DuracionTotal):-
    maplist(duracion,Sketches,HorasLista),
    sumlist(HorasLista,DuracionTotal).

% 3.
puedeSerInterpretado(Sketch,Interpretes):-
    sketch(Sketch),
    findall(RolNecesario,necesita(Sketch,RolNecesario),RolesNecesarios),
    findall(RolActual,(member(Interprete,Interpretes),puedeCumplir(Interprete,RolActual)),RolesActuales),
    subset(RolesNecesarios,RolesActuales).

todosLosInterpretes(Interpretes):-
    findall(Interprete,puedeCumplir(Interprete,_),InterpretesConRepetidos),
    list_to_set(InterpretesConRepetidos,Interpretes).

sketch(Sketch):-
    distinct(Sketch,necesita(Sketch,_)).

% 4.
% Es un tipo de combinatoria sin repeticion, al estilo de los piratas
% Vamos pickear un sketch si cumple que los interpretes lo pueden hacer.
generarShow(Interpretes,DuracionMaxima,Show):-
    sketchesPosibles(Interpretes,SketchesPosibles),
    sketches(SketchesPosibles,Show),
    duracionTotal(Show,Duracion),
    Duracion < DuracionMaxima,
    Show \= [].

sketches([],[]).
sketches(SketchesPosibles,[Sketch|Sketches]):-
    select(Sketch,SketchesPosibles,OtrosSketchesPosibles),
    sketches(OtrosSketchesPosibles,Sketches).

sketchesPosibles(Interpretes,Show):-
    findall(Sketch,puedeSerInterpretado(Sketch,Interpretes),Show).