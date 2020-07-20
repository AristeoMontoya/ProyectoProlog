vocal(a).
vocal(e).
vocal(i).
vocal(o).
vocal(u).

compuesto(jose).
compuesto(maria).

%Entidad Federativa
aguascalientes(aguascalientes). 
campeche(campeche). 
coahuila(coahuila).
colima(colima).
chiapas(chiapas).
chihuahua(chihuahua).
durango(durango).
guanajuato(guanajuato).
guerrero(guerrero).
hidalgo(hidalgo).
jalisco(jalisco).
michoacan(michoacan).
morelos(morelos).
nayarit(nayarit).
oaxaca(oaxaca).
puebla(puebla).
queretaro(queretaro).
sinaloa(sinaloa).
sonora(sonora).
tabasco(tabasco).
tamaulipas(tamaulipas).
tlaxcala(tlaxcala).
veracruz(veracruz).
yucatan(yucatan).
zacatecas(zacatecas).

distrito(distrito). federal(federal).
quintana(quintana). roo(roo).
nuevo(nuevo). leon(leon).

ciudad(ciudad). de(de). mexico(mexico).
san(san). luis(luis). potosi(potosi). 
baja(baja). california(california). sur(sur). norte(norte).

%numero representa a meses
numero_mes(enero, '01').
numero_mes(febrero, '02').
numero_mes(marzo, '03').
numero_mes(abril, '04').
numero_mes(mayo, '05').
numero_mes(junio, '06').
numero_mes(julio, '07').
numero_mes(agosto, '08').
numero_mes(septiembre, '09').
numero_mes(octubre, '10').
numero_mes(noviembre, '11').
numero_mes(diciembre, '12').

generar(Curp):- 
	primer_apellido(PAV, PAC, PACI), 
	segundo_apellido(SA, SAC),
	nombre(N, NC),
	fecha_nacimiento(D, M, A4),
	entidad_federativa(EF),
	genero(S),
	homoclave(A4, HC),
	año_cuatro_digitos(A4, A2),
	concatenar([PAC, PAV, SA, N, A2, M, D, S, EF, PACI, SAC, NC, HC], Curp).

% Concatena de forma recursiva los elementos de una lista en un String
concatenar([A], A) :- !.
concatenar([A | R], Curp) :- concatenar(R, B), string_concat(A, B, Curp1), string_upper(Curp1, Curp).

% Realiza corte al encontrar una vocal, de lo contrario 
% se hace el caso recursvio con el resto de la lista.
primer_vocal([], _).
primer_vocal([C | _], C) :- vocal(C), !. 
primer_vocal([_ | R], V) :- primer_vocal(R, V).

%valida formato fecha de nacimiento
año([_,_|B], R) :- string_to_atom(B, X), atom_number(X, R).
año_cuatro_digitos(N, R) :- atom_chars(N, X), año(X, R).

% Extracción de consonantes
% Igual que las vocales pero con condición invertida
primer_consonante([], _).
primer_consonante([C | _], C) :- not(vocal(C)), !.
primer_consonante([_ | R], C) :- primer_consonante(R, C).

charName([Texto|_], L, R):- string_chars(Texto, [L | R]).

primer_apellido(S, L, I) :-
	write("Ingresa tu primer apellido: "),
	readln(C),
	charName(C, L, R),
	primer_vocal(R, S),
	primer_consonante(R, I).

segundo_apellido(L, CI):- write("Ingresa tu segundo apellido: "),
						  readln(C),
						  charName(C, L, R), % Se extrae la primera letra de la lista y la primer consonante del resto
						  primer_consonante(R, CI).

nombre(Res, Ci):- write("Ingresa nombre(s): "),
				  readln(T), enlistar(T, Res, Ci),!.
	
%por si no es maria ni josé el nombre correspondiente a esta llamada, puede ser la 1ra llamada o puede provenir del proc debajo
enlistar([T | _], Res, Ci):- not(compuesto(T)), charNameNombre(T, Res, Ci).

%por si el primer nombre es josé o maría descarta el proc de arriba, se manda a llamar el este y saca la letra del 2do nombre
enlistar([_, Y | _], Res, Ci):- charNameNombre(Y, Res, Ci).

%solo será para el nombre este proc de charNameNombre
charNameNombre(Texto, Res, Ci):- string_chars(Texto, [Res | R]), primer_consonante(R, Ci), !.

%Entidad Federativa
entidad_federativa(R):-
	write("Ingresa el estado donde naciste: "),
	readln(T), lista_palabras(T, [EF | Rest]),
	(aguascalientes(EF) -> R = "as" ;
		campeche(EF) -> R = "cc" ;
		coahuila(EF) -> R = "cl" ;
		colima(EF) -> R = "cm" ;
		chiapas(EF) -> R = "cs" ;
		chihuahua(EF) -> R = "ch" ;
		durango(EF) -> R = "dg" ;
		guanajuato(EF) -> R = "gt" ;
		guerrero(EF) -> R = "gr" ;
		hidalgo(EF) -> R = "hr" ; 
		jalisco(EF) -> R = "jc" ;
		mexico(EF) -> R = "mc" ;
		michoacan(EF) -> R = "mn" ;
		morelos(EF) -> R = "ms" ;
		nayarit(EF) -> R = "nt" ;
		oaxaca(EF) -> R = "oc" ;
		puebla(EF) -> R = "pl" ;
		queretaro(EF) -> R = "qt" ;
		sinaloa(EF) -> R = "sl" ;
		sonora(EF) -> R = "sr" ;
		tabasco(EF) -> R = "tc" ;
		tamaulipas(EF) -> R = "ts" ;
		tlaxcala(EF) -> R = "tl" ;
		veracruz(EF) -> R = "vz" ;
		yucatan(EF) -> R = "yn" ;
		zacatecas(EF) -> R = "zs" ;
		distrito(EF) -> R = "df";
		entidad_federativa2(Rest, R)
	).
entidad_federativa2([], R):- R = "ne".
entidad_federativa2([EF | []], R):-
	(	roo(EF) -> R = "qr" ;
		leon(EF) -> R = "nl" ;
		R = "ne"
	).
entidad_federativa2([_, Y], R):-
	(
		sur(Y) -> R = "bs" ;
		norte(Y) -> R = "bc" ;
		mexico(Y) -> R = "df" ;
		potosi(Y) -> R = "sp" 
		;
		R = "ne"
	).
entidad_federativa2([_, _ | _], R):- R = "ne".

lista_palabras(T, T).

% Determinar sexo
genero(R) :- write("Ingresa tu sexo: "), readln([G]), string_chars(G, [R | _]).  

%convertir a formato CURP la fecha de nacimiento
fecha_nacimiento(D, M, A) :- write('Ingresa el año de nacimiento: '), readln([A]), 
							 write('Ingresa el mes de nacimento: '), readln([M1]), numero_mes(M1, M),
							 write('Ingresa el día de nacimiento: '), readln([D]).

% Genera dos dígitos para años menores a 2000, de lo contrario genera un número entre el 65 y el 90,
% que son los números que representan de A a Z en ASCII, luego convierte ese número en el caracter correspondiente
homoclave(A, HC) :- (A < 2000 -> random(0, 9, I) ; random(65, 90, C), char_code(I, C)), random(0, 9, UD), string_concat(I, UD, HC).
