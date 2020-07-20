vocal(a).
vocal(e).
vocal(i).
vocal(o).
vocal(u).
jose(jose).
maria(maria).

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

main():- 
	primerApellido(PAV, PAC, PACI), 
	segundoApellido(SA, SAC),
	nombre(N, NC),
	entFederativa(EF),
%Incluir a futuro conversor a letra mayusculas y si ya son mayusculas comprobar
	write(PAC),
	write(PAV),
	write(SA),
	write(N),
	write(EF).

% Realiza corte al encontrar una vocal, de lo contrario 
% se hace el caso recursvio con el resto de la lista.
primerVocal([], _).
primerVocal([C | _], C) :- vocal(C), !. 
primerVocal([_ | R], V) :- primerVocal(R, V).

% Extracción de consonantes
% Igual que las vocales pero con condición invertida
primerConsonante([], _).
primerConsonante([C | _], C) :- not(vocal(C)), !.
primerConsonante([_ | R], C) :- primerConsonante(R, C).

charName([Texto|_], L, R):- write(Texto), string_chars(Texto, [L | R]).

primerApellido(S, L, I) :-
	write("Ingresa tu primer apellido: "),
	readln(C),
	charName(C, L, R),
	primerVocal(R, S),
	primerConsonante(R, I).

segundoApellido(L, CI):-
	write("Ingresa tu segundo apellido: "),
	readln(C),
	charName(C, L, R), % Se extrae la primera letra de la lista y la primer consonante del resto
	primerConsonante(R, CI).

nombre(Res, Ci):- write("Ingresa nombre(s): "),
	readln(T), enlistar(T, Res, Ci),!.
	
%por si no es maria ni josé el nombre correspondiente a esta llamada, puede ser la 1ra llamada o puede provenir del proc debajo
enlistar([T|_], Res, Ci):- not(jose(T)), not(maria(T)), charNameNombre(T, Res, Ci).

%por si el primer nombre es josé o maría descarta el proc de arriba, se manda a llamar el este y saca la letra del 2do nombre
enlistar([_,Y|_], Res, Ci):- charNameNombre(Y, Res, Ci).

%solo será para el nombre este proc de charNameNombre
charNameNombre(Texto, Res, Ci):- string_chars(Texto, [Res | R]), primerConsonante(R, Ci), !.

%Entidad Federativa
entFederativa(R):-
	write("Ingresa el estado donde naciste: "),
	readln(T), listaDpalabras(T, [EF|Rest]),
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
		entFederativa2(Rest, R)
	).
entFederativa2([], R):-
	R = "ne".
	
entFederativa2([EF|[]], R):-
	(	roo(EF) -> R = "qr" ;
		leon(EF) -> R = "nl" ;
		R = "ne"
	).
entFederativa2([_,Y], R):-
	(
		sur(Y) -> R = "bs" ;
		norte(Y) -> R = "bc" ;
		mexico(Y) -> R = "df" ;
		potosi(Y) -> R = "sp" 
		;
		R = "ne"
	).
entFederativa2([_,_|_], R):-
		R = "ne".

		
listaDpalabras(T, REF):-
	REF = T.

% Determinar sexo
 hombreMujer(R) :- write("ingresa tu sexo: "), readln(G), primerPalabra(G, R2), string_chars(R2, R).  
 primerPalabra([Texto | _], Texto).

prueba(A) :- write('Entra: '), readln(C), write(C), string_chars(C, A) .

% Fecha de nacimiento 
validarAño(D, M ,A):- write("Ingresa tu año de nacimiento: "), readln([A]),
                write("Ingresa tu mes de nacimiento: "), readln([ML]),
				write("Ingresa tu día de nacimiento: "), readln([D]),
				write("Tu fecha de nacimiento es: "),
				numeroMes(ML, M),
				write(A), write(M), write(D), A > 1900 , A < 2020.

numeroMes(enero, 01).
numeroMes(febrero, 02).
numeroMes(marzo, 03).
numeroMes(abril 04).
numeroMes(mayo, 05).
numeroMes(junio, 06).
numeroMes(julio, 07).
numeroMes(agosto, 08).
numeroMes(septiembre, 09).
numeroMes(octubre, 10).
numeroMes(noviembre, 11).
numeroMes(diciembre, 12).

% 65 al 90 las letras en ASCII
homoClave(A, HC) :- (A < 2000 -> random(0, 9, H) ; random(65, 90, C), char_code(H, C)), random(0, 9, UD), string_concat(H, UD, HUD), text_to_string(HC, HUD).
