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
	primerApellido(PAV, PAC),
	segundoApellido(SA),
	nombre(N),
	entFederativa(EF),
%Incluir a futuro conversor a letra mayusculas y si ya son mayusculas comprobar
	write(PAC),
	write(PAV),
	write(SA),
	write(N),
	write(EF).

main():-
	primerApellido(PAV, PAC),
	segundoApellido(SA),
	nombre(N),
%Incluir a futuro conversor a letra mayusculas y si ya son mayusculas comprobar
	write(PAC),
	write(PAV),
	write(SA),
	write(N).

primerVocal([], _).
primerVocal([C | R], V) :- vocal(C), V = C, !. 
primerVocal([C | R], V) :- not(vocal(C)), primerVocal(R, V).

primerApellido(S, L) :-
	write("Ingresa tu primer apellido: "),
	readln(C),
	charName(C, L, R),
	primerVocal(R, S).
%este charName es solo para primer apellido
charName([Texto|_], L, R):- string_chars(Texto, [L | R]).

segundoApellido(L):-
	write("Ingresa tu segundo apellido: "),
	readln(C),
	charName2doAp(C, L).
%este charName es solo para segundo apellido
charName2doAp([Texto|_], L):- string_chars(Texto, [L|_]).

nombre(Res):-write("Ingresa nombre(s): "),
	readln(T), enlistar(T, Res).
	
%por si no es maria ni josé el nombre correspondiente a esta llamada, puede ser la 1ra llamada o puede provenir del proc debajo
enlistar([T|_], Res):- not(jose(T)), not(maria(T)), charName(T, Res).

%por si el primer nombre es josé o maría descarta el proc de arriba, se manda a llamar el este y saca la letra del 2do nombre
enlistar([_,Y|_], Res):- charName(Y, Res).
%solo será para el nombre este proc de charName
charName(Texto, Res):- string_chars(Texto, [L | _]), Res = L, !.


%Entidad Federativa

entFederativa(R):-
	write("Ingresa el estado donde naciste: "),
	readln(T), listaDpalabras(T, [EF|Rest]),
	(aguascalientes(EF) -> R = "as" ;
		campeche(EF) -> R = "cc" ;
		coahuila(EF) -> R = "cl" ;
		%%%
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
		distrito(EF) -> R = "df"
	 ;
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