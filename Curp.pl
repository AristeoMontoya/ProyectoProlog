vocal(a).
vocal(e).
vocal(i).
vocal(o).
vocal(u).
jose(jose).
maria(maria).


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

