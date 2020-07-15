vocal(a).
vocal(e).
vocal(i).
vocal(o).
vocal(u).
jose(jose).
maria(maria).


primerVocal([], _).
primerVocal([C | R], V) :- vocal(C), V = C, !. 
primerVocal([C | R], V) :- not(vocal(C)), primerVocal(R, V).

prueba(S, L) :-
	write("Ingresa tu primer apellido: "),
	read(C), 
	string_chars(C, [L | R]),
	primerVocal(R, S).
	

nombre(Res):-write("Ingresa nombre(s): "),
	readln(T), enlistar(T, Res).
	
%por si no es maria ni josé el nombre correspondiente a esta llamada, puede ser la 1ra llamada o puede provenir del proc debajo
enlistar([T|_], Res):- not(jose(T)), not(maria(T)), charName(T, Res).

%por si el primer nombre es josé o maría descarta el proc de arriba, se manda a llamar el este y saca la letra del 2do nombre
enlistar([_,Y|_], Res):- charName(Y, Res).

charName(Texto, Res):- string_chars(Texto, [L | _]), Res = L, !.

