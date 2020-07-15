vocal(a).
vocal(e).
vocal(i).
vocal(o).
vocal(u).

primerVocal([], _).
primerVocal([C | R], V) :- vocal(C), V = C, !. 
primerVocal([C | R], V) :- not(vocal(C)), primerVocal(R, V).

prueba(S, R) :- read(C), string_chars(C, [L | R1]), write(L), primerVocal(R1, S).
