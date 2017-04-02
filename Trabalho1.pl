solucao_rainhas(L,S) :-
	entrar(1,L,N),
	permutation(N,S),
	mantem(S).

entrar(K,L,[K|N]) :- K<L, K1 is K+1, entrar(K1,L,N).
entrar(L,L,[L]).

atacar(Q,N) :- atacar(Q,1,N).

atacar(Q,L,[O|_]) :- Q is O+L ; Q is O-L ; Q is O.
atacar(Q,L,[_|Os]) :- L1 is L+1, atacar(Q,L1,Os).

mantem([O|Os]) :- mantem(Os), not(atacar(O,Os)).
mantem([]).

rainhas(A,B,C,D,E,F,G,H,I,J) :-
	verifica_var(A,B,C,D,E,F,G,H,I,J),
	imprime_tudo([A,B,C,D,E,F,G,H,I,J]),
        not(mantem([A,B,C,D,E,F,G,H,I,J])), !,
	solucao_rainhas(10,S),
	imprime_tudo(S).

verifica_var(A,B,C,D,E,F,G,H,I,J) :-
	(   var(A) -> write('instancie a variavel A'), false; true),
	(   var(B) -> write('instancie a variavel B'), false; true),
	(   var(C) -> write('instancie a variavel C'), false; true),
        (   var(D) -> write('instancie a variavel D'), false; true),
	(   var(E) -> write('instancie a variavel E'), false; true),
	(   var(F) -> write('instancie a variavel F'), false; true),
	(   var(G) -> write('instancie a variavel G'), false; true),
	(   var(H) -> write('instancie a variavel H'), false; true),
	(   var(I) -> write('instancie a variavel I'), false; true),
	(   var(J) -> write('instancie a variavel J'), false; true).

imprime_tudo([]):-
	nl.
imprime_tudo([Q|L]):-
	imprime_linha(Q,L,1),
	imprime_tudo(L).


imprime_linha(_,_,11):-
	nl.

imprime_linha(Q,L,Cont):-
    Cont == Q,
    not(atacar(Q,L)),
    write(' R '), Cont1 is Cont+1,
    imprime_linha(Q,L,Cont1),!.

imprime_linha(Q,L,Cont):-
    Cont == Q,
    atacar(Q,L),
    write(' C '), Cont1 is Cont+1,
    imprime_linha(Q,L,Cont1),!.

imprime_linha(Q,L,Cont):-
    Cont =\= Q,
    write(' - '), Cont1 is Cont+1,
    imprime_linha(Q,L,Cont1),!.









































