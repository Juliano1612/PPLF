%-----------fora de ataque------------%

foraAtaque([Q|_], Mx, Rainha) :-
	conferePar(q(Mx,Q), Rainha).

foraAtaque([_|Restante], Mx, Rainha) :-
	MxProx is Mx +1,
	foraAtaque(Restante, MxProx, Rainha).


%----------Confere Pares--------------%

conferePar(q(X,_), q(X,_)).
conferePar(q(_,Y), q(_,Y)).
conferePar(q(X,Y), q(X1,Y1)) :-
	Y1 - Y =:= X1 - X.

conferePar(q(X,Y), q(X1,Y1)) :-
	Y1 - Y =:= X - X1.

%------------ 10 rainhas---------------%

rainhas(A, B, C, D, E, F, G, H, I, J) :-
	(var(A) -> write('Corrija o valor do argumento A!'),false; true),
	(A > 10 -> write('O valor de A deve ser menor do que 10!'), false; true),
	(var(B) -> write('Corrija o valor do argumento B!'),false; true),
	(B > 10 -> write('O valor de B deve ser menor do que 10!'), false; true),
	(var(C) -> write('Corrija o valor do argumento C!'),false; true),
	(C > 10 -> write('O valor de C deve ser menor do que 10!'), false; true),
	(var(D) -> write('Corrija o valor do argumento D!'),false; true),
	(D > 10 -> write('O valor de D deve ser menor do que 10!'), false; true),
	(var(E) -> write('Corrija o valor do argumento E!'),false; true),
	(E > 10 -> write('O valor de E deve ser menor do que 10!'), false; true),
	(var(F) -> write('Corrija o valor do argumento F!'),false; true),
	(F > 10 -> write('O valor de F deve ser menor do que 10!'), false; true),
	(var(G) -> write('Corrija o valor do argumento G!'),false; true),
	(G > 10 -> write('O valor de G deve ser menor do que 10!'), false; true),
	(var(H) -> write('Corrija o valor do argumento H!'),false; true),
	(H > 10 -> write('O valor de H deve ser menor do que 10!'), false; true),
	(var(I) -> write('Corrija o valor do argumento I!'),false; true),
	(I > 10 -> write('O valor de I deve ser menor do que 10!'), false; true),
	(var(J) -> write('Corrija o valor do argumento J!'),false; true),
	(J > 10 -> write('O valor de J deve ser menor do que 10!'), false; true),
	Tab = [A,B,C,D,E,F,G,H,I,J],
	numlist(1,10, Numeros),
	(permutation(Numeros, Tab) -> true; write('Ha valores repetidos nos argumentos!'), false),
	printTabuleiroConflito(Tab),
	nb_getval(flag, X),
	(rainhas(Tab) -> write('Nao ha conflitos!'), true; write('Ha conflitos no tabuleiro. Possivel resposta:'),rainhas(10, Sol),!),
	!.

%---------------N Rainhas------------------%

rainhas(ListaInicial) :-
	length(ListaInicial, T),
	(T >= 4 -> true; write('A quantidade de rainhas deve ser maior do que tres!')),
	nb_setval(counter, T),
	rainhas(T,[], ListaInicial).

rainhas(N, Solucao) :-
	(integer(N) -> true; write('N precisa ser um valor inteiro!'), false),
	(N >= 4 -> true; write('N precisa ser maior do que tres!'),false),
	nb_setval(counter,N),
	rainhas(N, [], Solucao),
	printTabuleiro(Solucao).



rainhas(0, Solucao, Solucao).
rainhas(M, SolucaoParcial, Final) :-
	M > 0,
	nb_getval(counter, N),
	noAlcance(1, N , Y),
	Mx is M + 1,
	\+ foraAtaque(SolucaoParcial, Mx, q(M,Y)),
	MProx is M - 1,
	rainhas(MProx, [Y| SolucaoParcial], Final).


%-----------No Alcance---------------%
%
noAlcance(Min, Max, Min) :-
	Min =< Max.
noAlcance(Min, Max, X) :-
	Min < Max,
	MinProx is Min + 1,
	noAlcance(MinProx, Max, X).

noAlcanceDec(Min, Max, Max) :-
	Min =< Max.
noAlcanceDec(Min, Max, X) :-
	Min < Max,
	MaxProx is Max - 1,
	noAlcanceDec(Min, MaxProx, X).

%------------Exibir Tabuleiro--------%

printLinha([], _Linha) :-
	nl.
printLinha([Y|Restante], Linha) :-
	%fazer lista reversa e testar da mesma forma
	\+ foraAtaque(Restante, Linha + 1, q(Y,Linha)),
	(   Y = Linha -> write('R '); write('_ ')),
	printLinha(Restante, Linha).

printLinha([Y|Restante], Linha) :-
	(   Y = Linha -> write('C '), nb_setval(flag1,'1'); write('_ ')),
	printLinha(Restante,Linha).

printTabuleiro(Tabuleiro):-
	length(Tabuleiro, N),
	nl,
	forall(noAlcanceDec(1, N, Linha),
	       printLinha(Tabuleiro, Linha)
	      ),
	nl.

printTabuleiroConflito(Tabuleiro) :-
	length(Tabuleiro, N),
	nl,
	forall(noAlcance(1, N, Linha),
	       printLinha(Tabuleiro, Linha)
	      ),
	nl.





















