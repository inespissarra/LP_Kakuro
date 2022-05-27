% NOME:INES PISSARRA NUMERO:99236

:- [codigo_comum].

%------------------------------------------------------------------------------
%                combinacoes_soma(N, Els, Soma, Combs)
% combinacoes_soma(N, Els, Soma, Comb), em que N e Soma sao inteiros, e Els uma
% lista de inteiros, significa que Combs eh a lista ordenada cujos elementos
% sao as combinacoes N a N, dos elementos de Els cuja soma eh Soma
%------------------------------------------------------------------------------
combinacoes_soma(N, Els, Soma, Combs):- 
    setof(X, (combinacao(N, Els, X), 
    sum_list(X, Soma)), Combs).

%------------------------------------------------------------------------------
%                permutacoes_soma(N, Els, Soma, Perms)
% permutacoes_soma(N, Els, Soma, Perms), em que N e Soma sao inteiros, e Els 
% uma lista de inteiros, significa que Perms eh a lista ordenada de permutacoes
% das combinacoes N a N, dos elementos Els cuja soma eh Soma.
%------------------------------------------------------------------------------
permutacoes_soma(N, Els, Soma, Perms):- 
    combinacoes_soma(N, Els, Soma, Combs),
    findall(X, findall(Y, (member(C, Combs), permutation(C, Y)), X), Perms1),
    append(Perms1, Perms2),
    sort(Perms2,Perms).

%------------------------------------------------------------------------------
%                        espaco_fila(Fila, Esp, H_V)
% espaco_fila(Fila, Esp, H_V), em que Fila eh uma fila de um puzzle e H_V eh um
% dos atomos h-horizontal ou v-vertical, significa que Esp eh um espaco de Fila
%------------------------------------------------------------------------------
espaco_fila(Fila, espaco(S, Vars), H_V):- 
    ((append([_,[Somas], Vars, [Somas2], _], Fila), nonvar(Somas2));
    append([_,[Somas], Vars], Fila)), 
    Vars\==[], nonvar(Somas), forall(member(X, Vars), var(X)),
    escolhe_H_V(Somas, H_V, S).

escolhe_H_V([V|_], v, V).
escolhe_H_V([_|[H]], h, H).

%------------------------------------------------------------------------------
%                      espacos_fila(H_V, Fila, Espacos)
% espacos_fila(H_V, Fila, Espacos), em que Fila eh uma fila e H_V eh um dos 
% atomos h-horizontal ou v-vertical, significa que Espacos eh a lista de 
% todos os espacos de Fila, da esquerda para a direita.
%------------------------------------------------------------------------------
espacos_fila(H_V, Fila, Espacos):-
    bagof(X, espaco_fila(Fila, X, H_V), Espacos), !.

espacos_fila(_, _, []).

%------------------------------------------------------------------------------
%                       espacos_puzzle(Puzzle, Espacos)
% espacos_puzzle(Puzzle, Espacos), em que Puzzle eh um puzzle, significa que 
% Espacos eh a lista de espacos de todas as colunas e linhas de Puzzle.
%------------------------------------------------------------------------------
espacos_puzzle(Puzzle, Espacos):-
    maplist(espacos_fila(h), Puzzle, Linhas),
    mat_transposta(Puzzle, Transp),
    maplist(espacos_fila(v), Transp, Colunas),
    append(Linhas, Colunas, Espacos2),
    append(Espacos2, Espacos).

%------------------------------------------------------------------------------
%                espacos_com_posicoes_comuns(Espacos, Esp, Esps_com)
% espacos_com_posicoes_comuns(Espacos, Esp, Esps_com), em que Espacos eh uma 
% lista de espacos e Esp um espaco, significa que Esps_com eh a lista de 
% espacos com variaveis em comum com Esp, exceptuando Esp.
%------------------------------------------------------------------------------
espacos_com_posicoes_comuns(Espacos, espaco(_, Vars), Esps_com):- 
    exclude(sem_comuns(Vars), Espacos, Esps_com).

% sem_comuns(Vars, Esp), True se Esp nao tem variaveis pertencentes a Vars e  
% a lista das variaveis de Esp eh diferente de Vars, False caso contrario.
sem_comuns(Vars, espaco(_, Vars2)):- 
    forall(member(X, Vars2), \+(pertence(X, Vars)));
    forall(member(X, Vars2), pertence(X, Vars)).

% pertence(X, Lst), True se X pertence a Lst, False caso contrario.
pertence(X, [E|_]):- X==E, !.
pertence(X, [E|Res]):- X\==E, !, pertence(X, Res).

%------------------------------------------------------------------------------
%                permutacoes_soma_espacos(Espacos, Perms_soma)
% permutacoes_soma_espacos(Espacos, Perms_soma), significa que Perms_Soma eh a
% lista de listas de dois elementos, sendo o primeiro um espaco de Espacos,
% e o segundo a lista de permutacoes cuja soma eh igual a soma do espaco.
%------------------------------------------------------------------------------
permutacoes_soma_espacos([],[]).

permutacoes_soma_espacos([espaco(S, Vars)|Res], [[espaco(S, Vars), P]|P_Res]):-
    length(Vars, Comp), 
    permutacoes_soma(Comp, [1,2,3,4,5,6,7,8,9], S, P),
    permutacoes_soma_espacos(Res, P_Res).

%------------------------------------------------------------------------------
%           permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma)
% permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma) significa que 
% Perm eh uma permutacao possivel para o espaco Esp que nao impossibilita o 
% preenchimento de outros espacos, elementos de Espacos, com posicoes comuns 
% com Esp.
%------------------------------------------------------------------------------
permutacao_possivel_espaco(Perm, espaco(S, Vars), Espacos, Perms_soma):-
    length(Vars, N), 
    permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], S, Perms), 
    member(Perm, Perms), 
    espacos_com_posicoes_comuns(Espacos, espaco(S, Vars), Comuns),
    forall(member(Esp, Comuns),verifica_possivel(Vars, Perm, Esp, Perms_soma)).

% verifica_possivel(Vars, Perm, Esp, Perms_soma), True se, substituindo 
% as variaveis de Vars pela permutacao Perm, Esp continua a ser possivel, 
% False caso contrario.
verifica_possivel(Vars, Perm, espaco(S, Vars2), Perms_soma):-
    Vars = Perm,
    member([espaco(S, Vars2), Perms], Perms_soma),
    member(Vars2, Perms).

%------------------------------------------------------------------------------
%       permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss)
% permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, Perms_poss) significa 
% que Perms_poss eh a lista das permutacoes possiveis para o espaco Esp.
%------------------------------------------------------------------------------
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp, [Vars, Perms_poss]):-
    Esp = espaco(_, Vars),
    bagof(X, permutacao_possivel_espaco(X, Esp, Espacos, Perms_soma),
        Perms_poss), !.

permutacoes_possiveis_espaco(_, _, _, []).

%------------------------------------------------------------------------------
%       permutacoes_possiveis_espacos(Espacos, Perms_poss_esps)
% permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) significa que 
% Perms_poss_esps eh a lista das permutacoes possiveis para todos os espacos 
% de Espacos. 
%------------------------------------------------------------------------------
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
    permutacoes_soma_espacos(Espacos, Perms_soma),
    maplist(permutacoes_possiveis_espaco(Espacos, Perms_soma), 
        Espacos, Perms_poss_esps).

%------------------------------------------------------------------------------
%                  numeros_comuns(Lst_Perms, Numeros_comuns)
% numeros_comuns(Lst_Perms, Numeros_comuns), Numeros_comuns eh a lista de pares
% (pos, num) que significa que todas as permutacoes da lista de permutacoes 
% Lst_Perms contem o numero num na posicao pos.
%------------------------------------------------------------------------------
numeros_comuns([Lst|Res], Numeros_comuns):-
    numeros_comuns(Lst, Res, 1, Numeros_comuns).

numeros_comuns([], _, _, []).
numeros_comuns([Perm|Res], Lst, Pos, [(Pos, Perm)| N_C_Res]):-
    forall((member(X, Lst), nth1(Pos, X, Perm2)), Perm==Perm2), !,
    Pos2 is Pos + 1,
    numeros_comuns(Res, Lst, Pos2, N_C_Res).
numeros_comuns([Perm|Res], Lst, Pos, N_C_Res):-
    \+(forall((member(X, Lst), nth1(Pos, X, Perm2)), Perm==Perm2)), !,
    Pos2 is Pos + 1,
    numeros_comuns(Res, Lst, Pos2, N_C_Res).

%------------------------------------------------------------------------------
%                         atribui_comuns(Perms_Possiveis)
% atribui_comuns(Perms_Possiveis), atualiza a lista de permutacoes possiveis 
% Perms_Possiveis, atribuindo a cada espaco os seus numeros comuns.
%------------------------------------------------------------------------------
atribui_comuns([]).
atribui_comuns([[Vars, Perms]|Res]):-
    numeros_comuns(Perms, Numeros_comuns),
    atribui_comuns(Numeros_comuns, Vars),
    atribui_comuns(Res).

atribui_comuns([], _).
atribui_comuns([(Pos, Numero)|Res], Vars):-
    nth1(Pos, Vars, Numero), atribui_comuns(Res, Vars).

%------------------------------------------------------------------------------
%       retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis)
% retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) significa que 
% Novas_Perms_Possiveis eh o resultado de retirar as permutacoes impossiveis 
% da lista de permutacoes possiveis Perms_Possiveis.
%------------------------------------------------------------------------------
retira_impossiveis([], []).
retira_impossiveis([[Vars, Perms_Poss]|Res],[[Vars, Novas_Perms_Poss]|N_Res]):-
    include(subsumes_term(Vars), Perms_Poss, Novas_Perms_Poss),
    retira_impossiveis(Res, N_Res).

%------------------------------------------------------------------------------
%            simplifica(Perms_Possiveis, Novas_Perms_Possiveis)
% simplifica(Perms_Possiveis, Novas_Perms_Possiveis) significa que 
% Novas_Perms_Possiveis eh o resultado de atribuir os numeros comuns e retirar
% as permutacoes impossiveis de Perms_Possiveis ate nao haver mais alteracoes. 
%------------------------------------------------------------------------------
simplifica(Perms_Possiveis, Perms_Possiveis):- 
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Perms_Possiveis), !.

simplifica(Perms_Possiveis, Novas_Perms_Possiveis):- 
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis2), 
    simplifica(Novas_Perms_Possiveis2, Novas_Perms_Possiveis).

%------------------------------------------------------------------------------
%                    inicializa(Puzzle, Perms_Possiveis)
% inicializa(Puzzle, Perms_Possiveis),em que Puzzle eh um puzzle, significa que
% Perms_Possiveis eh a lista de permutacoes possiveis simplificada para Puzzle
%------------------------------------------------------------------------------
inicializa(Puzzle, Perms_Possiveis):-
    espacos_puzzle(Puzzle, Espacos), 
    permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
    simplifica(Perms_poss_esps, Perms_Possiveis).

%------------------------------------------------------------------------------
%             escolhe_menos_alternativas(Perms_Possiveis, Escolha)
% escolhe_menos_alternativas(Perms_Possiveis, Escolha) significa que Escolha
% eh o elemento com menos permutacoes dos elementos de de Perms_Possiveis com
% mais do que uma permutacao. 
%------------------------------------------------------------------------------
escolhe_menos_alternativas(Perms_Possiveis, Escolha):-
    include(comp_maior_1, Perms_Possiveis, Possiveis),
    compara_perms(Possiveis, Escolha).

% comp_maior_1([Vars, Lst_Perms]), True se o comprimento de Lst_Perms eh maior
% que 1, False caso contrario.
comp_maior_1([_, Lst_Perms]):- length(Lst_Perms, Comp), Comp>1.

% compara_perms(Perms, Escolha) significa que Escolha eh a o elemento de Perms
% com menos permutacoes.
compara_perms([Perms], Perms):-!.
compara_perms([[Vars, Perms]|Res], Escolha):-
    compara_perms(Res, [Vars2, Perms2]),
    length(Perms, C), length(Perms2, C2),
    (C=<C2, Escolha = [Vars, Perms],!; Escolha = [Vars2, Perms2]).

%------------------------------------------------------------------------------
%       experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis)
% experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis), significa
% que Novas_Perms_Possiveis eh o resultado de substituir em Perms_Possiveis uma
% permutacao possivel para o espaco Escolha. 
%------------------------------------------------------------------------------
experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis):-
    Escolha = [Vars, Lst_Perms], member(Perm, Lst_Perms), Vars = Perm,
    substitui(Perms_Possiveis, Escolha, [Vars, [Perm]], Novas_Perms_Possiveis).

% substitui(Lst, E, Subst, Lst_Subst) significa que Lst_Subst eh o resultado
% de substituir o elemento E da lista Lst por Subst.
substitui([], _, _, []).
substitui([El|Res], E, Subst, [Subst|Res]):- 
    El==E, !.
substitui([El|Res], E, Subst, [El|Res2]):- 
    El\==E, !,
    substitui(Res, E, Subst, Res2).

%------------------------------------------------------------------------------
%             resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis)
% resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis), significa que 
% Novas_Perms_Possiveis eh o resultado de escolher o espaco menos alternativo,
% experimentar uma permutacao e simplificar, sucessivamente, ate que todos os 
% espacos so tenham uma permutacao possivel.
%------------------------------------------------------------------------------
resolve_aux(Perms_Possiveis, Perms_Possiveis):-
    \+(escolhe_menos_alternativas(Perms_Possiveis, _)).
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha),
    experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis2),
    simplifica(Novas_Perms_Possiveis2, Novas_Perms_Possiveis3),
    resolve_aux(Novas_Perms_Possiveis3, Novas_Perms_Possiveis), !.

%------------------------------------------------------------------------------
%                              resolve(Puz)
% resolve(Puz), em que Puz eh um puzzle, resolve esse puzzle, isto eh, apos a
% invocacao deste predicado a grelha de Puz tem todas as variaveis substituidas
% por numeros que respeitam as restricoes de Puz.
%------------------------------------------------------------------------------
resolve(Puzzle):-
    inicializa(Puzzle, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _), !.