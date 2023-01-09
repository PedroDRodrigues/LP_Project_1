:-[codigo_comum].

/*AUTOR: Pedro Rodrigues
 IST: al99300
 */


/* ================== Auxiliares gerais ===========================*/
/*verifica se um elemento se encontra numa lista*/
membro_de_lista(_,[]):-
    fail.
membro_de_lista(Membro,[El1|Resto]):-
    Membro==El1,!;
    membro_de_lista(Membro,Resto).

/*verifica se existe itersecao entre duas listas*/
existe_intersecao(_,[]):-
    fail.
existe_intersecao(espaco(N,Lista1),espaco(N1,[El1|Resto])):-
    membro_de_lista(El1,Lista1),!;
    existe_intersecao(espaco(N,Lista1),espaco(N1,Resto)).

/*verifica se a soma dos eleentos de uma lista eh igual a Total*/
verifica_soma(List,Total):- verifica_soma(List,0,Total).

verifica_soma([],Soma,Total):- Soma=Total.
verifica_soma([El1|Resto], Soma,Total):-
    Soma1 is Soma + El1, verifica_soma(Resto,Soma1,Total).






combinacao(N,Els,Soma,Comb):-
    combinacao(N,Els,Comb), verifica_soma(Comb,Soma).

combinacoes_soma(N, Els, Soma, Combs):-
    bagof(Comb, combinacao(N,Els,Soma,Comb), Combs).



todas_perms(Lista,Perms):-
    bagof(Comb,permutation(Lista,Comb),Perms).


permutacoes_soma(N, Els, Soma, Perms):-
    combinacoes_soma(N, Els, Soma, Combs),
    maplist(todas_perms,Combs,Perms1), append(Perms1,Perms2),
    sort(Perms2,Perms).






/**/
espaco_fila(Fila,Espaco,'h'):-
    append([Prefixo,Esp,Sufixo],Fila),
    maplist(var,Esp),
    Esp\==[],
    eh_prefixo(Prefixo),
    eh_sufixo(Sufixo),
    last(Prefixo,L),
    last(L,Soma),
    Espaco = espaco(Soma,Esp).
espaco_fila(Fila,Espaco,'v'):-
    append([Prefixo,Esp,Sufixo],Fila),
    maplist(var,Esp),
    Esp\==[],
    eh_prefixo(Prefixo),
    eh_sufixo(Sufixo),
    last(Prefixo,L),
    L=[Soma,_],
    Espaco = espaco(Soma,Esp).
/*eh_prefixo e eh_sufixo sao auxiliares de espaco fila.
    verificam se o espaco esta no limite da fila ou entre '#'                   */
eh_prefixo([]).
eh_prefixo(Prefixo):-
    last(Prefixo,Last),not(var(Last)), is_list(Last).
eh_sufixo([]).
eh_sufixo([Suf1|_]):-
    is_list(Suf1), not(var(Suf1)), is_list(Suf1).






/**/

espacos_fila(H_V, Fila, Espacos):-
    bagof(Esp,espaco_fila(Fila,Esp,H_V),Espacos),!;
    Espacos=[].


espacos_puzzle(Puzzle, Espacos):-
    maplist(espacos_fila(h),Puzzle,Espacos_n),
    mat_transposta(Puzzle,Puzzle_t),
    maplist(espacos_fila(v),Puzzle_t,Espacos_t),
    append(Espacos_n,Espacos_t,Todos_Espacos),
    append(Todos_Espacos,Espacos),!.



espacos_com_posicoes_comuns(Espacos,Esp,Esps_com):-
    include(existe_intersecao(Esp),Espacos,Esps_com_aux),
    exclude(==(Esp), Esps_com_aux, Esps_com),!.



ve_se_cabe_perm(Esp,Perm):-
    unifiable(Perm,Esp,[_|_]).



permutacoes_soma_espacos_aux(Espaco, [Espaco,Perms]):-
    Espaco=espaco(Soma,Esp),
    length(Esp,N),
    permutacoes_soma(N, [1,2,3,4,5,6,7,8,9], Soma, Perms1),
    include(ve_se_cabe_perm(Esp),Perms1,Perms).


permutacoes_soma_espacos(Espacos, Perms_soma):-
    maplist(permutacoes_soma_espacos_aux,Espacos,Perms_soma).



permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma):-
    member([Esp,Perms],Perms_soma),
    member(Perm,Perms),
    Esp=espaco(_,E),
    E=Perm,
    espacos_com_posicoes_comuns(Espacos,Esp,Esps_com),
    permutacoes_soma_espacos(Esps_com, Perms_somanew),
    permutacao_possivel_esp_aux(Perms_somanew).


/*esta funcao auxiliar percorre todos os espacos e
    verifica se existe pelo menos uma permutacao possivel para os restantes espacos
    do puzzle depois de preencher a permutacao que se esta a testar em
    permutacao_possivel_esp*/
permutacao_possivel_esp_aux([]):- true.
permutacao_possivel_esp_aux([[_,Lst]|Esps]):-
    Lst\==[],
    permutacao_possivel_esp_aux(Esps).

retira_repetidas([],[]).
retira_repetidas([Perm1|Perms], [Perm1|Perms1]):-
    exclude(==(Perm1),Perms,Lst),
    retira_repetidas(Lst,Perms1).

permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,[L,Perms]):-
    Esp=espaco(_,L),
    findall(Perm,permutacao_possivel_espaco(Perm,Esp,Espacos,Perms_soma),Perms1),!,
    retira_repetidas(Perms1,Perms).


permutacoes_possiveis_espacos(Espacos, Perms_poss_esps):-
    permutacoes_soma_espacos(Espacos,Perms_soma),
    permutacoes_possiveis_espacos(Espacos,Perms_poss_esps,Perms_soma,Espacos),!.

permutacoes_possiveis_espacos(_,[],_,[]).
permutacoes_possiveis_espacos(Espacos, [Perm1|Resto], Perms_soma, [Esp1|Esps]):-

    permutacoes_possiveis_espaco(Espacos,Perms_soma,Esp1,Perm1),
    permutacoes_possiveis_espacos(Espacos,Resto,Perms_soma,Esps).



/* numeros_comuns:
    recebe uma lista de permutacoes e retorna os numeros que sao comuns
    em todas elas.*/
numeros_comuns(Lst_Perms,Numeros_comuns):-
    Lst_Perms=[Num|_],
    numeros_comuns_aux(Lst_Perms,Numeros_comuns,Num,1),!;
    Numeros_comuns=[].
/*tem como objetivo o mesmo que a funrao original, mas recebe mais dois
    argumentos para auxilio.*/
numeros_comuns_aux(_,[],[],_).
numeros_comuns_aux(Lst_Perms,[Numero|Numeros_coms],[Numero1|Numeros],C):-
    verifica_numeros_pos_C(Lst_Perms,Numero1,C),
    Numero=(C,Numero1) , C1 is C+1,
    numeros_comuns_aux(Lst_Perms,Numeros_coms,Numeros,C1).
numeros_comuns_aux(Lst_Perms,Numeros_coms,[_|Numeros],C):-
    C1 is C+1,
    numeros_comuns_aux(Lst_Perms,Numeros_coms,Numeros,C1).

/*recebe uma lista de permutacoes, uma numeros 'Numero1' e uma constante e
    verifica se todos os numeros da posicao C das permutacoes sao iguais
    ao numero 'Numero1'*/
verifica_numeros_pos_C([],_,_):- true.
verifica_numeros_pos_C([Num|Lst_Perms],Numero1,C):-
    nth1(C,Num,Numero1),verifica_numeros_pos_C(Lst_Perms,Numero1,C).



/*-----------------------//----------------------*/
/*atribui_comuns:
  recebe uma lista de elementos do tipo [espaco, Permutacoes]
  e atribui os numeros comuns ao espaco (para todos os espacos linha) */
atribui_comuns([]).
atribui_comuns([[Espaco,Permutacoes]|Nums_Possiveis]):-
    numeros_comuns(Permutacoes,Permutacoes_comuns),
    faz_atribuicao(Espaco,Permutacoes_comuns),
    atribui_comuns(Nums_Possiveis).

/*
  recebe um espaco e um conjunto de numeros comuns e
  atrbui-as ao espaco.*/
faz_atribuicao(_,[]).
faz_atribuicao(Espaco,[(C,Letra)|Numeros_comuns]):-
    nth1(C,Espaco,Letra),
    faz_atribuicao(Espaco,Numeros_comuns).


/*-----------------------//----------------------*/

/*retira_impossiveis:
    recebe um conjunto de permutacoes possiveis e devolve esse conjunto sem as
    permutacoes que por qualquer motivo, deixaram de poder ser unificadas com
    o espaco que tinham correspondente.*/
retira_impossiveis([],[]).
retira_impossiveis([[Espaco,Perms]|Perms_Possiveis],[[Espaco,Novas_Perms]|Novas_Perms_Possiveis]):-
    exclude(\=(Espaco),Perms,Novas_Perms),
    retira_impossiveis(Perms_Possiveis,Novas_Perms_Possiveis).

/*-----------------------//----------------------*/

/*simplifica:
  esta funcao aplica sucessivamente a uma lista de permutacoes possiveis os
  predicados atribui_comuns, retira_impossiveis e retira_unicas ate a lista
  antes da aplicacao destes predicados seja igual ah lista apos a aplicacao.*/
simplifica(Perms_Possiveis,Novas_Perms_Possiveis):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis,Novas_Perms_Possiveis2),
    Perms_Possiveis==Novas_Perms_Possiveis2,
    Novas_Perms_Possiveis=Novas_Perms_Possiveis2,!.
simplifica(Perms_Possiveis,Novas_Perms_Possiveis):-
    atribui_comuns(Perms_Possiveis),
    retira_impossiveis(Perms_Possiveis,Novas_Perms_Possiveis2),
    simplifica(Novas_Perms_Possiveis2,Novas_Perms_Possiveis),!.


inicializa(Puzzle,Perms_Possiveis):-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Perms_Possiveis1),
    simplifica(Perms_Possiveis1, Perms_Possiveis).



/*============================ RESOLUCAO =================================*/


/* escolhe_menos_alternativas:
    percorre a lista de permutacoes_possiveis e escolhe o elemento que tem menor
    numero de permutacoes. caso haja dois ou mais com o mesmo numero, escolhe o prmeiro.
    se a lista tiver so elementos com uma permutacao, a funcao retorna 'false'.*/
escolhe_menos_alternativas([X],X):-
    X=[_,Perms],length(Perms,C),C>1,!.
escolhe_menos_alternativas([X,Y|Cauda], Escolha):-
    Y=[_,Perms2],length(Perms2,1),escolhe_menos_alternativas([X|Cauda], Escolha),!.
escolhe_menos_alternativas([X,Y|Cauda], Escolha):-
    X=[_,Perms1],Y=[_,Perms2],length(Perms1,X1),length(Perms2,Y2),
    X1=<Y2, X1>1 ,escolhe_menos_alternativas([X|Cauda],Escolha),!.
escolhe_menos_alternativas([_|Cauda],Escolha):-
    escolhe_menos_alternativas(Cauda,Escolha),!.



/*-----------------------//----------------------*/

/*experimenta_pal:
    recebe uma escolha =['espaco','permutacoes'], e uma lista de permutacoes possiveis,
    atualiza a lista de permutacoes possiveis , substituindo a linha que correspondente
    ah escolha, por uma copia mas apenas com a primeira permutacao de 'permutacoes'*/
experimenta_perm(Escolha, [Perm|Perms_Possiveis], [Nova_Perm|Novas_Perms_Possiveis]):-
    Escolha == Perm, Perm=[Esp,Perms],
    member(A,Perms),
    Esp=A,
    Nova_Perm=[Esp,[A]],
    Perms_Possiveis=Novas_Perms_Possiveis
    ;
    Nova_Perm=Perm,
    experimenta_perm(Escolha,Perms_Possiveis,Novas_Perms_Possiveis).





/*resolve_aux:
    recebe uma lista de permutacoes possiveis e aplica sucessivamente os predicados
    escolhe_menos_alternativas, esperimenta_perm e simplifica ate os espacos estarem
    todos preenchidos.*/
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    verifica_estado_final(Perms_Possiveis,1),
    atribui_comuns(Perms_Possiveis),
    Perms_Possiveis = Novas_Perms_Possiveis.
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis):-
    escolhe_menos_alternativas(Perms_Possiveis,Escolha),
    experimenta_perm(Escolha,Perms_Possiveis, Perms_Possiveis_2),
    simplifica(Perms_Possiveis_2,Novas_Perms),
    verifica_estado(Novas_Perms,1),
    resolve_aux(Novas_Perms, Novas_Perms_Possiveis).


/*verifica se nao existe nenhuma lista de permutacoes vazia em Perms_Possiveis */
verifica_estado([],1).
verifica_estado([[_,Perm]|Perms_Possiveis],Flag):-
    length(Perm,C),C>0,
    verifica_estado(Perms_Possiveis,Flag).


/*verifica se os espacos ja so teem uma permutacao possivel cada.*/
verifica_estado_final([],1).
verifica_estado_final([[_,Perm]|Perms_Possiveis],Flag):-
    length(Perm,1),
    verifica_estado_final(Perms_Possiveis,Flag).


/*-----------------------//----------------------*/
/*resolve:
    funcao que atravez da aplicacao dos predicados inicializa e resolve_aux
    resolve o puzzle.*/
resolve(Puz):-
    inicializa(Puz,Pals_Possiveis),
    resolve_aux(Pals_Possiveis,_),!.
