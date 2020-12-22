expression(L) :- name(L).
expression(L) :- function(L).
expression(L) :- application(L).
function([lambda, V, B]) :- name(V), expression(B).
application([E1, E2])    :- expression(E1), expression(E2).
name(X)       :- not(is_list(X)), ground(X), X \= lambda.
beta_reduction([lambda, V, B], A, R) :- replace(V, B, A, R).
replace(V,V,A,A) :- name(V).
replace(V,W,_,W) :- name(W), V \= W.
replace(V,[lambda, V, B],_,[lambda, V, B]).
replace(V,[lambda, W, B],A,[lambda, W, S]) :-
    W \= V,
    replace(V,B,A,S).
replace(V, [E1, E2], A, [R1, R2]) :-
    replace(V,E1,A,R1),
    replace(V,E2,A,R2).
evaluate([E1, E2], R) :-
    evaluate(E1, R1), E1 \= R1,
    evaluate([R1, E2], R).
evaluate([E1, E2], R) :-
    beta_reduction(E1, E2, R1),
    evaluate(R1, R).
evaluate(L, L).
alpha_reduction(L, L) :- name(L).
alpha_reduction([E1, E2], [R1, R2]) :-
    alpha_reduction(E1, R1),
    alpha_reduction(E2, R2).
alpha_reduction([lambda, V, B], [lambda, X, ABB]) :-
    gensym(alpha_,X),
    replace(V, B, X, BB),
    alpha_reduction(BB, ABB).
canon(L, R) :- reset_gensym(alpha_), alpha_reduction(L, R).
isomorphic(A, B) :- canon(A, C), canon(B, C).
desugar([N,E], L, R) :- beta_reduction([lambda, N, L], E, R).
desugar_all(L, D, R) :- foldl(desugar, D, L, R).
resugar([N, E], L, N) :- isomorphic(E, L).
resugar(M, [lambda, V, B], [lambda, V, SB]) :- resugar(M, B, SB).
resugar(M, [E1, E2], [R1, R2]) :-
    resugar(M, E1, R1),
    resugar(M, E2, R2).
resugar(_, L, L).
resugar_all(L, D, R) :- foldl(resugar, D, L, S), S \= L, resugar_all(S, D, R).
resugar_all(L, _, L).
read_s(S)           :- get_char(C),                    read_s(C, S).
read_s('\n',[]).
read_s(' ',S)       :- get_char(D),                    read_s(D, S).
read_s('(',['('|S]) :- get_char(D),                    read_s(D, S).
read_s(')',[')'|S]) :- get_char(D),                    read_s(D, S).
read_s(C,[N|S])     :- name_char(C), read_name(C,N,D), read_s(D, S).
read_name(C,N,E) :-
    read_name_chars(S,C,E),
    atom_chars(N,S).
read_name_chars([C|S], C, E) :-
    name_char(C), !, % needed for io
    get_char(D),
    read_name_chars(S, D, E).
read_name_chars([],C,C) :- not(name_char(C)).
name_char(C) :- char_code(C,N), N >= 65, N =< 90. % upper case
name_char(C) :- char_code(C,N), N >= 97, N =< 122. % lower case.
name_char(C) :- char_code(C,N), N >= 48, N =< 57. % decimals
parse(S,L) :- parse(S, [], [L]).
parse(['('|T], R, [M|L]) :- parse(T, [')'|S], M), parse(S, R, L).
parse([')'|T], [')'|T], []).
parse([N|T], R, [N|L]) :- N \= ')', N \= '(', parse(T, R, L).
parse([],[],[]).
lambda_to_atom(L, R) :- parse(S, L), atomic_list_concat(S, ' ', R).
compute(L, D, R) :-
    desugar_all(L, D, DL),
    evaluate(DL, S),
    resugar_all(S, D, R).
execute_command(['define', N, L], D, [[N,R]|D]) :- compute(L, D, R).
execute_command(['execute', L], D,D) :-
    compute(L, D, R),
    lambda_to_atom(R,O),
    write(O), nl.
execute_command(['halt'],_,_) :- halt.
main_loop(OD) :-
    read_s(S),
    parse(S,L),
    execute_command(L,OD,ND),
    main_loop(ND).
main_loop(OD) :- write('Parse error'), nl, main_loop(OD).
main :- main_loop([]), halt.
