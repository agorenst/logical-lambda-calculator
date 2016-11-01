name(X) :- not(is_list(X)), ground(X), X \= lambda.
function([lambda, V, B]) :- name(V), expression(B).
application([E1, E2]) :- expression(E1), expression(E2).
expression(L) :- name(L).
expression(L) :- function(L).
expression(L) :- application(L).

function([lambda, V, B], V, B) :- function([lambda, V, B]).
application([E1, E2], E1, E2) :- application([E1, E2]).

% R is the beta reduction of F applied to A, if F is a function with variable
% V and body B, then when replace V in B with A we get R.
beta_reduction(F,A,R) :- function(F,V,B), replace(V,B,A,R).
replace(V,V,A,A) :- name(V).
replace(V,W,_,W) :- name(W), V \= W.
replace(V,F,_,F) :- function(F,V,_).
replace(V,F,A,R) :- function(F,W,B), W \= V, replace(V,B,A,S), function(R,W,S).
replace(V,P,A,R) :- application(P,E1,E2), 
    replace(V,E1,A,R1),
    replace(V,E2,A,R2),
    application(R,R1,R2).

evaluate(L,L) :- name(L).
evaluate(L,R) :- function(L,V,B), evaluate(B,BR), function(R,V,BR).
evaluate(L,R) :- application(L,E1,E2),
    evaluate_application(E1,E2,R).
evaluate_application(E1,E2,R) :- name(E1),
    evaluate(E2,R2),
    application(R,E1,R2).
evaluate_application(E1,E2,R) :- function(E1),
    beta_reduction(E1,E2,S),
    evaluate(S,R).
evaluate_application(E1,E2,R) :- application(E1),
    evaluate(E1,R1), E1 \= R1,
    application(S,R1,E2),
    evaluate(S,R).
evaluate_application(E1,E2,R) :- application(E1),
    evaluate(E1,E1),
    evaluate(E2,R2),
    application(R,E1,R2).

evaluate_star(L,R) :- evaluate(L,S), L \= S, evaluate_star(S,R).
evaluate_star(L,L) :- evaluate(L,L).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

alpha_reduction(L,L) :- name(L).
alpha_reduction(L,R) :- application(L,E1,E2),
    alpha_reduction(E1,R1), alpha_reduction(E2,R2),
    application(R,R1,R2).
alpha_reduction(L,R) :- function(L),
    gensym(alpha_,X),
    beta_reduction(L,X,BR),
    alpha_reduction(BR,ABR),
    function(R,X,ABR).

isomorphic(L,R) :-
    reset_gensym(alpha_),
    alpha_reduction(L,E),
    reset_gensym(alpha_),
    alpha_reduction(R,E).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
reverse_macro(L,[N,E],N) :- isomorphic(L,E).
reverse_macro(L,_,L) :- name(L).
reverse_macro(L,M,R) :- function(L,V,B),
    reverse_macro(B,M,RB),
    function(R,V,RB).
reverse_macro(L,M,R) :- application(L,E1,E2),
    reverse_macro(E1,M,R1),
    reverse_macro(E2,M,R2),
    application(R,R1,R2).

reverse_macros(L,[],L).
reverse_macros(L,[M|T],R) :- reverse_macro(L,M,S), reverse_macros(S,T,R).

reverse_macros_star(L,D,R) :- reverse_macros(L,D,S), S \= L, reverse_macros_star(S,D,R).
reverse_macros_star(L,D,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% That's all that was needed to actually compute lambda calculus expressions!
% About 50-60 lines altogether. The rest is providing I/O to the outside
% world, as well as developing the ability to create "macros", which is just
% a way to save the lambda expressions and build up a more concise way of
% writing big lambda expressions.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

apply_macro(L,[N,E],R) :-
    function(F,N,L),
    beta_reduction(F,E,R).

apply_macros(L,[],L).
apply_macros(L,[M|T],R) :- apply_macro(L,M,S), apply_macros(S,T,R).

% This, I think, takes a list of atoms like ['(', 'lambda, 'x', 'x', ')']
% and turns it into the internal prolog represention [lambda, x, x].
% The tricky part was nesting parenthesis and matching them.
parse(['('|T], [M|L], R) :- parse(T,M,[')'|S]), parse(S,L,R).
parse([')'|T], [], [')'|T]).
parse([N|T], [N|L], R) :- N \= ')', N \= '(', parse(T,L,R).
parse([],[],[]).
parse(S,L) :- parse(S,L,[]).


% This is the main I/O driver: it reads from stdin and tokenizes
% things into "names" or parenthesis. That's it.
read_s(S) :- get_char(C), read_s(C,S).
read_s(C,S)     :- whitespace(C), get_char(D),      read_s(D, S).
read_s(C,[C|S]) :- parens(C),     get_char(D),      read_s(D, S).
read_s(C,[N|S]) :- name_char(C),  read_name(C,N,D), read_s(D, S).
read_s(C,[])    :- end_of_line(C).

whitespace(' ').
parens('(').
parens(')').
name_char(C) :- char_code(C,N), N >= 65, N =< 90. % upper case
name_char(C) :- char_code(C,N), N >= 97, N =< 122. % lower case.
name_char(C) :- char_code(C,N), N == 95.
name_char(C) :- char_code(C,N), N >= 48, N =< 57.
end_of_line('\n').

read_name(C,N,E) :-
    get_char(D),
    read_name_chars(S,D,E),
    atom_chars(N,[C|S]).
read_name_chars([C|S], C, E) :-
    name_char(C), !, % needed for io
    get_char(D),
    read_name_chars(S, D, E).
read_name_chars([],C,C) :- not(name_char(C)).

atom_concat_list([],'').
atom_concat_list([A|T],S) :- atom_concat_list(T,R), atom_concat(A,R,S).

% this creates a "pretty printer" for lambda expressions, translating
% from the internal prolog representation to a more scheme-like parens
% syntax.
internal_lambda_string(L,S) :- function(L,V,B),
    internal_lambda_string(V,VS),
    internal_lambda_string(B,BS),
    atom_concat_list(['(lambda ', VS, ' ', BS, ')'],S).

internal_lambda_string(L,S) :- application(L,F,A),
    internal_lambda_string(F,FS),
    internal_lambda_string(A,AS),
    atom_concat_list(['(', FS, ' ', AS, ')'], S).

internal_lambda_string(L,L) :- name(L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This is the main driver loop.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

execute_command(['define', N, L], OD, [[N,L]|OD]).
execute_command(['execute', L], OD,OD) :-
    internal_lambda_string(L,LS),
    write('Executing '), write(LS), nl,
    apply_macros(L,OD,R),
    internal_lambda_string(R,RS),
    evaluate_star(R,Z),
    internal_lambda_string(Z,ZS),
    write(RS), nl, write('\t=>\t'), nl, write(ZS), nl,
    write('Reversing'), nl,
    reverse_macros_star(Z,OD,FINAL),
    write('Done reversing: '),
    internal_lambda_string(FINAL,FINAL_STRING),
    write(FINAL_STRING), nl.
execute_command(['alpha', L], D, D) :-
    internal_lambda_string(L,LS),
    write('Renaming '), write(LS), nl,
    apply_macros(L,D,R),
    internal_lambda_string(R,RS),
    alpha_reduction(R,A),
    internal_lambda_string(A,AS),
    write(RS), nl, write('\t=>\t'), nl, write(AS), nl.

execute_command(['print'], [],[]) :- nl.
execute_command(['print'], [E|D],[E|D]) :-
    internal_lambda_string(E,S),
    write(S), nl,
    execute_command(['print'], D, D).
execute_command(['halt'],_,_) :- halt.
    

main_loop(OD) :-
    read_s(S),
    parse(S,[L]), % I strip out the other list immediately. Why not.
    execute_command(L,OD,ND),
    main_loop(ND).

main_loop(OD) :- write('Parse error'), nl, main_loop(OD).


:- initialization main.
main :- main_loop([]), halt.
