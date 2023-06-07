%P01 (*): Find the last element of a list

% my_last(X,L) :- X is the last element of the list L
%    (element,list) (?,?)

% Note: last(?Elem, ?List) is predefined

my_last(X, [X]).
my_last(X, [_|XS]) :- my_last(X, XS).




% P02 (*): Find the last but one element of a list

% last_but_one(X,L) :- X is the last but one element of the list L
%    (element,list) (?,?)

last_but_one(X,[X,_]).
last_but_one(X,[_|Ys]) :- last_but_one(X,Ys).




% P03 (*): Find the Kth element of a list.

%       The first element in the list is number 1.
%           Example:
%               ?- element_at(X,[a,b,c,d,e],3).
%               X = c

element_at(X,[X|_], 1).
element_at(X,[_|Ys], NUMBER) :- NEW_NUMBER is NUMBER - 1, element_at(X, Ys, NEW_NUMBER).



% P04 (*): Find the number of elements of a list.

% my_length(L,N) :- the list L contains N elements
%    (list,integer) (+,?) 

% Note: length(?List, ?Int) is predefined

my_length([], 0).
my_length([_], NUMBER) :- my_length([], NEW_NUM), NUMBER is NEW_NUM + 1.
my_length([_|Xs], NUMBER) :- my_length(Xs, NEW_NUM), NUMBER is NEW_NUM + 1.




% P05 (*): Reverse a list.

% my_reverse(L1,L2) :- L2 is the list obtained from L1 by reversing 
%    the order of the elements.
%    (list,list) (?,?)

% Note: reverse(+List1, -List2) is predefined

reverse([], []).
reverse([X], [X]).
reverse([X|Xs], NEW_LIST) :- reverse(Xs, LIST), append(LIST, [X], NEW_LIST).



% P06 (*): Find out whether a list is a palindrome
% A palindrome can be read forward or backward; e.g. [x,a,m,a,x]

% is_palindrome(L) :- L is a palindrome list
%    (list) (?)

is_palindrome(L) :- reverse(L, L2), L2 == L.



% P07 (**): Flatten a nested list structure.

% my_flatten(L1,L2) :- the list L2 is obtained from the list L1 by
%    flattening; i.e. if an element of L1 is a list then it is replaced
%    by its elements, recursively. 
%    (list,list) (+,?)

% Note: flatten(+List1, -List2) is a predefined predicate


my_flatten([X|Xs], RESULT) :- append(X, Xs, NEWLIST), my_flatten(NEWLIST, RESULT).


    
% alebo ina verzia:
genV(S,E,S):- S < (E + 1).
genV(S, E, V):- S < (E + 1), NS is S + 1, genV(NS, E, V).

solve(Xm, Ym, X, Y, Z) :-
    genV(1, Xm, X),
    genV(1, Ym, Y),
    L is X * X + Y * Y,
    genV(1, L, M),
    Z is M*M,
    Z == L.

resolve(Xm, Ym, ResL, ResR, X, Y, Z) :-
    genV(1, Xm, X),
    genV(1, Ym, Y),
    L is ResL,
    genV(1, L, Z),
    P is ResR,
    P == L.



cel(N,[_|T]) :- N>0, NN is N-1, cel(NN,T).
cel(N,[]) :-  N =< 0.

fv(E,Vs) :- fv3(E,[],Vs).
    
fv3(var(V),Bs,[]) :- member(V,Bs), !.
fv3(var(V),_,[V]).
fv3(app(E1,E2),Bs,Vs) :- fv3(E1,Bs,V1), fv3(E2,Bs,V2), uni(V1,V2,Vs).
fv3(abs(V,E),Bs,Vs) :- fv3(E,[V|Bs],Vs).

uni([], L, L).
uni([X|Xs], R, Ys) :-
    member(X, R), !, uni(Xs, R, Ys).
uni([X|Xs], R, [X|Ys]) :- uni(Xs, R, Ys).

heh(_,[]) :- !.
heh([Y|Ys], [X|Xs]) :- X == Y, heh(Ys, Xs).