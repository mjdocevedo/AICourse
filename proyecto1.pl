:- dynamic polynomial/3.

valor_max(X,Y,X) :- X>Y, !.
valor_max(_,Y,Y).

comparison(X,Z,_,Z) :- X=\=0,!.
comparison(_,_,W,W) :- W=\= -1,!.
comparison(_,_,_,0).

degree([],_,W,W) :- !.
degree([X|Y],I,W,Deg) :-
  Z is I+1,
  comparison(X,Z,W,D),
  degree(Y,Z,D,Deg),!.

coef_polynomial(A,B,B,[A]) :- !.
coef_polynomial(A,B,I,[0|X]) :-
  Z is I+1,
  coef_polynomial(A,B,Z,X).

create_polynomial(Name,A,B) :-
   coef_polynomial(A,B,0,Coef),
   degree(Coef,-1,-1,Deg),
   write(Coef),nl,
   write(Deg),
   assert(polynomial(Name,Coef,Deg)),!.

plus(Pol1,Pol2,Name) :-
  polynomial(Pol1,Coef1,Deg1),
  polynomial(Pol2,Coef2,Deg2),
  valor_max(Deg1,Deg2,B),
  coef_polynomial(0,B,0,Coef3),
  sum(Coef1,Coef3,CoefA),
  sum(Coef2,CoefA,CoefF),
  degree(CoefF,-1,-1,DegF),
  write(CoefF),nl,
  write(DegF),
  assert(polynomial(Name,CoefF,DegF)).

sum([],W,W) :- !.
sum([X|Y],[Z|W],[A|B]) :-
  A is X+Z,
  sum(Y,W,B).

minus(Pol1,Pol2,Name) :-
  polynomial(Pol1,Coef1,Deg1),
  polynomial(Pol2,Coef2,Deg2),
  valor_max(Deg1,Deg2,B),
  coef_polynomial(0,B,0,Coef3),
  sum(Coef1,Coef3,CoefA),
  substraction(Coef2,CoefA,CoefF),
  degree(CoefF,-1,-1,DegF),
  write(CoefF),nl,
  write(DegF),
  assert(polynomial(Name,CoefF,DegF)).

substraction([],W,W) :- !.
substraction([X|Y],[Z|W],[A|B]) :-
  A is Z-X,
  substraction(Y,W,B).

times(Pol1,Pol2,Name) :-
  polynomial(Pol1,Coef1,Deg1),
  polynomial(Pol2,Coef2,Deg2),
  B is Deg1+Deg2,
  coef_polynomial(0,B,0,Coef3),
  multiply(Coef1,Coef2,Coef3,0,Deg1,Deg2,Result),
  degree(Result,-1,-1,DegF),
  write(Result),nl,
  write(DegF),
  assert(polynomial(Name,Result,DegF)).

multiply(Coef1,Coef2,Coef3,I,I,Deg2,Out):-
  multiply2(Coef1,Coef2,Coef3,I,0,Deg2,Out),!.

multiply(Coef1,Coef2,Coef3,I,Deg1,Deg2,Out) :-
  multiply2(Coef1,Coef2,Coef3,I,0,Deg2,NewList),
  Count is I+1,
  multiply(Coef1,Coef2,NewList,Count,Deg1,Deg2,Out).

multiply2(Coef1,Coef2,Coef3,I,J,J,Out) :-
  Z is I+J,
  iterator(Coef3,Z,0,S1),
  iterator(Coef1,I,0,S2),
  iterator(Coef2,J,0,S3),
  Aux is S1+(S2*S3),
  addToList(Coef3,Aux,Z,0,Out),!.

multiply2(Coef1,Coef2,Coef3,I,J,Deg2,Out) :-
  Z is I+J,
  iterator(Coef3,Z,0,S1),
  iterator(Coef1,I,0,S2),
  iterator(Coef2,J,0,S3),
  Aux is S1+(S2*S3),
  addToList(Coef3,Aux,Z,0,NewList),
  Count is J+1,
  multiply2(Coef1,Coef2,NewList,I,Count,Deg2,Out).

addToList([_|B],New,X,X,[New|B]) :-!.
addToList([A|B],New,Position,Index,[A|Z]) :-
  Aux is Index+1,
  addToList(B,New,Position,Aux,Z).

iterator([A|_],X,X,A) :- !.
iterator([_|B],Position,Index,Out) :-
  Aux is Index+1,
  iterator(B,Position,Aux,Out).

compose(Pol1,Pol2,Name) :-
  polynomial(Pol1,Coef1,Deg1),
  polynomial(Pol2,Coef2,Deg2),
  fcompose(Deg1,[0],Coef1,Coef2,Deg2,Composition),
  degree(Composition,-1,-1,DegC),
  assert(polynomial(Name,Composition,DegC)).

fcompose(-1,C,_,_,_,C) :- !.
fcompose(I,C,Coef1,Coef2,Deg2,Out) :-
  iterator(Coef1,I,0,Term),
  degree(C,-1,-1,DegC),
  B is DegC+Deg2,
  coef_polynomial(0,B,0,CoefF),
  multiply(Coef2,C,CoefF,0,Deg2,DegC,Result1),
  sum([Term],Result1,Result2),
  Aux is I-1,
  fcompose(Aux,Result2,Coef1,Coef2,Deg2,Out).

evaluate(Pol1,X,Y) :-
  polynomial(Pol1,Coef1,Deg1),
  fevaluate(Coef1,X,Deg1,0,Y),
  write(Y),nl.

fevaluate(_,_,-1,P,P) :- !.
fevaluate(W,X,I,P,Out) :-
  iterator(W,I,0, CoefI),
  NewP is CoefI + X*P,
  Aux is I-1,
  fevaluate(W,X,Aux,NewP,Out).


differentiate(Pol1,Name) :-
  polynomial(Pol1,Coef1,Deg1),
  fdifferentiate(Coef1,Deg1,Diff,0),
  degree(CoefA,-1,-1,DegA),
  write(Diff),nl,
  write(DegA),
  assert(polynomial(Name,CoefA,DegA)).

fdifferentiate(_,0,_,_,[0]) :-!.

fdifferentiate(_,C,0,C) :- !.
fdifferentiate(Coef1,Deg1,[Z|W],I) :-
  Aux is I+1,
  iterator(Coef1,Aux,0,X),
  Z is Aux*X,

  fdifferentiate(Coef1,Deg1,W,Aux).
