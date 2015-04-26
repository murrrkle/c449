%% CPSC 449: Assignment 5
%% Fall 2014
%% Submitted by HUNG, Michael; UCID 10099049


%% PROBLEM 1 %%
%%%%%%%%%%%%%%%

substitute(X, Y, [], []).
substitute(X, Y, [X | L1],[Y | L2]) :- substitute(X, Y, L1, L2).
substitute(X, Y, [Z | L1], [Z | L2]) :- X \= Z, substitute(X, Y, L1, L2).

%% PROBLEM 2 %%
%%%%%%%%%%%%%%%

% Does permutations instead of combinations. Incomplete implementation.

select_pair(X, Y, [X | L1], R) :- select(Y, L1, R).
select_pair(X, Y, [Z | L1], [Z | L2]) :- select_pair(X, Y, L1, L2).


%% PROBLEM 3 %%
%%%%%%%%%%%%%%%
%% Usage: ? - solution(X, Y).

%% FACTS %%
toRight(A, B, (B, A, _, _, _)).
toRight(A, B, (_, B, A, _, _)).
toRight(A, B, (_, _, B, A, _)).
toRight(A, B, (_, _, _, B, A)).

mid(A, (_, _, A, _, _)).
first(A, (A, _, _, _, _)).

%% RULES %%

nextTo(A, B, H) :- toRight(A, B, H). % Symmetric
nextTo(A, B, H) :- toRight(B, A, H).

exists(A, H) :- toRight(A, _, H). % Symmetric
exists(A, H) :- toRight(_, A, H).

solution(ZebraOwner, WaterDrinker) :- 
	exists(house(englishman, _, _, _, red), Houses), 						 %  The Englishman lives in the red house.
    exists(house(spaniard, dog, _, _, _), Houses), 							 %  The Spaniard owns the dog.
	exists(house(_, _, _, coffee, green), Houses),							 %  Coffee is drunk in the green house.
	exists(house(ukrainian, _, _, tea, _), Houses),							 %  The Ukrainian drinks tea.
	toRight(house(_, _, _, _, green), house(_, _, _, _, ivory), Houses),	 %  The green house is immediately to the right (your right) of the ivory house.
	exists(house(_, snail, winston, _, _), Houses),							 %  The Winston smoker owns snails.
	exists(house(_, _, kools, _, yellow), Houses),							 %  Kools are smoked in the yellow house.
	mid(house(_, _, _, milk, _), Houses),									 %  Milk is drunk in the middle house.
	first(house(norwegian, _, _, _, _), Houses),							 %  The Norwegian lives in the first house on the left.
	nextTo(house(_, _, chesterfields, _, _), house(_, fox, _, _, _),Houses), %  The man who smokes Chesterfields lives in the house next to the man with the fox.
	nextTo(house(_, _, kools, _, _), house(_, horse, _, _, _), Houses),		 %  Kools are smoked in the house next to the house where the horse is kept.
	exists(house(_, _, luckyStrike, orangeJuice, _), Houses), 				 %  The Lucky Strike smoker drinks orange juice.
	exists(house(japanese, _, parliaments, _, _), Houses),					 %  The Japanese smokes Parliaments.
	nextTo(house(norwegian, _, _, _, _), house(_, _, _, _, blue), Houses),	 %  The Norwegian lives next to the blue house.
	exists(house(ZebraOwner, zebra, _, _, _),Houses),						 %  Who owns the zebra?
	exists(house(WaterDrinker, _, _, water, _), Houses).					 %  Who drinks water?
	
	
%% PROBLEM 4 %%
%%%%%%%%%%%%%%%

% Strange error in assoc.

map(test, add(add(add(lit(2), lit(3)), lit(4)), lit(5))). 

expr(lit(_)).
expr(add(E1, E2)) :- expr(E1), expr(E2).
expr(sub(E1, E2)) :- expr(E1), expr(E2).

assoc(add(lit(X), lit(Y)), add(lit(X), lit(Y))).
assoc(sub(lit(X), lit(Y)), sub(lit(X), lit(Y))).
