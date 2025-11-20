% ---------------------------------------------------------
% AI Sous-Chef – Burger Construction Agent
% Using Situation Calculus + Successor State Axioms + IDS
% ---------------------------------------------------------

%:- include('KB1.pl').
% To test KB2:
 :- include('KB2.pl').


% ---------------------------------------------------------
% INGREDIENT LIST
% ---------------------------------------------------------

ingredients([bottom_bun, patty, lettuce, cheese, pickles, tomato, top_bun]).


% ---------------------------------------------------------
% FLUENTS (Dynamic - change with situation)
% ---------------------------------------------------------

% stacked(X,S): ingredient X has been stacked in situation S.
% onTop(X,S): X is the topmost ingredient in situation S.

:- dynamic(stacked/2).
:- dynamic(onTop/2).


% ---------------------------------------------------------
% SUCCESSOR STATE AXIOM FOR stacked/2
% ---------------------------------------------------------

stacked(X, result(stack(A), S)) :-
    X = A ;
    (stacked(X, S), X \= A).


% ---------------------------------------------------------
% SUCCESSOR STATE AXIOM FOR onTop/2
% ---------------------------------------------------------

onTop(X, result(stack(X), _)).   % If we stack X, X becomes top.

onTop(X, result(stack(A), S)) :- % Otherwise X stays top if it was top before
    X \= A,
    onTop(X, S).


% ---------------------------------------------------------
% ACTION PRECONDITIONS
% ---------------------------------------------------------

% You can stack X if:
%   1) X is a valid ingredient
%   2) X has not been stacked yet
%   3) All Y such that above(X,Y) must already be stacked

poss(stack(X), S) :-
    ingredients(L),
    member(X, L),
    \+ stacked(X, S),
    forall(above(X, Y), stacked(Y, S)).


% ---------------------------------------------------------
% CONVERT A SITUATION INTO A LIST OF INGREDIENTS
% ---------------------------------------------------------

makeList(s0, []).

makeList(result(stack(X), S), L2) :-
    makeList(S, L1),
    append(L1, [X], L2).


% ---------------------------------------------------------
% FIND INDEX OF ELEMENT
% ---------------------------------------------------------

indexOf(X, [X|_], 0).
indexOf(X, [_|T], N) :-
    indexOf(X, T, N1),
    N is N1 + 1.


% ---------------------------------------------------------
% CHECK IF X IS ABOVE Y IN THE CURRENT SITUATION
% ---------------------------------------------------------

isAboveInSituation(X, Y, S) :-
    makeList(S, L),
    indexOf(X, L, IX),
    indexOf(Y, L, IY),
    IX > IY.


% ---------------------------------------------------------
% CHECK IF BURGER IS COMPLETE AND VALID
% ---------------------------------------------------------

burgerReady(S) :-
    ingredients(L),
    forall(member(X, L), stacked(X, S)),              % all ingredients present
    forall(above(X, Y), isAboveInSituation(X, Y, S)). % all constraints satisfied


% ---------------------------------------------------------
% TRANSITION RELATION
% ---------------------------------------------------------

do(stack(X), S, result(stack(X), S)) :-
    poss(stack(X), S).


% ---------------------------------------------------------
% DEPTH-LIMITED SEARCH
% ---------------------------------------------------------

search(S, _, S) :-
    burgerReady(S).   % goal reached

search(S, Depth, Sol) :-
    Depth > 0,
    do(stack(X), S, S2),  % generate next situation
    D1 is Depth - 1,
    search(S2, D1, Sol).


% ---------------------------------------------------------
% ITERATIVE DEEPENING SEARCH (IDS)
% ---------------------------------------------------------

solve(Solution) :-
    between(1, 20, Depth),  % try increasing depth
    search(s0, Depth, Solution),
    !. % stop at first found solution
