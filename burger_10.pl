% AI Sous-Chef – Burger Construction Agent

% Welcome to the AI Sous-Chef! This agent uses Situation Calculus and Iterative Deepening
% Search (IDS) to figure out the perfect order to stack a burger.
% It reasons about actions (stacking ingredients) and their effects on the world (the burger).

% KNOWLEDGE BASE SELECTION
% We can choose which set of rules (Knowledge Base) to use.
% Uncomment the one you want to test.

%:- include('KB1.pl').
 :- include('KB2.pl').  % Currently using Knowledge Base 2


% INGREDIENT LIST
% These are the building blocks of our burger. The agent knows about these specific items.

ingredients([bottom_bun, patty, lettuce, cheese, pickles, tomato, top_bun]).


% FLUENTS (Dynamic Properties)
% Fluents are properties of the world that can change as we perform actions.
% Since the state of the burger changes with every 'stack' action, we declare these as dynamic.

% stacked(X, S) -> Means ingredient X is already on the burger in situation S.
% onTop(X, S)   -> Means ingredient X is currently the topmost item in situation S.

:- dynamic(stacked/2).
:- dynamic(onTop/2).


% SUCCESSOR STATE AXIOMS
% These rules define how the world changes (or stays the same) after an action.
% They solve the "Frame Problem" by explicitly stating what holds true in the next situation.

% Axiom for 'stacked':
% An ingredient X is stacked in the NEW situation (result of stacking A) if:
% 1. X is the very item we just stacked (X = A), OR
% 2. X was already stacked in the previous situation S, and it's not the one we just moved.
stacked(X, result(stack(A), S)) :-
    X = A ;
    (stacked(X, S), X \= A).


% Axiom for 'onTop':
% 1. If we stack X, then X is definitely on top in the new situation.
onTop(X, result(stack(X), _)).

% 2. Standard frame axiom structure: properties tend to persist unless changed.
%    This rule says X remains 'onTop' if it was 'onTop' before and isn't the thing being stacked (A).
onTop(X, result(stack(A), S)) :-
    X \= A,
    onTop(X, S).


% ACTION PRECONDITIONS
% Before we perform an action, we must check if it's actually possible.
% The predicate poss(stack(X), S) defines when it is valid to stack ingredient X in situation S.

poss(stack(X), S) :-
    ingredients(L),
    member(X, L),                       % 1. X must be a known ingredient from our list.
    \+ stacked(X, S),                   % 2. We can't stack the same item twice! X must not be stacked yet.
    forall(above(X, Y), stacked(Y, S)). % 3. PREREQUISITE CHECK:
                                        %    If the Knowledge Base says X must be above Y (above(X, Y)),
                                        %    then Y must ALREADY be in the stack.
                                        %    We check this for ALL such Ys.


% HELPER: SITUATION TO LIST
% Situations in calculus are nested terms like result(stack(a), result(stack(b), s0)).
% This is hard to read and process for order.
% makeList/2 converts that nested structure into a clean Prolog list: [b, a].

makeList(s0, []).  % The initial situation s0 corresponds to an empty list.

makeList(result(stack(X), S), L2) :-
    makeList(S, L1),        % First, convert the previous situation S to a list L1.
    append(L1, [X], L2).    % Then, add the new ingredient X to the end.


% HELPER: FIND INDEX
% A simple utility to find where an item sits in a list (0-based index).
% We use this to verify the physical order of ingredients.

indexOf(X, [X|_], 0).       % Found it at the head! Index is 0.
indexOf(X, [_|T], N) :-     % Not at the head? Look in the tail.
    indexOf(X, T, N1),
    N is N1 + 1.            % Add 1 to the result from the tail.


% VERIFY ORDER IN SITUATION
% Checks if ingredient X is physically higher than ingredient Y in the current stack S.

isAboveInSituation(X, Y, S) :-
    makeList(S, L),         % Get the linear list of ingredients.
    indexOf(X, L, IX),      % Find position of X.
    indexOf(Y, L, IY),      % Find position of Y.
    IX > IY.                % X is above Y if X's index is greater (added later).


% GOAL STATE: IS THE BURGER READY?
% This predicate defines our success condition. The burger is ready when:

burgerReady(S) :-
    ingredients(L),
    % Condition 1: Completeness
    % Every ingredient in our official list must be present in the stack.
    forall(member(X, L), stacked(X, S)),

    % Condition 2: Correctness
    % Every constraint in our Knowledge Base must be satisfied.
    % If the KB says "above(X, Y)", then X must physically be above Y in our stack.
    forall(above(X, Y), isAboveInSituation(X, Y, S)).


% TRANSITION RELATION
% This connects the "possibility" of an action to its execution.
% If it is possible to stack X in S, then we can move to the result state.

do(stack(X), S, result(stack(X), S)) :-
    poss(stack(X), S).


% DEPTH-LIMITED SEARCH (DFS with Limit)
% This is the core search engine. It looks for a solution but gives up if it goes too deep.
% This prevents it from getting lost in infinite loops or extremely long paths initially.

% Base Case: Success!
search(S, _, S) :-
    burgerReady(S).   % We found a state S where the burger is complete and valid.

% Recursive Step: Keep searching
search(S, Depth, Sol) :-
    Depth > 0,              % Only proceed if we have depth allowance left.
    do(stack(X), S, S2),    % Try to perform a valid action (stack X) to get state S2.
    D1 is Depth - 1,        % Decrease our remaining depth budget.
    search(S2, D1, Sol).    % Recursively search from the new state S2.


% ITERATIVE DEEPENING SEARCH (IDS)
% IDS combines the best of Breadth-First and Depth-First search.
% It tries Depth 1, then Depth 2, then Depth 3... ensuring we find the shortest solution first.

solve(Solution) :-
    between(1, 20, Depth),          % Generate depth limits from 1 to 20.
    search(s0, Depth, Solution),    % Try to find a solution within that depth.
    !.                              % Cut (!) to stop searching once the first solution is found.
