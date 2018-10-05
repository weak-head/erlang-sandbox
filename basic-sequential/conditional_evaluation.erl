-module(conditional_evaluation).
-import(math, [sqrt/1, pi/0]).
-export([area/1, listlen/1, index/1, index/2, area_case/1]).

% -----
% Conditional 1 -> Pattern matching.
% get an area of the surface.
area({triangle, A, B, C}) ->
    S = (A + B + C) / 2,
    sqrt(S*(S-A)*(S-B)*(S-C));
area({rectangle, A, B}) -> A * B;
area({circle, R}) -> pi() * R * R;
area({square, A}) -> A * A;
area(_Other) -> {error, invalid_object}.


% -----
% Conditional 2 -> Case expression.
listlen(L) ->
    case L of
        []     -> 0;
        [_|Xs] -> 1 + listlen(Xs)
    end.

index(X,Y) ->
    index({X,Y}).

index(Z) ->
    case Z of
        {0,[X|_]}           -> X;
        {N,[_|Xs]} when N>0 -> index({N-1,Xs})
    end.

area_case(Z) ->
    case Z of
        {triangle, A, B, C} ->
            S = (A+B+C)/2,
            sqrt(S*(S-A)*(S-B)*(S-C));
        {circle, R} -> pi() * R * R()
    end.