-module(conditional_evaluation).
-import(math, [sqrt/1, pi/0]).
-export([area/1]).

% Conditional 1 -> Pattern matching.
% get an area of the surface.
area({triangle, A, B, C}) ->
    S = (A + B + C) / 2,
    sqrt(S*(S-A)*(S-B)*(S-C));
area({rectangle, A, B}) -> A * B;
area({circle, R}) -> pi() * R * R;
area({square, A}) -> A * A;
area(_Other) -> {error, invalid_object}.