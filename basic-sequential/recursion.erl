-module(recursion).
-export([bump/1]).

% bumping list of integers.
bump([]) -> [];
bump([Head | Tail]) -> [Head + 1 | bump(Tail)].