-module(recursion).
-export([ bump/1
        , average/1
        , sum/1
        , len/1
        , better_average/1]).

% -------------------------
% bumping list of integers.
bump([]) -> [];
bump([Head | Tail]) -> [Head + 1 | bump(Tail)].

% -------------------------
% a few examples of getting an average value of an integer list.
average(Lst) -> sum(Lst) / len(Lst).

sum([]) -> 0;
sum([H|T]) -> H + sum(T).

len([]) -> 0;
len([_|T]) -> 1 + len(T).

better_average([]) -> {error, "Empty list"};
better_average(Lst) ->
    {Sum, Len} = foldl_average(Lst, 0, 0),
    Sum / Len.

foldl_average([], S, L)    -> {S, L};
foldl_average([H|T], S, L) -> foldl_average(T, S+H, L+1).