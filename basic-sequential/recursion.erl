-module(recursion).
-export([ bump/1
        , average/1
        , sum/1
        , len/1
        , better_average/1
        , even/1
        , member/2]).

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

% -------------------------
% we are not using HOF here,
% though this is a good place for it
% anyways here is our `even` filter
even([]) -> [];
even([H|T]) when H rem 2 == 0 -> [H|even(T)];
even([_|T]) -> even(T).

member(_, [])    -> false;
member(H, [H|_]) -> true;
member(H, [_|T]) -> member(H, T).