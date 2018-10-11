-module(recursion).
-export([ bump/1
        , average/1
        , sum/1
        , len/1
        , better_average/1
        , even/1
        , member/2
        , sum_t/1
        , bump_t/1]).

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

% --------------------------
% tail-recursion
sum_t([]) -> 0;
sum_t(L) -> sum_acc(L,0).

sum_acc([], N) -> N;
sum_acc([H|T], N) -> sum_acc(T, N+H).

bump_t(L) -> reverse_acc(bump_acc(L, []), []).

bump_acc([], A)    -> A;
bump_acc([H|T], A) -> bump_acc(T, [H+1 | A]).

reverse_acc([], A)    -> A;
reverse_acc([H|T], A) -> reverse_acc(T, [H | A]).