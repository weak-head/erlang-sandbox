-module(listf).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).
-export([quick_sort/1, merge_sort/1]).

quick_sort([]) -> [];
quick_sort([H|T]) ->
    Le = quick_sort(filter(T, fun(E) -> H =< E end)),
    Gt = quick_sort(filter(T, fun(E) -> H >  E end)),
    flatten([Le, [H], Gt]).

merge_sort([]) -> [];
merge_sort([A]) -> [A];
merge_sort(Ls) ->
    {Fst, Snd} = split(Ls),
    merge(merge_sort(Fst), merge_sort(Snd)).

split([])  -> {[], []};
split([A]) -> {[A], []};
split(Ls) ->
    Ln = length(Ls),
    split_at_acc(Ls, (Ln div 2), []).

split_at_acc([], _, Acc)   -> {Acc, []};
split_at_acc(Tail, 0, Acc) -> {Acc, Tail};
split_at_acc([H|T], N, Acc) ->
    split_at_acc(T, (N-1), [H|Acc]).

merge([], [])   -> [];
merge(As, [])   -> As;
merge([], Bs)   -> Bs;
merge([A|Ta], [B|Tb]) ->
    case A > B of
        true  -> [B|merge([A|Ta], Tb)];
        false -> [A|merge(Ta, [B|Tb])]
    end.

% ---

filter([], _)            -> [];
filter([H|T], Predicate) ->
    case Predicate(H) of
        true  -> [H|filter(T, Predicate)];
        false -> filter(T, Predicate)
    end.

reverse(Lst) ->
    ReverseAcc = fun (F, Ls, Acc) ->
        case Ls of
            []    -> Acc;
            [H|T] -> F(F, T, [H|Acc])
        end
    end,
    ReverseAcc(ReverseAcc, Lst, []).

% concatenate([[1,2,3],[],[4,five]])
concatenate([])    -> [];
concatenate([H|T]) ->
    Flat = fun (F, Lst) ->
        case Lst of
            [] -> concatenate(T);
            [Hs|Ts] -> [Hs|F(F, Ts)]
        end
    end,
    Flat(Flat, H).

%
% > listf:flatten([[[1,2,3],[4],[[5],[6],[7],[[[[[8]]]]]]],[9],[10]]).
%  -> [1,2,3,4,5,6,7,8,9,10]
%
% > listf:flatten([1,[2],[[3,4],[5,[[6],[[[7]]]]]],8,[9,10],[[[11,12]]]]).
%  -> [1,2,3,4,5,6,7,8,9,10,11,12]
%
flatten([])      -> [];
flatten([[H|T]]) -> flatten([H|T]);
flatten([H|T])   -> concatenate([flatten(H), flatten(T)]);
flatten(L)       -> [L].
