-module(listf).
-export([filter/2, reverse/1, concatenate/1, flatten/1]).
-export([quick_sort/1, merge_sort/1]).

quick_sort([]) -> [];
quick_sort([H|T]) ->
    Le = quick_sort(filter(T, fun(E) -> H =< E end)),
    Gt = quick_sort(filter(T, fun(E) -> H >  E end)),
    flatten([Le, [H], Gt]).

merge_sort([]) -> [].

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
