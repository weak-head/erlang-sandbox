-module(listf).
-export([filter/2, reverse/1]).

filter([], _)    -> [];
filter([H|T], E) ->
    if H =< E -> [H|filter(T, E)];
       true   -> filter(T, E)
    end.

reverse(Lst) ->
    ReverseAcc = fun (F, Ls, Acc) ->
        case Ls of
            []    -> Acc;
            [H|T] -> F(F, T, [H|Acc])
        end
    end,
    ReverseAcc(ReverseAcc, Lst, []).