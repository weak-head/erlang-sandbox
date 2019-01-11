-module(permut).
-export([perms/1]).

perms([]) ->
    [];
perms([X | Xs]) ->
   [ join(As, X, Bs) || Xps <- perms(Xs), {As, Bs} <- split(Xps) ].

split([]) ->
    {[], []};
split([X | Xs] = Xss) ->
    [ {[], Xss} | [ {[X | As], Bs} || {As, Bs} <- split(Xs)] ].

join(As, X, Bs) ->
    lists:append([As, [X], Bs]).