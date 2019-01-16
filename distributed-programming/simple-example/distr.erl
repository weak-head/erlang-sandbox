-module(distr).
-export([dt/1]).

dt(From) ->
    io:format('out -> ~w~n', [From]),
    From ! {resp, self(), node()}.