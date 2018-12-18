-module(simple_link).
-export([start/0, request/1, loop/0]).

start() ->
    register(simple_link, spawn_link(simple_link, loop, [])).

request(Int) ->
    simple_link ! {request, self(), Int},
    receive
        {result, Result} -> Result
        after 1000 -> timeout
    end.

loop() ->
    receive
        {request, Pid, Msg} ->
            Pid ! {result, Msg + 1}
    end,
    loop().