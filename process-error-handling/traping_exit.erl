-module(traping_exit).
-export([start/0, request/1, loop/0]).

start() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(traping_exit, loop, []),
    register(traping_exit, Pid),
    {ok, Pid}.

request(Int) ->
    traping_exit ! {request, self(), Int},
    receive
        {result, Result} -> Result;
        {'EXIT', _Pid, Reason} ->
            start(),
            {error, Reason}
        after 1000 -> timeout
    end.

loop() ->
    receive
        {request, Pid, Int} ->
            Pid ! {result, Int + 1}
    end,
    loop().