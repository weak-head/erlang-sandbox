-module(add_two).
-export([start/0, loop/0, request/1]).

start() ->
    process_flag(trap_exit, true),
    Pid = spawn_link(add_two, loop, []),
    register(add_two, Pid),
    {ok, Pid}.

loop() ->
    receive
        {request, Pid, Request} ->
            Pid ! {response, Request + 2}
    end,
    loop().

request(Request) ->
    add_two ! {request, self(), Request},
    receive
        {'EXIT', _Pid, Reason} -> {error, Reason};
        {response, Result} -> Result
        after 1000 -> timeout
    end.