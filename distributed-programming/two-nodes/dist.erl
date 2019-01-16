-module(dist).
-export([start_server/0]).

start_server() ->
    register(server, self()),
    loop().

loop() ->
    receive
        {From, Msg} ->
            From ! {resp, Msg}
    end.