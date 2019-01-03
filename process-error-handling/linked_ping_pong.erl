-module(linked_ping_pong).
-export([start/0, stop/0]).
-export([start_ping/0, ping_pong_loop/1]).

start() ->
    Pid = spawn(linked_ping_pong, start_ping, []),
    register(ping_proc, Pid), ok.

stop() ->
    exit(whereis(ping_proc), kill), ok.

start_ping() ->
    Pid = spawn_link(linked_ping_pong, ping_pong_loop, [self()]),
    ping_pong_loop(Pid).

ping_pong_loop(CouplePid) ->
    process_flag(trap_exit, true),
    receive
        {request, ping, From} ->
            io:format("~s~w~n", ["ping from ", From]),
            From ! {response, pong, self()};
        {'EXIT', CouplePid, _Reason} ->
            io:format("~s~w~n", ["terminating pong ", CouplePid]), ok
        after 5000 ->
            CouplePid ! {request, ping, self()}
    end,
    ping_pong_loop(CouplePid).
