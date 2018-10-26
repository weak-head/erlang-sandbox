-module(echo).
-export([go/0, loop/0]).
-export([go_r/0, loop_r/0]).
-export([start/0, stop/0, print/1, message_loop/0]).

go() ->
    Pid = spawn(echo, loop, []),
    Pid ! {self(), hello},
    receive
        {Pid, Msg} ->
            io:format("~w~n", [Msg])
    end,
    Pid ! stop.

loop() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop();
        stop ->
            true
    end.

% ---------------

go_r() ->
    register(echo, spawn(echo, loop_r, [])),
    echo ! {self(), hello},
    receive
        {_Pid, Msg} ->
            io:format("~w~n", [Msg])
    end,
    echo ! stop.

loop_r() ->
    receive
        {From, Msg} ->
            From ! {self(), Msg},
            loop_r();
        stop ->
            true
    end.

% ----------------

start() ->
    register(echo, spawn(?MODULE, message_loop, [])), ok.

message_loop() ->
    receive
        stop ->
            ok;
        {print, Msg} ->
            io:format("~w~n", [Msg]),
            message_loop()
    end.

print(Msg) ->
    case whereis(echo) of
        undefined ->
            {error, server_not_running};
        Pid when is_pid(Pid) ->
            Pid ! {print, Msg}, ok
    end.

stop() ->
    case whereis(echo) of
        undefined ->
            {error, server_not_running};
        Pid when is_pid(Pid) ->
            Pid ! stop, ok
    end.
