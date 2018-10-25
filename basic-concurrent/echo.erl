-module(echo).
-export([go/0, loop/0]).
-export([go_r/0, loop_r/0]).

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

