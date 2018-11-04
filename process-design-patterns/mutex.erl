-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).
-export([example/0, worker/1]).

% Mutex FSM (finite state machine)

wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex ! {signal, self()}, ok.

start() ->
    register(mutex, spawn(?MODULE, init, [])).

stop() ->
    mutex ! terminate.

init() ->
    free().

free() ->
    receive
        {wait, Pid} ->
            Pid ! ok,
            busy(Pid);
        terminate ->
            terminate()
    end.

busy(Pid) ->
    receive
        {signal, Pid} ->
            free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after
        0 -> ok
    end.

% ----------
% -- Example
% ----------

example() ->
    start(),
    spawn_workers(20),
    receive after
        2000 -> ok
    end,
    stop().

spawn_workers(0) -> ok;
spawn_workers(N) ->
    spawn(?MODULE, worker, [N]),
    spawn_workers(N-1).

worker(Id) ->
    io:format("~w~s~n", [Id, [" -> obtaining lock"]]),
    wait(),
    io:format("~w~s~n", [Id, [" -> doing some stuff"]]),
    signal(),
    io:format("~w~s~n", [Id, [" -> released"]]).