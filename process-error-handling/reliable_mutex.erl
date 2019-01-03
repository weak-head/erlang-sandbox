-module(reliable_mutex).
-export([start/0, stop/0]).
-export([wait/0, release/0]).
-export([init/0, free/0, busy/2]).

% >> Mutex

start() ->
    Pid = spawn_link(?MODULE, init, []),
    register(reliable_mutex, Pid), ok.

stop() ->
    reliable_mutex ! terminate,
    receive ok -> ok end.

wait() ->
    reliable_mutex ! {wait, self()},
    receive ok -> ok end.

release() ->
    reliable_mutex ! {release, self()}, ok.

init() ->
    process_flag(trap_exit, true),
    free().

free() ->
    receive
        {wait, From} ->
            Reference = monitor(process, From),
            io:format("obtained~n"),
            From ! ok,
            busy(From, Reference)
    end.

busy(Pid, Reference) ->
    receive
        {wait, Pid} ->
            Pid ! ok,
            busy(Pid, Reference);
        {'DOWN', Reference, process, Pid, _Reason} ->
            io:format("The process ~w has ended, releasing mutex~n", [Pid]),
            free();
        {release, Pid} ->
            free()
    end.