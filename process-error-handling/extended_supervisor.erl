-module(extended_supervisor).
-export([supervise/2, init/1]).
% -export([start_supervisor/2, attach_supervisor/2]).
% -export([spawn_worker/1, spawn_workers/1]).
% -export([main_loop/1]).
% -export([respawn/2, respawn_or_drop/2, cleanup/1]).

supervise(Name, WorkerSpecs) ->
    case (whereis(Name)) of
        undefined ->
            start_supervisor(Name, WorkerSpecs);
        _ ->
            attach_supervisor(Name, WorkerSpecs)
    end.

% start a new supervisor for the worker specs
start_supervisor(Name, WorkerSpecs) ->
    Pid = spawn_link(?MODULE, init, [WorkerSpecs]),
    register(Name, Pid), ok.

% attach worker specs to the existing supervisor
attach_supervisor(Name, WorkerSpecs) ->
    Name ! {attach, WorkerSpecs}, ok.

init(WorkerSpecs) ->
    process_flag(trap_exit, true),
    main_loop(spawn_workers(WorkerSpecs)).

spawn_worker({{Module, Func, Args}, {_Pid, Failures, Type}}) ->
    case (catch(apply(Module, Func, Args))) of
        {ok, Pid} ->
            {{Module, Func, Args}, {Pid, 0, Type}};
        _ ->
            {{Module, Func, Args}, {none, Failures + 1, Type}}
    end.

spawn_workers([]) -> [];
spawn_workers([{Module, Func, Args, Type} | Rest]) ->
    [spawn_worker({{Module, Func, Args}, {none, 0, Type}}) | spawn_workers(Rest)].

main_loop(WorkerDefSpecs) ->
    receive
        {'EXIT', Pid, normal} ->
            main_loop(respawn_or_drop(Pid, WorkerDefSpecs));

        {'EXIT', Pid, _Other} ->
            main_loop(respawn(Pid, WorkerDefSpecs));

        {attach, WorkerSpecs} ->
            main_loop(WorkerDefSpecs ++ spawn_workers(WorkerSpecs))

    after
        3000 ->
            main_loop(cleanup(WorkerDefSpecs))
    end.

respawn_or_drop(Pid, WorkerDefSpecs) ->
    WorkerDefSpecs.

respawn(Pid, WorkerDefSpecs) ->
    WorkerDefSpecs.

cleanup(WorkerDefSpecs) ->
    WorkerDefSpecs.