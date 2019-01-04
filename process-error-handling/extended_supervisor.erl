-module(extended_supervisor).
-export([supervise/2, init/1]).
% -export([start_supervisor/2, attach_supervisor/2]).
% -export([spawn_worker/1, spawn_workers/1]).
% -export([main_loop/1]).
% -export([respawn/2, respawn_or_drop/2, cleanup/1]).

% >> >> >>

% c(extended_supervisor).
% c(add_two).
% extended_supervisor:supervise(addtwo, [{add_two, start, [], permanent}]).
% add_two:request(10).
% add_two:request(abc).
% add_two:request(12).

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

spawn_worker({_Pid, {Module, Func, Args}, {Failures, Type}}) ->
    case (catch(apply(Module, Func, Args))) of
        {ok, Pid} ->
            {Pid, {Module, Func, Args}, {0, Type}};
        _ ->
            {none, {Module, Func, Args}, {Failures + 1, Type}}
    end.

spawn_workers([]) -> [];
spawn_workers([{Module, Func, Args, Type} | Rest]) ->
    [spawn_worker({none, {Module, Func, Args}, {0, Type}}) | spawn_workers(Rest)].

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
    {value, {_OldPid, {M, F, A}, {Failures, Type}}} = lists:keysearch(Pid, 1, WorkerDefSpecs),
    case Type of
        permanent ->
            [spawn_worker({_OldPid, {M, F, A}, {Failures, Type}}) | lists:keydelete(Pid, 1, WorkerDefSpecs)];
        transient ->
            lists:keydelete(Pid, 1, WorkerDefSpecs)
    end.

respawn(Pid, WorkerDefSpecs) ->
    {value, WorkerSpec} = lists:keysearch(Pid, 1, WorkerDefSpecs),
    [spawn_worker(WorkerSpec) | lists:keydelete(Pid, 1, WorkerDefSpecs)].

cleanup(WorkerDefSpecs) ->
    % here we can handle workers that are keep failing...
    lists:filter(fun({_, _, {Failures, _}}) -> Failures < 3 end, WorkerDefSpecs).