-module(supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
    register(Name, spawn_link(supervisor, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
    process_flag(trap_exit, true),
    loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M, F, A} | Rest]) ->
    case (catch apply(M, F, A)) of
        {ok, Pid} ->
            [{Pid, {M, F, A}} | start_children(Rest)];
        _ ->
            start_children(Rest)
    end.

restart_child(Pid, ChildList) ->
    {_OldPid, {M, F, A}} = lists:keysearch(Pid, 1, ChildList),
    {ok, NewPid} = apply(M, F, A),
    [{NewPid, {M, F, A}} | lists:keydelete(Pid, 1, ChildList)].

loop(ChildList) ->
    receive
        {'EXIT', Pid, _Reason} ->
            loop(restart_child(Pid, ChildList));
        {stop, From} ->
            From ! {reply, terminate(ChildList)}
    end.

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Resp} -> Resp end.

terminate([]) -> ok;
terminate([{Pid, _Spec} | Rest]) ->
    exit(Pid, kill),
    terminate(Rest).