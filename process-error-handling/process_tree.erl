-module(process_tree).
-export([build_tree/0]).
-export([supervisor_init/0, supervisor_loop/0, worker_loop/0]).
% ---

build_tree() ->
    process_flag(trap_exit, true),
    Pid = spawn_monitor(process_tree, supervisor_init, []),
    {ok, Pid}.

% ---

supervisor_init() ->
    process_flag(trap_exit, true),
    lists:foreach(fun(_) ->
                    spawn_link(process_tree, worker_loop, [])
                  end, lists:seq(1,3)),
    supervisor_loop().

supervisor_loop() ->
    receive
        {'EXIT', Pid, Reason} ->
            io:format("~s~w~s~w~n", ["resurecting ", Pid, " -> exit reason: ", Reason]),
            spawn_link(process_tree, worker_loop, [])
    end,
    supervisor_loop().

worker_loop() ->
    receive
        {request, print, Msg} ->
            io:format("~w~w~n", [self(), Msg]),
            worker_loop()
    after
        3000 ->
            {status, suspended}
    end.