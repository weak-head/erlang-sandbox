-module(heartbeat).
-export([start/0, stop/0, status/0]).
-export([init/0, entry_point/0, main_loop/2]).

% >>>
%  c(heartbeat).
%  heartbeat:start().
%  exit(whereis(heartbeat_r), exit_sync).
% >>>

% -------------------------

start() ->
    init(),
    {ok}.

stop() ->
    {ok}.

status() ->
    {running}.

% -------------------------

init() ->
    RPID = spawn_link(heartbeat, entry_point, []),
    LPID = spawn_link(heartbeat, entry_point, []),
    register(heartbeat_r, RPID),
    register(heartbeat_l, LPID),
    heartbeat_r ! {attach, LPID},
    heartbeat_l ! {attach, RPID}.

entry_point() ->
    receive
        {attach, PID} ->
            main_loop(PID, false)
    end.

main_loop(PID, Waiting) ->
    receive
        {request, ping} ->
            PID ! {response, pong},
            main_loop(PID, Waiting);
        {response, pong} ->
            case Waiting of
                false ->
                    throw({unsynced_instance});
                true ->
                    io:format("~w~w~n", [self(), ' pong']),
                    main_loop(PID, false)
            end
    after
        1000 ->
            case Waiting of
                true ->
                    throw({dead_couple});
                false ->
                    PID ! {request, ping},
                    io:format("~w~w~n", [self(), ' ping']),
                    main_loop(PID, true)
            end
    end.