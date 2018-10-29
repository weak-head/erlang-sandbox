-module(thering).
-export([start/1, start_proc/2, bench/1, bench/0]).
-export([start_ring/3, make_ring/2, main_loop/1, ring_chain/1]).

bench() ->
    {bench(100000), bench(1000000), bench(10000000)}.

% bench(100000).
% bench(1000000).
% bench(10000000).
bench(Num) ->
    timer:tc(?MODULE, start, [Num]).

start(Num) ->
    start_proc(Num, self()).

start_proc(0, Pid) ->
    Pid ! ok;

start_proc(Num, Pid) ->
    Pid2 = spawn(?MODULE, start_proc, [Num-1, Pid]),
    Pid2 ! ok,
    receive ok -> ok end.

% ------------
% process ring
% ------------

% A -> B -> C -> D
% |              |
% |  <-   <-   <-|

start_ring(M, N, Message) ->
    HeadPid = make_ring(N, self()),
    HeadPid ! {send_msg, M, Message},
    main_loop(HeadPid),
    HeadPid ! exit.

main_loop(HeadPid) ->
     receive
        {send_msg, 0, Msg} ->
            io:format("~w~n", [Msg]);
        {send_msg, Num, Msg} ->
            HeadPid ! {send_msg, Num-1, Msg},
            main_loop(HeadPid)
    end.

make_ring(0, Pid) -> Pid;
make_ring(N, Pid) ->
    Pid2 = spawn(thering, ring_chain, [Pid]),
    make_ring(N-1, Pid2).

ring_chain(Next) ->
    receive
        {send_msg, Num, Msg} ->
            Next ! {send_msg, Num, Msg},
            ring_chain(Next);
        exit ->
            true
    end.