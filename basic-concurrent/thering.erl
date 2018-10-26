-module(thering).
-export([start/1, start_proc/2, bench/1]).

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