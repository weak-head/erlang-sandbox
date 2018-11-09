-module(client_server).
% -import(lists, [keydelete/3]).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0]).

start() ->
    register(client_server, spawn(client_server, init, [])).

stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

% ----

init() ->
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

call(Msg) ->
    client_server ! {request, self(), Msg},
    receive
        {reply, Reply} -> Reply
    end.

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            {Reply, NewFrequencies} = deallocate(Frequencies, Pid, Freq),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Pid, Freq) ->
    case lists:keyfind(Freq, 1, Allocated) of
        {Freq, Pid} ->
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {ok, {[Freq|Free], NewAllocated}};
        false ->
            {{error, no_match}, {Free, Allocated}}
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

get_frequencies() -> [10,11,12,13,14,15].