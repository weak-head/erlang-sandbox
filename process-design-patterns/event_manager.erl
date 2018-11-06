-module(event_manager).
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, get_data/2, send_event/2]).
-export([init/1]).

start(Name, HandlerList) ->
    register(Name, spawn(?MODULE, init, [HandlerList])), ok.

init(HandlerList) ->
    loop(initialize(HandlerList)).

initialize([]) -> [];
initialize([{Handler, InitData}|Rest]) ->
    [{Handler, Handler:init(InitData)}|initialize(Rest)].

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

terminate([]) -> [];
terminate([{Handler, Data}|Rest]) ->
    [{Handler, Handler:terminate(Data)}|terminate(Rest)].

add_handler(Name, Handler, InitData) -> ok.

delete_handler(Name, Handler) -> ok.

get_data(Name, Handler) -> ok.

send_event(Name, Event) -> ok.

loop(State) -> ok.