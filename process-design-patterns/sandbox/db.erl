-module(db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).

% --- Public API

start() ->
    register(db, spawn(db, init, [])).

stop() ->
    call(stop).

write(Key, Element) ->
    call({write, Key, Element}).

delete(Key) ->
    call({delete, Key}).

read(Key) ->
    call({read, Key}).

match(Element) ->
    call({match, Element}).


% --- Private generic API

init() ->
    loop([]).

loop(State) ->
    receive
        {request, From, Msg} ->
            {Reply, NewState} = handle_request(Msg, State),
            reply(From, Reply),
            loop(NewState);
        {stop, From} ->
            reply(From, ok)
    end.

call(Msg) ->
    db ! {request, self(), Msg},
    receive {reply, Response} -> Response end.

reply(To, Msg) ->
    To ! {reply, Msg}.


% --- Request handlers

handle_request({write, Key, Element}, State) -> ok.