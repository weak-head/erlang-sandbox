-module(db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([init/0]).

% --- Public API

start() ->
    register(db, spawn(db, init, [])).

stop() ->
    db ! {stop, self()},
    receive {reply, Reply} -> Reply end.

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

handle_request({write, Key, Element}, State) ->
    {ok, [{Key, Element}|State]};

handle_request({delete, Key}, State) ->
    {ok, delete(Key, State)};

handle_request({read, Key}, State) ->
    {read(Key, State), State};

handle_request({match, Element}, State) ->
    {match(Element, State), State}.

% --- Helpers

delete(_, []) ->
    [];
delete(K, [{K,_}|Db]) ->
    delete(K, Db);
delete(Key, [{K,V}|Db]) ->
    [{K,V}|delete(Key,Db)].

read(_, []) ->
    {error, instance};
read(_K, [{_K,V}|_]) ->
    {ok, V};
read(Key, [{_K,_V}|Db]) ->
    read(Key, Db).

match(_, []) ->
    [];
match(E, [{K,E}|Db]) ->
    [K|match(E,Db)];
match(E, [{_K,_R}|Db]) ->
    match(E, Db).