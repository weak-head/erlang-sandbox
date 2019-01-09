-module(db_server).
-export([start/0, stop/0]).
-export([write/2, read/1, delete/1]).
-export([init/0, loop/1]).
-export([upgrade/1]).
-vsn(1.0).

start() ->
    Pid = spawn_link(db_server, init, []),
    register(db_server, Pid).

stop() ->
    db_server ! {stop, self()}.

read(Key) ->
    db_server ! {read, self(), Key},
    receive Data -> Data end.

write(Key, Data) ->
    db_server ! {write, self(), Key, Data}.

delete(Key) ->
    db_server ! {delete, self(), Key}.

upgrade(Data) ->
    db_server ! {upgrade, Data}.

init() ->
    loop(db:new()).

loop(Db) ->
    receive
        {stop, _Pid} ->
            db:destroy(Db);
        {read, Pid, Key} ->
            Data = db:read(Key, Db),
            Pid ! Data,
            loop(Db);
        {write, _Pid, Key, Data} ->
            loop(db:write(Key, Data, Db));
        {delete, _Pid, Key} ->
            loop(db:delete(Key, Db));
        {upgrade, Data} ->
            NewDb = db:convert(Data, Db),
            db_server:loop(NewDb)
    end.
