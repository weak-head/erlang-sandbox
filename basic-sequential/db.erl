-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2, example/0]).

new() ->
    [].

destroy(_Db) ->
    {ok}.

write(Key, Element, Db) ->
    [{Key, Element}|Db].

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

example() ->
    Db  = new(),
    Db1 = write(francesco, london, Db),
    Db2 = write(lelle, stockholm, Db1),
    {ok, london} = read(francesco, Db2),
    Db3 = write(joern, stockholm, Db2),
    {error, instance} = read(ola, Db3),
    [joern, lelle] = match(stockholm, Db3),
    Db4 = delete(lelle, Db3),
    [joern] = match(stockholm, Db4).

