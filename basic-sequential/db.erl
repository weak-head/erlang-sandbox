-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2, example/0]).

new() ->
   throw(not_implemented).

destroy(Db) ->
   throw(not_implemented).

write(Key, Element, Db) ->
   throw(not_implemented).

delete(Key, Db) ->
   throw(not_implemented).

read(Key,Db) ->
   throw(not_implemented).

match(Element, Db) ->
   throw(not_implemented).


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

