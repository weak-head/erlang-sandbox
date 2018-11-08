-module(db_usage).
-import(db, [start/0, stop/0, write/2, delete/1, read/1, match/1]).
-export([run/0]).

run() ->
    start(),

    write(first, 42),
    write(second, 77),
    write(third, 42),
    write(fourth, 88),

    {ok, E1} = read(third),
    io:format("~w~n", [E1]),

    Match = match(42),
    io:format("~p~n", [Match]),

    delete(third),

    {error, instance} = read(third),
    [first] = match(42),

    stop().

