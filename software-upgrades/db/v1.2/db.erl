-module(db).
-export([new/0, destroy/1]).
-export([write/3, delete/2, read/2]).
-export([convert/2]).
-vsn(1.2).

new() ->
    gb_trees:empty().

destroy(_Db) ->
    ok.

write(Key, Data, Db) ->
    gb_trees:insert(Key, Data, Db).

delete(Key, Db) ->
    gb_trees:delete(Key, Db).

read(Key, Db) ->
    case gb_trees:lookup(Key, Db) of
        none          -> {error, instance};
        {value, Data} -> {ok, Data}
    end.

convert(dict, Dict) ->
    from_dict(dict:fetch_keys(Dict), Dict, new());
convert(_, Data) ->
    Data.

from_dict([Key | Rest], Dict, Db) ->
    Data      = dict:fetch(Key, Dict),
    UpdatedDb = write(Key, Data, Db),
    from_dict(Rest, Dict, UpdatedDb);
from_dict([], _Dict, Db) ->
    Db.