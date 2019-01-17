% Database API (ETS + Dets)
% ETS  -> Erlang Term Storage
% Dets -> Disk Erlang Term Storage

-module(usr_db).
-include("usr.hrl").
-export([create_tables/1, close_tables/0]).

create_tables(FileName) ->
    % named set that is based on a hashtable and stored in ram
    ets:new(usrRam, [named_table, {keypos, #usr.msisdn}]),
    ets:new(usrIndex, [named_table]),
    % named set that is persisted in file and based on a hashtable
    dets:open_file(usrDisk, [{file, FileName}, {keypos, #usr.msisdn}]).

close_tables() ->
    ets:delete(usrRam),
    ets:delete(usrIndex),
    dets:close(usrDisk).