% Database API (ETS + Dets)
% ETS  -> Erlang Term Storage
% Dets -> Disk Erlang Term Storage

-module(usr_db).
-include("usr.hrl").
-export([create_tables/1, close_tables/0]).
-export([add_usr/1, update_usr/1]).
-export([lookup_id/1, lookup_msisdn/1]).

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

add_usr(#usr{msisdn=PhoneNo, id=CustId} = User) ->
    ets:insert(usrIndex, {CustId, PhoneNo}),
    update_usr(User).

update_usr(User) ->
    % enderlined sets would be updated based on the #usr.msisdn key
    ets:insert(usrRam, User),
    dets:insert(usrDisk, User),
    ok.

% gets user based on id
lookup_id(CustId) ->
    case get_index(CustId) of
        {ok, PhoneNo}     -> lookup_msisdn(PhoneNo);
        {error, instance} -> {error, instance}
    end.

% gets user based on phone number
lookup_msisdn(PhoneNo) ->
    case ets:lookup(usrRam, PhoneNo) of
        [Usr] -> {ok, Usr};
        []    -> {error, instance}
    end.

% user id to phone number
get_index(CustId) ->
    case ets:lookup(usrIndex, CustId) of
        [{CustId, PhoneNo}] -> {ok, PhoneNo};
        []                  -> {error, instance}
    end.