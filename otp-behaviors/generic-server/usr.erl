%% gen_server

-module(usr).
-behavior(gen_server).

-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([add_usr/3, delete_usr/1, set_service/3, set_status/2, delete_disabled/0, lookup_id/1]).

-include("usr.hrl").

%% ------------------------------------------------------------------
%% Operation and Maintenance API

start_link() ->
    start_link("usrDb").

start_link(FileName) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, FileName, []).

stop() ->
    gen_server:cast(?MODULE, stop).

%% ------------------------------------------------------------------
%% Service API

add_usr(PhoneNum, CustId, Plan) when Plan == prepay; Plan == postpay ->
    gen_server:call(?MODULE, {add_usr, PhoneNum, CustId, Plan}).

delete_usr(CustId) ->
    gen_server:call(?MODULE, {delete_usr, CustId}).

set_service(CustId, Service, Flag) when Flag == true; Flag == false ->
    gen_server:call(?MODULE, {set_service, CustId, Service, Flag}).

set_status(CustId, Status) when Status == enabled; Status == disabled ->
    gen_server:call(?MODULE, {set_status, CustId, Status}).

delete_disabled() ->
    gen_server:call(?MODULE, delete_disabled).

lookup_id(CustId) ->
    usr_db:lookup_id(CustId).

%% ------------------------------------------------------------------
%% gen_server behavior API callbacks

init(FileName) ->
    usr_db:create_tables(FileName),
    usr_db:restore_database(),
    {ok, null}.

terminate(_Reason, _LoopData) ->
    usr_db:close_tables().

handle_cast(stop, LoopData) ->
    {stop, normal, LoopData}.

% handle add_usr
handle_call({add_usr, PhoneNum, CustId, Plan}, _From, LoopData) ->
    Reply = usr_db:add_usr(#usr{msisdn = PhoneNum,
                                id = CustId,
                                plan = Plan}),
    {reply, Reply, LoopData};

% handle delete_usr
handle_call({delete_usr, CustId}, _From, LoopData) ->
    Reply = usr_db:delete_usr(CustId),
    {reply, Reply, LoopData};

% handle set_service
handle_call({set_service, CustId, Service, Flag}, _From, LoopData) ->
    Reply = case usr_db:lookup_id(CustId) of
        {ok, Usr} ->
            Services = lists:delete(Service, Usr#usr.services),
            NewServices = case Flag of
                true  -> [Service | Services];
                false -> Services
            end,
            usr_db:update_usr(Usr#usr{services = NewServices});
        {error, instance} ->
            {error, instance}
    end,
    {reply, Reply, LoopData}.