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

init(_) -> ok.
terminate(_, _) -> ok.
handle_call(_, _, _) -> ok.
handle_cast(_, _) -> ok.