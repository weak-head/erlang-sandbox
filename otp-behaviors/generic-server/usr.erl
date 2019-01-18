%% gen_server

-module(usr).
-behavior(gen_server).

-export([start_link/0, start_link/1, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).

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
%% gen_server behavior API callbacks

init(_) -> ok.
terminate(_, _) -> ok.
handle_call(_, _, _) -> ok.
handle_cast(_, _) -> ok.