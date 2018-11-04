-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

% Finite state machine

start() -> ok.
stop() -> ok.
init() -> ok.
wait() -> ok.
signal() -> ok.