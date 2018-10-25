-module(simple_timer).
-export([send_after/2, sleep/1, send/3]).

send_after(Time, Msg) ->
    spawn(simple_timer, send, [self(), Time, Msg]).

send(Pid, Time, Msg) ->
    receive
    after Time ->
        Pid ! Msg
    end.

sleep(Time) ->
    receive
    after Time ->
        true
    end.