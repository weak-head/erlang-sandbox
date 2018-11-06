-module(event_manager_test).
-import(event_manager, [start/2, stop/1, send_event/2, add_handler/3, get_data/2, delete_handler/2]).
% -import(log_handler, [init/1, terminate/1, handle_event/2]).
% -import(io_handler, [init/1, terminate/2, handle_event/2]).
-export([run/0]).

run() ->
    start(alarm, [{log_handler, "AlarmLog"}]),
    send_event(alarm, {raise_alarm, 10, cabinet_open}),

    add_handler(alarm, io_handler, 1),
    send_event(alarm, {clear_alarm, 10, cabinet_open}),
    send_event(alarm, {event, 156, link_up}),
    get_data(alarm, io_handler),

    delete_handler(alarm, stats_handler),

    stop(alarm).