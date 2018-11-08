-module(db).
-export([start/0, stop/0, write/2, delete/1, read/1, match/1]).

% ---

start() -> ok.

stop() -> ok.

write(Key, Element) -> ok.

delete(Key) -> ok.

read(Key) -> {ok, element}.

match(Element) -> [key1, key2].

% ---