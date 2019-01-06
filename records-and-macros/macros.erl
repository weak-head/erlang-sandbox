-module(macros).

-export([double/1]).
-export([theFun/2]).
-export([someStuff/0]).

% the simplest one
-define(TIMEOUT, 1000).

% similar to C
-define(FUNC, X).
-define(TION, +X).
double(X) -> ?FUNC?TION.

% with params
-define(TooMuch(X, Y), (X * Y) > 100).

theFun(A, B) when ?TooMuch(A, B) -> false;
theFun(_A, _B) -> true.

% conditional macros definition
-ifdef(debug).
    -define(DBG(Str, Args), io:format(Str, Args)).
-else.
    -define(DBG(Str, Args), ok).
-endif.

% c(macros, [{d, debug}]).
someStuff() ->
    ?DBG("doing some stuff [(~s)] ~n", ["done ^_^ done"]),
    ok.