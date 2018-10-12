-module(handling_errors).
-export([test/1]).


% 43
test(first) ->
    X=2,
    try (X=3) of
        Val -> {normal, Val}
    catch
        _:_ -> 43
    end;
% 43
test(second) ->
    X=2,
    try (X=3) of
        Val -> {normal, Val}
    catch
        error:_ -> 43
    end;
% {error, {badmatch, 3}}
test(third) ->
    X=2,
    try (X=3) of
        Val -> {normal, Val}
    catch
        error:Error -> {error,Error}
    end;
% {throw, non_normal_return}
test(fourth) ->
    X=2,
    try (throw(non_normal_return)) of
        Val -> {normal, Val}
    catch
        throw:Error -> {throw,Error}
    end.