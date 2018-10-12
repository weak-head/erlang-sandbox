-module(handling_errors).
-export([test/1, return_error/1, try_return/1]).


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


return_error(X) when X < 0 ->
    throw({'EXIT', {badarith, [{exception,return_error,1},
                               {erl_eval,do_apply,5},
                               {shell,exprs,6},
                               {shell,eval_exprs,6},
                               {shell,eval_loop,3}]}});
return_error(X) when X == 0 ->
    1/X;
return_error(X) when X > 0 ->
    {'EXIT', {badarith, [{exception,return_error,1},
                         {erl_eval,do_apply,5},
                         {shell,exprs,6},
                         {shell,eval_exprs,6},
                         {shell,eval_loop,3}]}}.

% three different kinds of behavior:
% try_return(1).
% try_return(0).
% try_return(-1).
try_return(X) when is_integer(X) ->
    try return_error(X) of
        Val -> {normal, Val}
    catch
        exit:Reason -> {exit, Reason};
        throw:Throw -> {throw, Throw};
        error:Error -> {error, Error}
    end.