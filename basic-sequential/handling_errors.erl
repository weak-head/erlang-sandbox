-module(handling_errors).
-export([test/1, return_error/1, try_return/1, try_wildcard/1, try_shadow/1, return_catch/1]).


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
return_error(X) when X == 42 ->
    exit({badarith, [{exception, return_error, 1},
                     {erl_eval, do_apply, 5},
                     {shell, exprs, 6},
                     {shell, eval_exprs, 6},
                     {shell, eval_loop, 3}]});
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

% as an example of try catch
% try_wildcard(-1).
% try_wildcard(0).
% try_wildcard(1).
try_wildcard(X) when is_integer(X) ->
    try return_error(X)
        catch
        throw:Throw -> {throw, Throw};
        error:_     -> error;
        Type:Error  -> {Type, Error};
        _           -> other;
        _:_         -> other
    end.

% try_shadow(-1).
% try_shadow(0).
% try_shadow(1).
try_shadow(X) when is_integer(X) ->
    try return_error(X) of
        Val -> {normal, Val}
        catch
        exit:_  -> 34;
        throw:_ -> 109;
        error:_ -> 191
    end.


% --------------------------------------
% it's better to avoid `catch` expression:
% we are going to have the identical result for
% all these four calls:
%
% return_catch(0).
% return_catch(-1).
% return_catch(1).
% return_catch(42).
return_catch(X) when is_integer(X) ->
    catch return_error(X).
