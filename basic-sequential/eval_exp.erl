-module(eval_exp).
-export([parse/1]).
-export([expressionToAST/2]).
-export([next_token/1, tokenize/1]).

% create AST from a simple arithmetic expression
% Expr:
%  (23+42)-4
% Into:
%  {minus, {plus, {num, 23}, {num, 42}}, {num, 4}}
parse(Lst) ->
    Tokens = tokenize(Lst),
    {Expr, Rest} = expressionToAST(Tokens, {}),
    case Rest of
        [] -> Expr;
        _  -> {parsing_error, Expr, Rest}
    end.


% converts a well-formated expression
% represented as tokenized stream into AST
% Expr:
%   [open_parenthesis, {num,23}, plus, {num,42}, close_parenthesis, minus, {num,4}]
% Into:
%  {minus, {plus, {num, 23}, {num, 42}}, {num, 4}}
expressionToAST([], HeadExp)    -> {HeadExp, []};
expressionToAST([H|T], HeadExp) ->
    case H of
        open_parenthesis ->
            {Expr, [close_parenthesis|Rest]} = expressionToAST(T, {}),
            expressionToAST(Rest, Expr);
        close_parenthesis ->
            {HeadExp, [H|T]};
        {num, Number} ->
            case T of
                [] ->
                    {{num, Number}, []};
                [Operation|TRest] ->
                    case Operation of
                        Op when Op =:= minus;
                                Op =:= plus ->
                            {NextExpr, Rest} = expressionToAST(TRest, {}),
                            {{Op, {num, Number}, NextExpr}, Rest};
                        _ -> {{num, Number}, T}
                    end
                end;
        Op when Op =:= minus;
                Op =:= plus ->
            {NextExp, Rest} = expressionToAST(T, {}),
            {{Op, HeadExp, NextExp}, Rest}
    end.

% tokenize simple arithmetic expression
% Expr:
%   (23+42)-4
% Into:
%   [open_parenthesis, {num,23}, plus, {num,42}, close_parenthesis, minus, {num,4}]
tokenize([])  -> [];
tokenize(Lst) ->
    NextToken = next_token(Lst),
    case NextToken of
        {error, Reason} -> [{error, Reason}];
        {{eof}, []}     -> [];
        {Token, Rest}   -> [Token|tokenize(Rest)]
    end.


next_token([])    -> {{eof}, []};
next_token([H|T]) ->
    case [H] of
        " " -> next_token(T);
        "(" -> {open_parenthesis, T};
        ")" -> {close_parenthesis, T};
        "{" -> {open_brace, T};  % curly bracket
        "}" -> {close_brace, T}; % curly bracket
        "[" -> {open_bracket, T};
        "]" -> {close_bracket, T};
        "-" -> {minus, T};
        "+" -> {plus, T};
        _   -> case string:to_integer([H|T]) of
                {error, _}  -> {error, unknown_token};
                {Int, Rest} -> {{num, Int}, Rest}
               end
    end.