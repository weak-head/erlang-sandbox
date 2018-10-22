-module(eval_exp).
-export([parse/1]).
-export([expressionToAST/2]).
-export([next_token/1, tokenize/1]).

% parses a string representation of a simple
% arithmetic expression and converts it into AST
%
% Converts:
%  "(2+3+4)-(11+77-(23+77)+14)"
%
% Into:
%  {minus,{plus,{num,2},{plus,{num,3},{num,4}}},
%         {plus,{num,11},
%               {minus,{num,77},{plus,{plus,{num,23},{num,77}},{num,14}}}}}
%
parse(Expression) ->
    Tokens = tokenize(Expression),
    {Expr, Rest} = expressionToAST(Tokens, {}),
    case Rest of
        [] -> Expr;
        _  -> {parsing_error, Expr, Rest}
    end.


% converts a well-formated,
% previously tokenized expression into AST
%
% Converts:
%   [open_parenthesis, {num,23}, plus, {num,42}, close_parenthesis, minus, {num,4}]
%
% Into:
%  {minus, {plus, {num, 23}, {num, 42}}, {num, 4}}
%
expressionToAST([], HeadExp)    ->
    {HeadExp, []};

expressionToAST([open_parenthesis|T], _HeadExp) ->
    {Expr, [close_parenthesis|Rest]} = expressionToAST(T, {}),
    expressionToAST(Rest, Expr);

expressionToAST([close_parenthesis|T], HeadExp) ->
    {HeadExp, [close_parenthesis|T]};

expressionToAST([{num, Number}|T], _HeadExp) ->
    case T of
        [] -> {{num, Number}, []};
        [Operation|TRest] ->
            case Operation of
                Op when Op =:= minus;
                        Op =:= plus ->
                    {NextExpr, Rest} = expressionToAST(TRest, {}),
                    {{Op, {num, Number}, NextExpr}, Rest};
                _ -> {{num, Number}, T}
            end
    end;

expressionToAST([Op|T], HeadExp) when Op =:= minus;
                                      Op =:= plus ->
    {NextExp, Rest} = expressionToAST(T, {}),
    {{Op, HeadExp, NextExp}, Rest}.


% tokenize simple arithmetic expression
%
% Expr:
%   (23+42)-4
%
% Into:
%   [open_parenthesis, {num,23}, plus, {num,42}, close_parenthesis, minus, {num,4}]
%
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