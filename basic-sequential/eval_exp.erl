-module(eval_exp).
-export([next_token/1]).

% parse simple arithmetic expression
% Expr:
%   ((2+3)-4)
% Into:
%   {minus, {plus, {num, 2}, {num, 3}}, {num, 4}}
parse([]) -> [].

next_token([]) -> {{eof}, []};
next_token([H|T]) ->
    case [H] of
        " " -> next_token(T);
        "(" -> {{open_parenthesis}, T};
        ")" -> {{close_parenthesis}, T};
        "{" -> {{open_brace}, T};  % curly bracket
        "}" -> {{close_brace}, T}; % curly bracket
        "[" -> {{open_bracket}, T};
        "]" -> {{close_bracket}, T};
        "-" -> {{minus}, T};
        "+" -> {{plus}, T};
        _   -> case string:to_integer([H|T]) of
                {Int, Rest} -> {{num, Int}, Rest};
                {error, _}  -> {error, unknown_token}
               end
    end.

