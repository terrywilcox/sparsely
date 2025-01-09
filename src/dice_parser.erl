%% Copyright 2024 Terry Wilcox
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(dice_parser).

-export([digit/0, digits/0, pos_int/0, dice/0, number_of_dice/0]).
-export([integer/0, float/0, operator/0, number/0]).
-export([dice_term/0, variable/0, expression_factor/0, expression/0]).
-export([expression_parse/1, dice_const/0, expression_term/0]).
-export([number_or_variable/0, explode/0]).
-export([expression_term_parse/1, expression_factor_parse/1]).
-export([bracketed_expression_parse/1]).
-export([parser/0, dice_wrapper/1]).

%% Simple dice notation takes the form of NdS, where N is the number of dice and
%% S is the size of dice. So 3d6 is 3 six-sided dice. The 'd' can also be a 'D'.
%%

convert_to_fractional_float(Int) when is_integer(Int) ->
    Len = length(integer_to_list(Int)),
    Int / math:pow(10, Len).

white_space() ->
    sparsely:skip(
        sparsely:repeat(
            sparsely:character(" \t\n"))).

additive_operator() ->
    sparsely:wrap(
        sparsely:character("+-"),
        fun ({ok, $+, Rest}) ->
                {ok, add, Rest};
            ({ok, $-, Rest}) ->
                {ok, subtract, Rest};
            ({error, _} = Error) ->
                Error
        end).

multiplicative_operator() ->
    sparsely:wrap(
        sparsely:character("*/"),
        fun ({ok, $*, Rest}) ->
                {ok, multiply, Rest};
            ({ok, $/, Rest}) ->
                {ok, divide, Rest};
            ({error, _} = Error) ->
                Error
        end).

operator() ->
    sparsely:one_of([additive_operator(), multiplicative_operator()]).

digit() ->
    sparsely:character("1234567890").

digits() ->
    sparsely:repeat(digit()).

alpha() ->
    sparsely:character("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").

pos_int() ->
    sparsely:wrap(digits(),
                  fun ({error, _} = Error) ->
                          Error;
                      ({ok, Value, Rest}) ->
                          case string:to_integer(Value) of
                              {error, _} ->
                                  {error, "Not a valid integer"};
                              {Integer, _} ->
                                  {ok, Integer, Rest}
                          end
                  end).

variable() ->
    VariableName = sparsely:repeat(alpha()),
    VariableToken = sparsely:character("$"),
    VariableParser =
        sparsely:chain([VariableToken,
                        sparsely:character("{"),
                        VariableName,
                        sparsely:character("}")]),
    sparsely:wrap(VariableParser,
                  fun ({ok, [_, _, Name, _], Rest}) ->
                          {ok, {variable, Name}, Rest};
                      ({error, _} = Error) ->
                          Error
                  end).

integer() ->
    Negative = sparsely:character("-"),
    sparsely:wrap(
        sparsely:chain([sparsely:optional(Negative, 1), pos_int()]),
        fun ({ok, [Sign, Value], Rest}) ->
                case Sign of
                    $- ->
                        {ok, -Value, Rest};
                    _ ->
                        {ok, Value, Rest}
                end;
            ({error, _} = Error) ->
                Error
        end).

float() ->
    sparsely:wrap(
        sparsely:chain([integer(), sparsely:character("."), pos_int()]),
        fun ({ok, [Value, _, Fraction], Rest}) ->
                case (Value < 0) == true of
                    true ->
                        {ok, Value - convert_to_fractional_float(Fraction), Rest};
                    _ ->
                        {ok, Value + convert_to_fractional_float(Fraction), Rest}
                end;
            ({error, _} = Error) ->
                Error
        end).

number() ->
    sparsely:one_of([float(), integer()]).

const() ->
    sparsely:one_of([float(), integer(), variable()]).

dice() ->
    D = sparsely:character("dD"),
    sparsely:wrap(D,
                  fun ({ok, _, Rest}) ->
                          {ok, dice, Rest};
                      ({error, _} = Error) ->
                          Error
                  end).

dice_const() ->
    sparsely:one_of([variable(), pos_int()]).

number_or_variable() ->
    sparsely:one_of([variable(), number()]).

number_of_dice() ->
    sparsely:optional(dice_const(), 1).

explode() ->
    ExplodePoint = sparsely:optional(pos_int()),
    Explode = sparsely:character("!"),
    ExplodeWrap =
        sparsely:wrap(Explode,
                      fun ({ok, _, Rest}) ->
                              {ok, explode, Rest};
                          ({error, _} = Error) ->
                              Error
                      end),

    ExplodeChain = sparsely:chain([ExplodeWrap, ExplodePoint]),

    sparsely:optional(ExplodeChain).

keep_dice() ->
    Keep =
        sparsely:wrap(
            sparsely:character("kK"),
            fun ({ok, _, Rest}) ->
                    {ok, keep, Rest};
                ({error, _} = Error) ->
                    Error
            end),

    Drop =
        sparsely:wrap(
            sparsely:character("dD"),
            fun ({ok, _, Rest}) ->
                    {ok, drop, Rest};
                ({error, _} = Error) ->
                    Error
            end),

    Highest =
        sparsely:wrap(
            sparsely:character("hH"),
            fun ({ok, _, Rest}) ->
                    {ok, highest, Rest};
                ({error, _} = Error) ->
                    Error
            end),

    Lowest =
        sparsely:wrap(
            sparsely:character("lL"),
            fun ({ok, _, Rest}) ->
                    {ok, lowest, Rest};
                ({error, _} = Error) ->
                    Error
            end),

    NumberToKeep = sparsely:optional(dice_const(), 1),

    KeepDice =
        sparsely:chain([sparsely:one_of([Keep, Drop]),
                        sparsely:optional(
                            sparsely:one_of([Highest, Lowest]), highest),
                        NumberToKeep]),
    sparsely:optional(KeepDice).

dice_roll() ->
    sparsely:chain([number_of_dice(), dice(), dice_const()]).

dice_term() ->
    Roll = sparsely:chain([dice_roll(), explode(), keep_dice()]),
    F = fun dice_parser:dice_wrapper/1,
    sparsely:wrap(Roll, F).

dice_wrapper({error, _} = Error) ->
    Error;
dice_wrapper([{error, _} | _] = Error) ->
    Error;
dice_wrapper([{ok, DiceResult, Rest}]) ->
    [{ok, dice_wrapper(DiceResult, []), Rest}];
dice_wrapper({ok, DiceResult, Rest}) ->
    {ok, dice_wrapper(DiceResult, []), Rest}.

dice_wrapper([], Acc) ->
    Dice = proplists:get_value(dice, Acc),
    Roll = {dice, Dice},
    Roll2 =
        case proplists:get_value(explode, Acc, undefined) of
            undefined ->
                Roll;
            Point ->
                {explode, {Point, Roll}}
        end,
    case proplists:get_value(keep, Acc, undefined) of
        undefined ->
            Roll2;
        {Keep, Parity, Number} ->
            {Keep, {Parity, Number}, Roll2}
    end;
dice_wrapper([[NumberOfDice, dice, SidesOfDice] | Rest], Acc) ->
    dice_wrapper(Rest, [{dice, {NumberOfDice, SidesOfDice}} | Acc]);
dice_wrapper([[explode, Point] | Rest], Acc) ->
    dice_wrapper(Rest, [{explode, Point} | Acc]);
dice_wrapper([[explode] | Rest], Acc) ->
    dice_wrapper(Rest, [{explode, maximum} | Acc]);
dice_wrapper([[Keep, Parity, Number] | Rest], Acc) when Keep == keep; Keep == drop ->
    dice_wrapper(Rest, [{keep, {Keep, Parity, Number}} | Acc]).

%% expression_factor is an expression in brackets, a dice_term, or a const
%% expression_term is an expression_factor followed by zero or more pairs of a multiplicative_operator and another expression_factor
%% expression is an expression_term followed by zero or more pairs of additive_operator and another expression_term

bracketed_expression_parse(S) ->
    LeftBracket = sparsely:chain([white_space(), sparsely:character("("), white_space()]),
    RightBracket =
        sparsely:ignore(
            sparsely:chain([white_space(), sparsely:character(")"), white_space()])),
    Expression = expression(),

    InnerExpression =
        case LeftBracket(S) of
            {ok, _, Rest} ->
                case Expression(Rest) of
                    {error, _} = Error ->
                        Error;
                    {ok, ExpressionValue, Rest2} ->
                        case RightBracket(Rest2) of
                            {error, _} ->
                                {error, "No right bracket"};
                            {ok, _, Rest3} ->
                                {ok, ExpressionValue, Rest3}
                        end
                end;
            {error, _} = Error ->
                Error
        end,
    InnerExpression.

bracketed_expression() ->
    sparsely:parse_fun(dice_parser, bracketed_expression_parse).

expression_factor_parse(S) ->
    ExpressionFactor = sparsely:one_of([bracketed_expression(), dice_term(), const()]),
    ExpressionFactor(S).

expression_factor() ->
    sparsely:parse_fun(dice_parser, expression_factor_parse).

expression_term_parse(S) ->
    ExpressionFactor = expression_factor(),
    MultiplicativeOperator = multiplicative_operator(),

    Term =
        case ExpressionFactor(S) of
            {error, _} = Error ->
                Error;
            {ok, Left, Rest} ->
                case MultiplicativeOperator(Rest) of
                    {error, _} ->
                        {ok, Left, Rest};
                    {ok, Operator, Rest2} ->
                        case expression_term_parse(Rest2) of
                            {error, _} = Error ->
                                Error;
                            {ok, Right, Rest3} ->
                                {ok, {Operator, Left, Right}, Rest3}
                        end
                end
        end,

    Term.

expression_term() ->
    sparsely:parse_fun(dice_parser, expression_term_parse).

expression_parse(S) ->
    ExpressionTerm = expression_term(),
    AdditiveOperator = additive_operator(),

    Expression =
        case ExpressionTerm(S) of
            {error, _} = Error ->
                Error;
            {ok, Left, Rest} ->
                case AdditiveOperator(Rest) of
                    {error, _} ->
                        {ok, Left, Rest};
                    {ok, Operator, Rest2} ->
                        case expression_parse(Rest2) of
                            {error, _} = Error ->
                                Error;
                            {ok, Right, Rest3} ->
                                {ok, {Operator, Left, Right}, Rest3}
                        end
                end
        end,

    Expression.

expression() ->
    sparsely:parse_fun(dice_parser, expression_parse).

parser() ->
    expression().
