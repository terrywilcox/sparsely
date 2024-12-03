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

-export[digit/0, digits/0, pos_int/0, dice/0, number_of_dice/0].
-export[integer/0, float/0, operator/0, number/0].
-export[dice_term/0, variable/0, term/0, expression/0].
-export[expression_parse/1, pos_int_or_variable/0].
-export[number_or_variable/0].

%% Simple dice notation takes the form of NdS, where N is the number of dice and
%% S is the size of dice. So 3d6 is 3 six-sided dice. The 'd' can also be a 'D'.
%%

convert_to_fractional_float(Int) when is_integer(Int) ->
    Len = length(integer_to_list(Int)),
    Int / math:pow(10, Len).

white_space() ->
	sparsely:optional(sparsely:repeat(sparsely:character(" \t\n")), skip).

operator() ->
	sparsely:character("+-*/").

digit() ->
	sparsely:character("1234567890").

digits() ->
	sparsely:repeat(digit()).

alpha() ->
	sparsely:character("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ").

pos_int() ->
	sparsely:wrap(digits(),
		      fun({ok, Value, Rest}) -> {Integer, _} = string:to_integer(Value),
						{ok, Integer, Rest};
			 (Any) -> Any
		      end).

variable() ->
	VariableName = sparsely:repeat(alpha()),
	VariableToken = sparsely:character("$"),
	VariableParser = sparsely:chain([VariableToken, sparsely:character("{"),
					 VariableName, sparsely:character("}")]),
	sparsely:wrap(VariableParser,
		      fun({ok, [_, _, Name, _], Rest}) -> {ok, {variable, Name}, Rest};
			 (Any) -> Any
		      end).

integer() ->
	Negative = sparsely:character("-"),
	sparsely:wrap(sparsely:chain([sparsely:optional(Negative, 1), pos_int()]),
		      fun({ok, [Sign, Value], Rest}) ->
				      case Sign of
					  $- -> {ok, -Value, Rest};
					  _ -> {ok, Value, Rest}
				      end;
			 (Any) -> Any
		      end).

float() ->
	sparsely:wrap(sparsely:chain([integer(), sparsely:character("."), pos_int()]),
		      fun({ok, [Value, _, Fraction], Rest}) ->
				      case (Value < 0) == true of
					      true -> {ok, Value - convert_to_fractional_float(Fraction), Rest};
					      _ -> {ok, Value + convert_to_fractional_float(Fraction), Rest}
				      end;
			 (Any) -> Any
		      end).

number() ->
	sparsely:one_of([float(), integer()]).

dice() ->	
	D = sparsely:character("dD"),
	sparsely:wrap(D, fun({ok, _, Rest}) -> {ok, dice, Rest};
			    (Any) -> Any
			 end).

pos_int_or_variable() ->
	sparsely:one_of([variable(), pos_int()]).

number_or_variable() ->
	sparsely:one_of([variable(), number()]).

number_of_dice() ->
	sparsely:optional(pos_int_or_variable(), 1).

dice_term() ->
	Roll = sparsely:chain([number_of_dice(), dice(), pos_int_or_variable()]),
	sparsely:wrap(Roll,
		      fun({ok, [NumberOfDice, dice, SidesOfDice], Rest}) ->
				      {ok, {dice, {NumberOfDice, SidesOfDice}}, Rest};
			 (Any) -> Any
		      end).

term() ->
	sparsely:one_of([expression(), dice_term(), number_or_variable()]).

expression_parse(S) ->
	LeftBracket = sparsely:character("("),
	RightBracket = sparsely:character(")"),
	WhiteSpace = white_space(),
	Expression = sparsely:chain([WhiteSpace, LeftBracket,
				     WhiteSpace, term(),
				     WhiteSpace, operator(),
				     WhiteSpace, term(),
				     WhiteSpace, RightBracket,
				     WhiteSpace
				    ]),

	Wrapped = sparsely:wrap(Expression,
				fun({ok, [$(, Left, Op, Right, $)], Rest}) ->
						{ok, {Op, Left, Right}, Rest};
				   (Any) -> Any
				end),
	Wrapped(S).

expression() ->
	sparsely:parse_fun(dice_parser, expression_parse).






