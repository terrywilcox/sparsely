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

-export[parser/0].

%% Simple dice notation takes the form of NdS, where N is the number of dice and
%% S is the size of dice. So 3d6 is 3 six-sided dice. The 'd' can also be a 'D'.
%%

parser() ->
	% create a parser to match a a single digit
	Digit = sparsely:character("1234567890"),
	{ok, $3, <<"45">>} = Digit("345"),

	% create a parser to repeat the Digit parser until we run out of digits
	Digits = sparsely:repeat(Digit),
	{ok, "345", <<"x">>} = Digits("345x"),

	% wrap Digits parser to turn its output value into an integer
	PosInt = sparsely:wrap(Digits,
			       fun({ok, Value, Rest}) -> {Integer, _} = string:to_integer(Value),
							 {ok, Integer, Rest};
				  (Any) -> Any
			       end),
	{ok, 345, <<"x">>} = PosInt("345x"),
	
	% PosInt can now match N or S in NdS, so let's move on to matching the D
	% Match either upper or lower case d
	D = sparsely:character("dD"),
	{ok, $d, <<"6">>} = D("d6"),

	% D just gives us the letter 'd'. We want the word 'dice'.
	% Wrap D for a different value.
	Dice = sparsely:wrap(D, fun({ok, _, Rest}) -> {ok, dice, Rest};
				(Any) -> Any
			     end),
	{ok, dice, <<"4">>} = Dice("d4"),

	% now combine PosInt and Dice to parse NdS
	% use "chain" to create a sequence of parsers
	Roll = sparsely:chain([PosInt, Dice, PosInt]),
	{ok, [3, dice, 6], <<>>} = Roll("3d6"),
	
	% treat 'dice' like an operator for when we want to evaluate
	% wrap Roll to rearrange the output
	EvalRoll = sparsely:wrap(Roll, fun({ok, [NumberOfDice, dice, SidesOfDice], Rest}) ->
						      {ok, {dice, {NumberOfDice, SidesOfDice}}, Rest};
					  (Any) -> Any
				       end),
	{ok, {dice, {3, 6}}, <<>>} = EvalRoll("3d6"),
	{ok, {dice, {12, 20}}, <<>>} = EvalRoll("12d20"),

	EvalRoll.






