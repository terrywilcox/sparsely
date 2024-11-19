%% Copyright 2022 Terry Wilcox
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

-module(sparsely).

-export([one_of/1, chain/1, optional/1, optional/2, repeat/2, repeat/1, character/1, wrap/2, match/1]).

wrap(Parser, F) when is_function(F) ->
	fun(S) -> F(Parser(S)) end.

character(Chars) when is_list(Chars) ->
	fun ([Character | Rest]) ->
			case lists:member(Character, Chars) of
				true -> {ok, Character, Rest};
				_ -> {error, "no matching character"}
			end;
	    ([]) ->
			{error, "no matching character"}
	end;
character(Chars) ->
	character([Chars]).

match(Chars) when is_list(Chars) ->
	Parsers = [character(Char) || Char <- Chars],
	chain(Parsers).

repeat(Parser, Times) when is_integer(Times) ->
	Parsers = [Parser || _ <- lists:seq(1, Times)],
	chain(Parsers).

repeat(Parser) ->
	fun(S) -> repeat_parse(S, Parser, []) end.

repeat_parse([], _Parser, Acc) ->
		{ok, lists:reverse(Acc), ""};
repeat_parse(S, Parser, Acc) ->
	case Parser(S) of
		{error, Reason} -> case length(Acc) of
				      0 -> {error, Reason};
				      _ ->{ok, lists:reverse(Acc), S}
				   end;
		{ok, ignore, Remainder} -> repeat_parse(Remainder, Parser, Acc);
		{ok, Value, Remainder} -> repeat_parse(Remainder, Parser, [Value | Acc])
	end.

one_of_parse(_S, []) ->
	{error, "no matching character"};
one_of_parse(S, [Parser| Parsers]) ->
	case Parser(S) of
		{error, _} -> one_of_parse(S, Parsers);
		Any -> Any
	end.

one_of(Parsers) when is_list(Parsers) ->
	fun(S) -> one_of_parse(S, Parsers) end.

chain_parse(S, [], Acc) ->
	{ok, lists:reverse(Acc), S};
chain_parse(S, [Parser|Parsers], Acc) when is_function(Parser) ->
	case Parser(S) of
		{error, _} = Error -> Error;
		{ok, ignore, Remainder} -> chain_parse(Remainder, Parsers, Acc);
		{ok, Value, Remainder} -> chain_parse(Remainder, Parsers, [Value | Acc])
	end.

chain(Parsers) when is_list(Parsers) ->
	fun(S) -> chain_parse(S, Parsers, []) end.

optional(Parser) ->
	optional(Parser, ignore).

optional(Parser, Default) ->
	fun(S) -> case Parser(S) of
			  {error, _} -> {ok, Default, S};
			  Any -> Any
		  end end.


