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

-module (sparsely_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include("include/sparsely.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([parse_single_char/1, parse_char_from_set/1, parse_repeat/1]).
-export([parse_one_of/1, parse_no_chars/1, parse_one_of_none/1, parse_match/1]).
-export([parse_repeat_times/1, parse_chain/1, parse_optional/1, parse_wrap/1]).
-export([parse_optional_with_default/1]).

init_per_suite(Config) ->
    ct:pal("Initializing test suite...~n"),
    Config.

end_per_suite(_Config) ->
    ct:pal("Ending test suite...~n"),
    ok.

init_per_testcase(TestCase, Config) ->
    ct:pal("Setting up for test case: ~p~n", [TestCase]),
    Config.

end_per_testcase(TestCase, _Config) ->
    ct:pal("Tearing down test case: ~p~n", [TestCase]),
    ok.

% Test cases
all() ->
    [
        parse_single_char, parse_char_from_set, parse_repeat,
	parse_one_of, parse_no_chars, parse_one_of_none,
	parse_repeat_times, parse_chain, parse_optional,
	parse_match, parse_wrap, parse_optional_with_default
    ].

assertError(Error) ->
	?assertEqual({error, "no matching character"}, Error).

parse_no_chars(_Config) ->
	Parser = sparsely:character("a"),
	assertError(Parser("")).

parse_single_char(_Config) ->
	Parser = sparsely:character("a"),
	Result = Parser("ated"),
	?assertEqual({ok, $a, "ted"}, Result), 

	assertError(Parser("zaz")).

parse_char_from_set(_Config) ->
	Parser = sparsely:character("abcde"),
	Result = Parser("bted"),
	?assertEqual({ok, $b, "ted"}, Result), 

	assertError(Parser("zaz")).

parse_one_of(_Config) ->
	Parser = sparsely:one_of([?DIGIT, ?LOWERCASE]),

	Result = Parser("3a9"),
	?assertEqual({ok, $3, "a9"}, Result), 

	Result2 = Parser("a69"),
	?assertEqual({ok, $a, "69"}, Result2), 

	assertError(Parser("Zaz")).

parse_one_of_none(_Config) ->
	Parser = sparsely:one_of([]),
	assertError(Parser("zaz")).

parse_repeat(_Config) ->
	Parser = sparsely:repeat(?DIGIT),

	Result = Parser("369"),
	?assertEqual({ok, "369", ""}, Result), 

	Result2 = Parser("369999x6"),
	?assertEqual({ok, "369999", "x6"}, Result2), 

	assertError(Parser("zaz")).

parse_chain(_Config) ->
	Digit = ?DIGIT,
	Lower = ?LOWERCASE,
	Parser = sparsely:chain([Digit, Lower, Digit, Digit]),

	Result = Parser("3x69"),
	?assertEqual({ok, "3x69", ""}, Result), 

	Result2 = Parser("3x69999x6"),
	?assertEqual({ok, "3x69", "999x6"}, Result2), 

	assertError(Parser("3x6")),
	assertError(Parser("x66zaz")).

parse_optional(_Config) ->
	Digit = ?DIGIT,
	Lower = ?LOWERCASE,
	Optional = sparsely:optional(Digit),
	Parser = sparsely:chain([Optional, Lower, Digit, Digit]),

	Result = Parser("x69"),
	?assertEqual({ok, "x69", ""}, Result), 

	Result2 = Parser("3x69"),
	?assertEqual({ok, "3x69", ""}, Result2), 

	Result3 = Parser("3x69999x6"),
	?assertEqual({ok, "3x69", "999x6"}, Result3), 

	Result4 = Parser("x69999x6"),
	?assertEqual({ok, "x69", "999x6"}, Result4), 

	assertError(Parser("3x6")),
	assertError(Parser("X66zaz")).

parse_optional_with_default(_Config) ->
	Digit = ?DIGIT,
	Lower = ?LOWERCASE,
	Optional = sparsely:optional(Digit, $N),
	Parser = sparsely:chain([Optional, Lower, Digit, Digit]),

	Result = Parser("x69"),
	?assertEqual({ok, "Nx69", ""}, Result), 

	Result2 = Parser("3x69"),
	?assertEqual({ok, "3x69", ""}, Result2), 

	Result3 = Parser("3x69999x6"),
	?assertEqual({ok, "3x69", "999x6"}, Result3), 

	Result4 = Parser("x69999x6"),
	?assertEqual({ok, "Nx69", "999x6"}, Result4), 

	assertError(Parser("3x6")),
	assertError(Parser("X66zaz")).

parse_repeat_times(_Config) ->
	Parser = sparsely:repeat(?DIGIT, 3),

	Result = Parser("369"),
	?assertEqual({ok, "369", ""}, Result), 

	Result2 = Parser("369999x6"),
	?assertEqual({ok, "369", "999x6"}, Result2), 

	assertError(Parser("66")),
	assertError(Parser("66zaz")).

parse_match(_Config) ->
	Parser = sparsely:match("1-800"),

	Result = Parser("1-800"),
	?assertEqual({ok, "1-800", ""}, Result), 

	Result2 = Parser("1-800-555-1212"),
	?assertEqual({ok, "1-800", "-555-1212"}, Result2), 

	assertError(Parser("1-801-555-1212")),
	assertError(Parser("1-80")).

parse_wrap(_Config) ->
	Digit = ?DIGIT,

	Parser = sparsely:wrap(sparsely:repeat(Digit),
				       fun({ok, Value, Rest}) -> {Integer, _} = string:to_integer(Value),
								 {ok, Integer, Rest};
					  (Any) -> Any
				       end),

	Result = Parser("3456"),
	?assertEqual({ok, 3456, ""}, Result), 

	Result2 = Parser("3456y"),
	?assertEqual({ok, 3456, "y"}, Result2), 

	assertError(Parser("x80")).




