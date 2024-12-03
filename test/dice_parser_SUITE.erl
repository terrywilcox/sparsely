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

-module (dice_parser_SUITE).
-include_lib("eunit/include/eunit.hrl").
-include("include/sparsely.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2, end_per_testcase/2]).
-export([single_die/1, multiple_dice/1, variable/1, number_of_dice/1, variable_sides/1]).
-export([integer/1, float/1, number/1, term/1, expression/1]).


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
     	integer, float, number, single_die, multiple_dice,
	variable, number_of_dice, term, expression, variable_sides
    ].

%assertError(Error) ->
%	?assertEqual({error, "no matching character"}, Error).

integer(_Config) ->
	Parser = dice_parser:integer(),
	?assertEqual({ok, 300, <<>>}, Parser("300")),
	?assertEqual({ok, -300, <<>>}, Parser("-300")).

float(_Config) ->
	Parser = dice_parser:float(),
	?assertEqual({ok, 300.2, <<>>}, Parser("300.2")),
	?assertEqual({ok, -300.2, <<>>}, Parser("-300.2")).

number(_Config) ->
	Parser = dice_parser:number(),
	?assertEqual({ok, -10, <<"+">>}, Parser("-10+")),
	?assertEqual({ok, 10.2, <<"bbb">>}, Parser("10.2bbb")),
	?assertEqual({ok, -33.3, <<"   4">>}, Parser("-33.3   4")).

number_of_dice(_Config) ->
	Parser = dice_parser:number_of_dice(),
	?assertEqual({ok, 3, <<"d6">>}, Parser("3d6")),
	% TODO: optional with default does not convert S to binary
	?assertEqual({ok, 1, "d6"}, Parser("d6")),
	?assertEqual({ok, {variable, "x"}, <<"d6">>}, Parser("${x}d6")),
	?assertEqual({ok, 10, <<"d6">>}, Parser("10d6")).

single_die(_Config) ->
	Parser = dice_parser:dice_term(),
	?assertEqual({ok, {dice, {1, 6}}, <<>>}, Parser("d6")).

multiple_dice(_Config) ->
	Parser = dice_parser:dice_term(),
	?assertEqual({ok, {dice, {10, 12}}, <<>>}, Parser("10d12")).

variable_sides(_Config) ->
	Parser = dice_parser:dice_term(),
	?assertEqual({ok, {dice, {10, {variable, "x"}}}, <<>>}, Parser("10d${x}")).

variable(_Config) ->
	Parser = dice_parser:variable(),
	?assertEqual({ok, {variable, "CHA"}, <<"555">>}, Parser("${CHA}555")).

term(_Config) ->
	Parser = dice_parser:term(),
	?assertEqual({ok, {dice, {10, 12}}, <<"bbb">>}, Parser("10d12bbb")),
	?assertEqual({ok, {variable, "bob"}, <<>>}, Parser("${bob}")),
	?assertEqual({ok, 1012, <<"bbb">>}, Parser("1012bbb")).

expression(_Config) ->
	Parser = dice_parser:expression(),
	?assertEqual({ok, {$+, {dice, {10, 12}}, 3}, <<>>}, Parser("(10d12+3)")),
	?assertEqual({ok, {$*, {variable, "bob"}, 7}, <<>>}, Parser("(${bob}*7)")),
	?assertEqual({ok, {$-, {dice, {10, 12}}, {variable, "bbb"}}, <<>>}, Parser("(10d12-${bbb})")),
	?assertEqual({ok, {$+, 42, {$-, {dice, {3, 12}}, {variable, "bbb"}}}, <<>>}, Parser("(42+(3d12-${bbb}))")),
	?assertEqual({ok, {$+, 42, {$-, {dice, {{variable, "x"}, 12}}, {variable, "bbb"}}}, <<>>}, Parser("(42+(${x}d12-${bbb}))")).

