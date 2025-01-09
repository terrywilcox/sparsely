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

-module(dice_parser_SUITE).

-include_lib("eunit/include/eunit.hrl").

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([variable/1, dice/1, explode/1]).
-export([integer/1, float/1, number/1, expression/1, keep/1]).
-export([target_number/1]).

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
    [integer, float, number, explode, variable, dice, expression, keep, target_number].

integer(_Config) ->
    Parser = dice_parser:parser(),
    ?assertEqual({ok, 300, <<>>}, Parser("300")),
    ?assertEqual({ok, -300, <<>>}, Parser("-300")).

float(_Config) ->
    Parser = dice_parser:parser(),
    ?assertEqual({ok, 300.2, <<>>}, Parser("300.2")),
    ?assertEqual({ok, -300.2, <<>>}, Parser("-300.2")).

number(_Config) ->
    Parser = dice_parser:parser(),
    ?assertEqual({ok, -10, <<>>}, Parser("-10")),
    ?assertEqual({ok, -10, <<"t">>}, Parser("-10t")),
    ?assertEqual({ok, 10.2, <<"bbb">>}, Parser("10.2bbb")),
    ?assertEqual({ok, -33.3, <<"   4">>}, Parser("-33.3   4")).

dice(_Config) ->
    Parser = dice_parser:parser(),
    ?assertEqual({ok, {dice, {3, 6}}, <<>>}, Parser("3d6")),
    ?assertEqual({ok, {dice, {1, 6}}, <<>>}, Parser("d6")),
    ?assertEqual({ok, {dice, {{variable, "x"}, {variable, "y"}}}, <<>>}, Parser("${x}d${y}")).

explode(_Config) ->
    Parser = dice_parser:parser(),
    ?assertEqual({ok, {explode, maximum, {dice, {3, 6}}}, <<>>}, Parser("3d6!")),
    ?assertEqual({ok, {explode, 4, {dice, {3, 6}}}, <<>>}, Parser("3d6!4")),
    ?assertEqual({ok, {explode, maximum, {dice, {{variable, "x"}, {variable, "y"}}}}, <<>>},
                 Parser("${x}d${y}!")).

keep(_Config) ->
    Parser = dice_parser:parser(),
    ?assertEqual({ok, {keep, {highest, 1}, {dice, {10, 6}}}, <<>>}, Parser("10d6kh1")),
    ?assertEqual({ok, {keep, {highest, 1}, {dice, {10, 6}}}, <<>>}, Parser("10d6k1")),
    ?assertEqual({ok, {keep, {lowest, 3}, {dice, {10, 6}}}, <<>>}, Parser("10d6kl3")),
    ?assertEqual({ok, {drop, {lowest, 3}, {dice, {10, 6}}}, <<>>}, Parser("10d6dl3")),
    ?assertEqual({ok, {drop, {lowest, 3}, {explode, 4, {dice, {10, 6}}}}, <<>>},
                 Parser("10d6!4dl3")).

target_number(_Config) ->
    Parser = dice_parser:parser(),
    ?assertEqual({ok, {target, {gte, 3}, {keep, {highest, 1}, {dice, {10, 6}}}}, <<>>},
                 Parser("10d6kh1>3")),
    ?assertEqual({ok, {target, {lte, 1}, {keep, {lowest, 1}, {dice, {10, 6}}}}, <<>>},
                 Parser("10d6kl1<1")).

variable(_Config) ->
    Parser = dice_parser:variable(),
    ?assertEqual({ok, {variable, "CHA"}, <<"555">>}, Parser("${CHA}555")).

expression(_Config) ->
    Parser = dice_parser:parser(),
    ?assertEqual({ok, {dice, {3, 6}}, <<>>}, Parser("3d6")),
    ?assertEqual({ok, {add, {dice, {3, 6}}, {multiply, 7, 4}}, <<>>}, Parser("3d6+7*4")),
    ?assertEqual({ok, {add, {multiply, {dice, {3, 6}}, 7}, 4}, <<>>}, Parser("3d6*7+4")),
    ?assertEqual({ok, {multiply, {dice, {3, 6}}, {add, 7, 4}}, <<>>}, Parser("3d6*(7+4)")),
    ?assertEqual({ok, {divide, {add, 7, 5}, {subtract, 6, 3}}, <<>>},
                 Parser("((7+5)/(6-3))")),
    ?assertEqual({ok, {divide, {add, 7, 5}, {subtract, 6, {variable, "var"}}}, <<>>},
                 Parser("((7+5)/(6-${var}))")).
