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

-define(DIGIT, sparsely:character("0123456789")).
-define(POSITIVE_INTEGER, sparsely:repeat(?DIGIT)).
-define(UPPERCASE, sparsely:character("ABCDEFGHIJKLMNOPQRSTUVWXYZ")).
-define(LOWERCASE, sparsely:character("abcdefghijklmnopqrstuvwxyz")).
-define(ALPHA, sparsely:one_of([?UPPERCASE, ?LOWERCASE])).
-define(ALPHANUMERIC, sparsely:one_of([?DIGIT, ?ALPHA])).
-define(WHITESPACE, sparsely:repeat(character(" \t"))).
