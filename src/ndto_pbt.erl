%%% Copyright 2023 Nomasystems, S.L. http://www.nomasystems.com
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
%% limitations under the License
-module(ndto_pbt).

%%% INCLUDE FILES
-include("ndto.hrl").
-include("ndto_pbt.hrl").

%%% EXTERNAL EXPORTS
-export([
    dto/1,
    dto/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec dto(Schema) -> TestDataGenerator when
    Schema :: schema(),
    TestDataGenerator :: test_data_generator().
dto(Schema) ->
    dto(Schema, triq).

-spec dto(Schema, PBTTool) -> TestDataGenerator when
    Schema :: schema(),
    PBTTool :: pbt_tool(),
    TestDataGenerator :: test_data_generator().
dto(Schema, PBTTool) ->
    PBTToolModule = erlang:binary_to_atom(<<"ndto_pbt_", (erlang:atom_to_binary(PBTTool))/binary>>),
    PBTToolModule:dto(Schema).
