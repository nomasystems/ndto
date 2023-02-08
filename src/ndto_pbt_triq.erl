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
-module(ndto_pbt_triq).

%%% INCLUDE FILES
-include("ndto.hrl").
-include("ndto_pbt.hrl").

%%% EXTERNAL EXPORTS
-export([
    dto/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec dto(Schema) -> TestDataGenerator when
    Schema :: schema(),
    TestDataGenerator :: test_data_generator().
dto(#{<<"type">> := <<"allOf">>} = Schema) ->
    all_of(Schema);
dto(#{<<"type">> := <<"anyOf">>} = Schema) ->
    any_of(Schema);
dto(#{<<"type">> := <<"array">>} = Schema) ->
    array(Schema);
dto(#{<<"type">> := <<"boolean">>} = Schema) ->
    boolean(Schema);
dto(#{<<"type">> := <<"integer">>} = Schema) ->
    integer(Schema);
dto(#{<<"type">> := <<"not">>} = Schema) ->
    'not'(Schema);
dto(#{<<"type">> := <<"number">>} = Schema) ->
    number(Schema);
dto(#{<<"type">> := <<"object">>} = Schema) ->
    object(Schema);
dto(#{<<"type">> := <<"oneOf">>} = Schema) ->
    one_of(Schema);
dto(#{<<"type">> := <<"string">>} = Schema) ->
    string(Schema);
dto(_Schema) ->
    any().

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec all_of(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
all_of(_Schema) ->
    erlang:throw(not_implemented).

-spec any() -> Dom when
    Dom :: test_data_generator().
any() ->
    erlang:throw(not_implemented).

-spec any_of(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
any_of(_Schema) ->
    erlang:throw(not_implemented).

-spec array(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
array(_Schema) ->
    erlang:throw(not_implemented).

-spec boolean(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
boolean(_Schema) ->
    triq_dom:bool().

-spec integer(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
integer(_Schema) ->
    erlang:throw(not_implemented).

-spec 'not'(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
'not'(_Schema) ->
    erlang:throw(not_implemented).

-spec number(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
number(_Schema) ->
    erlang:throw(not_implemented).

-spec object(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
object(_Schema) ->
    erlang:throw(not_implemented).

-spec one_of(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
one_of(_Schema) ->
    erlang:throw(not_implemented).

-spec string(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
string(_Schema) ->
    erlang:throw(not_implemented).
