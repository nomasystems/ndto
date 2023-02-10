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
%% limitations under the License.
-module(ndto_pbt_properties).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
prop_enum() ->
    Enum = [
        0,
        true,
        <<"foo">>,
        [1, false, <<"bar">>, #{<<"foo">> => <<"baz">>}],
        #{<<"foo">> => <<"bar">>}
    ],
    Schema = #{<<"enum">> => Enum},
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        lists:member(Value, Enum)
    ).

prop_boolean() ->
    Schema = #{<<"type">> => <<"boolean">>},
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        is_boolean(Value)
    ).

prop_string() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"minLength">> => 2,
        <<"maxLength">> => 4
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        string:length(Value) >= 2 andalso string:length(Value) =< 4
    ).

prop_string_base64() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"base64">>,
        <<"minLength">> => 4,
        <<"maxLength">> => 4
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        try
            string:length(Value) =:= 4
        catch
            _Class:_Reason ->
                false
        end
    ).

'prop_string_iso8601-datetime'() ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"iso8601-datetime">>
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        ncalendar:is_valid(iso8601, Value)
    ).
