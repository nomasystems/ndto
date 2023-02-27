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
prop_array_1() ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{
            <<"type">> => <<"number">>
        }
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        is_list(Value)
    ).

prop_array_2() ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"items">> => #{
            <<"type">> => <<"boolean">>
        },
        <<"minItems">> => 1,
        <<"maxItems">> => 5,
        <<"uniqueItems">> => true
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        is_list(Value) andalso erlang:length(Value) >= 1 andalso is_boolean(erlang:hd(Value)) andalso
            erlang:length(Value) =< 2
    ).

prop_boolean() ->
    Schema = #{<<"type">> => <<"boolean">>},
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        is_boolean(Value)
    ).

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

prop_integer_1() ->
    Schema = #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => 0,
        <<"maximum">> => 0
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        Value =:= 0
    ).

prop_integer_2() ->
    Schema = #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => 2,
        <<"exclusiveMinimum">> => false,
        <<"maximum">> => 12,
        <<"exclusiveMaximum">> => true,
        <<"multipleOf">> => 3
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        Value >= 2 andalso Value < 12 andalso (Value rem 3) =:= 0
    ).

prop_integer_3() ->
    Schema = #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => -12,
        <<"exclusiveMinimum">> => true,
        <<"maximum">> => 2,
        <<"exclusiveMaximum">> => false,
        <<"multipleOf">> => -3
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        Value > -12 andalso Value =< 2 andalso (Value rem 3) =:= 0
    ).

prop_number_1() ->
    Schema = #{
        <<"type">> => <<"number">>,
        <<"minimum">> => -2.5,
        <<"exclusiveMinimum">> => true,
        <<"maximum">> => 2.5,
        <<"exclusiveMaximum">> => false
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        Value > -2.5 andalso Value =< 2.5
    ).

prop_number_2() ->
    Schema = #{
        <<"type">> => <<"number">>,
        <<"minimum">> => -2.5,
        <<"exclusiveMinimum">> => false,
        <<"maximum">> => 2.5,
        <<"exclusiveMaximum">> => true
    },
    ?FORALL(
        Value,
        ndto_pbt:dto(Schema),
        Value > -2.5 andalso Value =< 2.5
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
        string:length(Value) =:= 4
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
