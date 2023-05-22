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
-module(ndto_schema_dom).

%%% GENERATORS
-export([
    schema/0,
    any/0,
    string/0,
    number/0,
    integer/0,
    boolean/0,
    array/0,
    object/0
]).

%%% MACROS
-define(DEPTH_BOUND, 3).

%%%-----------------------------------------------------------------------------
%%% GENERATORS
%%%-----------------------------------------------------------------------------
schema() ->
    schema(?DEPTH_BOUND).

schema(0) ->
    triq_dom:oneof([
        any(),
        string(),
        number(),
        integer(),
        boolean()
    ]);
schema(Depth) ->
    triq_dom:oneof([
        any(),
        string(),
        number(),
        integer(),
        boolean(),
        array(Depth),
        object(Depth)
    ]).

any() ->
    triq_dom:return(#{}).

string() ->
    triq_dom:bind(
        triq_dom:int(0, 50),
        fun(MaxLength) ->
            triq_dom:bind(
                triq_dom:int(0, MaxLength),
                fun(MinLength) ->
                    #{
                        <<"type">> => <<"string">>,
                        <<"minLength">> => MinLength,
                        <<"maxLength">> => MaxLength
                    }
                end
            )
        end
    ).

number() ->
    triq_dom:bind(
        {
            triq_dom:oneof([
                triq_dom:int(),
                triq_dom:real()
            ]),
            triq_dom:oneof([
                triq_dom:pos_integer(),
                triq_dom:bind(
                    triq_dom:real(),
                    fun(Float) -> erlang:abs(Float) end
                )
            ]),
            triq_dom:bool(),
            triq_dom:bool()
        },
        fun({Min, Offset, ExclusiveMinimum, ExclusiveMaximum}) ->
            Max = Min + Offset,
            #{
                <<"type">> => <<"number">>,
                <<"minimum">> => Min,
                <<"exclusiveMinimum">> => ExclusiveMinimum,
                <<"maximum">> => Max,
                <<"exclusiveMaximum">> => ExclusiveMaximum
            }
        end
    ).

integer() ->
    triq_dom:bind(
        {triq_dom:int(), triq_dom:pos_integer(), triq_dom:bool(), triq_dom:bool()},
        fun({Min, Offset, ExclusiveMinimum, ExclusiveMaximum}) ->
            Max = Min + Offset,
            triq_dom:bind(
                triq_dom:int(Min, Max),
                fun(MultipleOf) ->
                    #{
                        <<"type">> => <<"integer">>,
                        <<"minimum">> => Min,
                        <<"exclusiveMinimum">> => ExclusiveMinimum,
                        <<"maximum">> => Max,
                        <<"exclusiveMaximum">> => ExclusiveMaximum,
                        <<"multipleOf">> => MultipleOf
                    }
                end
            )
        end
    ).

boolean() ->
    triq_dom:return(#{<<"type">> => <<"boolean">>}).

array() ->
    array(?DEPTH_BOUND).

array(Depth) ->
    triq_dom:bind(
        {schema(Depth - 1), triq_dom:int(), triq_dom:pos_integer(), triq_dom:bool()},
        fun({Schema, MinItems, Offset, UniqueItems}) ->
            #{
                <<"type">> => <<"array">>,
                <<"items">> => Schema,
                <<"minItems">> => MinItems,
                <<"maxItems">> => MinItems + Offset,
                <<"uniqueItems">> => UniqueItems
            }
        end
    ).

object() ->
    object(?DEPTH_BOUND).

object(Depth) ->
    triq_dom:bind(
        {
            triq_dom:list({triq_dom:unicode_binary(10), schema(Depth - 1)}),
            triq_dom:oneof([triq_dom:bool(), schema(Depth - 1)])
        },
        fun({PropertyList, AdditionalProperties}) ->
            case PropertyList of
                [] ->
                    #{
                        <<"type">> => <<"object">>,
                        <<"additionalProperties">> => AdditionalProperties
                    };
                _PL ->
                    PropertyNames = proplists:get_keys(PropertyList),
                    Properties = maps:from_list(PropertyList),
                    PropertiesNum = erlang:length(PropertyList),
                    triq_dom:bind(
                        {
                            triq_dom:list(triq_dom:elements(PropertyNames)),
                            triq_dom:int(0, PropertiesNum)
                        },
                        fun({Required, MaxProperties}) ->
                            triq_dom:bind(
                                triq_dom:int(0, MaxProperties),
                                fun(MinProperties) ->
                                    #{
                                        <<"type">> => <<"object">>,
                                        <<"properties">> => Properties,
                                        <<"required">> => Required,
                                        <<"minProperties">> => MinProperties,
                                        <<"maxProperties">> => MaxProperties,
                                        <<"additionalProperties">> => AdditionalProperties
                                    }
                                end
                            )
                        end
                    )
            end
        end
    ).
