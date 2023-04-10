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
-module(ndto_properties).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%%-----------------------------------------------------------------------------
%%% PROPERTIES
%%%-----------------------------------------------------------------------------
prop_any() ->
    ?FORALL(
        Any,
        ndto_dom:any_value(),
        begin
            Schema = #{},
            DTO = ndto:generate(test_any, Schema),
            ok = ndto:load(DTO),

            true = test_any:is_valid(Any),
            true
        end
    ).

prop_ref() ->
    ?FORALL(
        Any,
        ndto_dom:any_value(),
        begin
            ReferencedName = test_referenced_dto,
            ReferencedSchema = #{},
            Schema = #{
                <<"$ref">> => <<"/components/schemas/", (erlang:atom_to_binary(ReferencedName))/binary>>
            },

            ReferencedDTO = ndto:generate(ReferencedName, ReferencedSchema),
            ok = ndto:load(ReferencedDTO),
            DTO = ndto:generate(test_ref, Schema),
            ok = ndto:load(DTO),

            true = test_ref:is_valid(Any),
            true
        end
    ).

prop_enum() ->
    triq:numtests(
        25,
        ?FORALL(
            Enum,
            triq_dom:list(ndto_dom:array_value()),
            begin
                Schema = #{<<"enum">> => Enum},
                DTO = ndto:generate(test_enum, Schema),
                ok = ndto:load(DTO),
    
                true = lists:all(
                    fun test_enum:is_valid/1,
                    Enum
                ),
                true
            end
        )
    ).

prop_string() ->
    ?FORALL(
        String,
        ndto_dom:string_value(),
        begin
            Schema = #{
                <<"type">> => <<"string">>,
                <<"minLength">> => string:length(String),
                <<"maxLength">> => string:length(String),
                <<"pattern">> => <<".*">>
            },
            DTO = ndto:generate(test_string, Schema),
            ok = ndto:load(DTO),

            true = test_string:is_valid(String),
            true
        end
    ).

prop_number() ->
    ?FORALL(
        Number,
        ndto_dom:number_value(),
        begin
            Schema = #{
                <<"type">> => <<"number">>,
                <<"minimum">> => Number,
                <<"exclusiveMinimum">> => false,
                <<"maximum">> => Number + 1,
                <<"exclusiveMaximum">> => true
            },
            DTO = ndto:generate(test_number, Schema),
            ok = ndto:load(DTO),

            true = test_number:is_valid(Number),
            true
        end
    ).

prop_integer() ->
    ?FORALL(
        {RawInteger, MultipleOf},
        ?SUCHTHAT(
            {N, M},
            {ndto_dom:integer_value(), ndto_dom:integer_value()},
            N > 0 andalso M > 0 andalso N > M
        ),
        begin
            Integer = RawInteger * MultipleOf,
            Schema = #{
                <<"type">> => <<"integer">>,
                <<"minimum">> => Integer - 1,
                <<"exclusiveMinimum">> => true,
                <<"maximum">> => Integer,
                <<"exclusiveMaximum">> => false,
                <<"multipleOf">> => MultipleOf
            },
            DTO = ndto:generate(test_integer, Schema),
            ok = ndto:load(DTO),

            true = test_integer:is_valid(Integer),
            true
        end
    ).

prop_boolean() ->
    ?FORALL(
        Boolean,
        ndto_dom:boolean_value(),
        begin
            Schema = #{
                <<"type">> => <<"boolean">>
            },
            DTO = ndto:generate(test_boolean, Schema),
            ok = ndto:load(DTO),

            true = test_boolean:is_valid(Boolean),
            true
        end
    ).

prop_array() ->
    ?FORALL(
        {Array, Type},
        ?LET(
            Type,
            triq_dom:elements(ndto_dom:types()),
            {ndto_dom:array_value(Type), Type}
        ),
        begin
            Schema = #{
                <<"type">> => <<"array">>,
                <<"items">> => #{
                    <<"type">> => Type,
                    <<"nullable">> => false
                },
                <<"minItems">> => erlang:length(Array),
                <<"maxItems">> => erlang:length(Array),
                <<"uniqueItems">> => false
            },
            DTO = ndto:generate(test_array, Schema),
            ok = ndto:load(DTO),

            true = test_array:is_valid(Array),
            true
        end
    ).

prop_object() ->
    ?FORALL(
        Object,
        ndto_dom:object_value(),
        begin
            Schema = #{
                <<"type">> => <<"object">>,
                <<"properties">> => maps:fold(
                    fun(Key, _Value, Acc) ->
                        maps:put(Key, #{}, Acc)
                    end,
                    #{},
                    Object
                ),
                <<"required">> => maps:keys(Object),
                <<"minProperties">> => erlang:length(maps:keys(Object)) - 1,
                <<"maxProperties">> => erlang:length(maps:keys(Object)) + 1
            },
            DTO = ndto:generate(test_object, Schema),
            ok = ndto:load(DTO),

            true = test_object:is_valid(Object),
            true
        end
    ).
