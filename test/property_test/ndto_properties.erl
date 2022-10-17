%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
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
            true = ndto_test_util:is_valid(<<"test_any">>, Schema, Any),
            true
        end
    ).

prop_ref() ->
    ?FORALL(
        Any,
        ndto_dom:any_value(),
        begin
            ReferencedName = <<"referenced_dto">>,
            ReferencedSchema = #{},
            Name = <<"ref_dto">>,
            Schema = #{
                <<"$ref">> => <<"/components/schemas/", ReferencedName/binary>>
            },
            ndto_test_util:compile(ReferencedName, ReferencedSchema),
            true = ndto_test_util:is_valid(Name, Schema, Any),
            true
        end
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
            true = ndto_test_util:is_valid(<<"test_string">>, Schema, String),
            true
        end
    ).

prop_number() ->
    ?FORALL(
        RawNumber,
        ndto_dom:number_value(),
        begin
            Number = RawNumber * 2,
            Schema = #{
                <<"type">> => <<"number">>,
                <<"multipleOf">> => 2,
                <<"minimum">> => Number,
                <<"exclusiveMinimum">> => Number - 1,
                <<"maximum">> => Number,
                <<"exclusiveMaximum">> => Number + 1
            },
            true = ndto_test_util:is_valid(<<"test_number">>, Schema, Number),
            true
        end
    ).

prop_integer() ->
    ?FORALL(
        RawInteger,
        ndto_dom:integer_value(),
        begin
            Integer = RawInteger * 2,
            Schema = #{
                <<"type">> => <<"integer">>,
                <<"multipleOf">> => 2,
                <<"minimum">> => Integer,
                <<"exclusiveMinimum">> => Integer - 1,
                <<"maximum">> => Integer,
                <<"exclusiveMaximum">> => Integer + 1
            },
            true = ndto_test_util:is_valid(<<"test_integer">>, Schema, Integer),
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
            true = ndto_test_util:is_valid(<<"test_boolean">>, Schema, Boolean),
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
                    <<"type">> => Type
                },
                <<"contains">> => #{
                    <<"type">> => Type
                },
                <<"minItems">> => erlang:length(Array),
                <<"maxItems">> => erlang:length(Array)
            },
            true = ndto_test_util:is_valid(<<"test_array">>, Schema, Array),
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
                <<"required">> => maps:keys(Object)
            },
            true = ndto_test_util:is_valid(<<"test_object">>, Schema, Object),
            true
        end
    ).
