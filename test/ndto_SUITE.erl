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
-module(ndto_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        {group, types},
        {group, subschemas},
        nullable,
        pattern,
        unique_items,
        pattern_properties,
        additional_properties,
        {group, string_formats},
        {group, examples}
    ].

groups() ->
    [
        {types, [parallel], [
            any,
            undefined,
            ref,
            enum,
            string,
            number,
            integer,
            boolean,
            array,
            object
        ]},
        {subschemas, [parallel], [
            oneOf,
            anyOf,
            allOf,
            'not'
        ]},
        {string_formats, [parallel], [
            iso8601,
            base64
        ]},
        {examples, [parallel], [
            petstore
        ]}
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    Config = nct_util:setup_suite(Conf),
    ct_property_test:init_per_suite(Config).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
any(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_any(),
        Conf
    ).

undefined(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_undefined(),
        Conf
    ).

ref(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_ref(),
        Conf
    ).

enum(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_enum(),
        Conf
    ).

string(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_string(),
        Conf
    ).

number(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_number(),
        Conf
    ).

integer(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_integer(),
        Conf
    ).

boolean(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_boolean(),
        Conf
    ).

array(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_array(),
        Conf
    ).

object(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_object(),
        Conf
    ).

nullable(_Conf) ->
    lists:foreach(
        fun(Type) ->
            Schema1 = #{
                <<"type">> => Type,
                <<"nullable">> => true
            },
            DTO1 = ndto:generate(test_nullable1, Schema1),
            ok = ndto:load(DTO1),

            ?assertEqual(true, test_nullable1:is_valid(undefined)),

            Schema2 = #{
                <<"type">> => Type
            },
            DTO2 = ndto:generate(test_nullable2, Schema2),
            ok = ndto:load(DTO2),

            ?assertEqual(false, test_nullable2:is_valid(undefined))
        end,
        ndto_dom:types()
    ).

oneOf(_Conf) ->
    Schema = #{
        <<"oneOf">> => [
            #{<<"type">> => <<"integer">>, <<"minimum">> => 0},
            #{<<"type">> => <<"integer">>, <<"minimum">> => 1},
            #{<<"type">> => <<"number">>, <<"minimum">> => 0}
        ]
    },
    DTO = ndto:generate(test_one_of, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(false, test_one_of:is_valid(<<"0">>)),
    ?assertEqual(false, test_one_of:is_valid(0)),
    ?assertEqual(true, test_one_of:is_valid(0.0)).

anyOf(_Conf) ->
    Schema = #{
        <<"anyOf">> => [
            #{<<"type">> => <<"integer">>, <<"minimum">> => 0},
            #{<<"type">> => <<"integer">>, <<"minimum">> => 1},
            #{<<"type">> => <<"number">>, <<"minimum">> => 0}
        ]
    },
    DTO = ndto:generate(test_any_of, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(false, test_any_of:is_valid(<<"0">>)),
    ?assertEqual(true, test_any_of:is_valid(0)),
    ?assertEqual(true, test_any_of:is_valid(0.0)).

allOf(_Conf) ->
    Schema = #{
        <<"allOf">> => [
            #{<<"type">> => <<"integer">>, <<"minimum">> => 0},
            #{<<"type">> => <<"integer">>, <<"minimum">> => 1},
            #{<<"type">> => <<"number">>, <<"minimum">> => 0}
        ]
    },
    DTO = ndto:generate(test_all_of, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(false, test_all_of:is_valid(<<"1">>)),
    ?assertEqual(false, test_all_of:is_valid(0)),
    ?assertEqual(false, test_all_of:is_valid(1.0)),
    ?assertEqual(true, test_all_of:is_valid(1)).

'not'(_Conf) ->
    Schema = #{
        <<"not">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 0}
    },
    DTO = ndto:generate(test_not, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(false, test_not:is_valid(0)),
    ?assertEqual(true, test_not:is_valid(<<"0">>)),
    ?assertEqual(true, test_not:is_valid(-1)).

pattern(_Conf) ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"pattern">> => <<"[a-z]+@[a-z]+\.[a-z]+">>
    },
    DTO = ndto:generate(test_pattern, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(true, test_pattern:is_valid(<<"test@ndto.erl">>)).

pattern_properties(_Conf) ->
    Schema = #{
        <<"type">> => <<"object">>,
        <<"patternProperties">> => #{
            <<"[a-z]+">> => #{<<"type">> => <<"string">>}
        }
    },
    DTO = ndto:generate(test_pattern_properties, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(false, test_pattern_properties:is_valid(#{<<"foo">> => 0})),
    ?assertEqual(true, test_pattern_properties:is_valid(#{<<"foo">> => <<"bar">>})),
    ?assertEqual(true, test_pattern_properties:is_valid(#{<<"0">> => <<"foo">>})).

additional_properties(_Conf) ->
    Schema1 = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"foo">> => #{}},
        <<"patternProperties">> => #{<<"[a-z]+">> => #{<<"type">> => <<"string">>}},
        <<"additionalProperties">> => false
    },
    DTO1 = ndto:generate(test_additional_properties1, Schema1),
    ok = ndto:load(DTO1),

    ?assertEqual(true, test_additional_properties1:is_valid(#{<<"foo">> => <<"bar">>})),
    ?assertEqual(
        true,
        test_additional_properties1:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>})
    ),
    ?assertEqual(
        false,
        test_additional_properties1:is_valid(#{
            <<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>, <<"foobar">> => 0
        })
    ),

    Schema2 = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"foo">> => #{}},
        <<"patternProperties">> => #{<<"[a-z]+">> => #{<<"type">> => <<"string">>}},
        <<"additionalProperties">> => true
    },
    DTO2 = ndto:generate(test_additional_properties2, Schema2),
    ok = ndto:load(DTO2),

    ?assertEqual(
        true,
        test_additional_properties2:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>})
    ),
    ?assertEqual(
        true, test_additional_properties2:is_valid(#{<<"foo">> => <<"bar">>, <<"0">> => [1]})
    ),

    Schema3 = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"foo">> => #{}},
        <<"patternProperties">> => #{<<"[a-z]+">> => #{<<"type">> => <<"string">>}},
        <<"additionalProperties">> => #{<<"type">> => <<"boolean">>}
    },
    DTO3 = ndto:generate(test_additional_properties3, Schema3),
    ok = ndto:load(DTO3),

    ?assertEqual(
        false, test_additional_properties3:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => true})
    ),
    ?assertEqual(
        false, test_additional_properties3:is_valid(#{<<"foo">> => <<"bar">>, <<"1">> => <<"baz">>})
    ),
    ?assertEqual(
        true, test_additional_properties3:is_valid(#{<<"foo">> => <<"bar">>, <<"1">> => true})
    ).

unique_items(_Conf) ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"uniqueItems">> => true
    },
    DTO = ndto:generate(test_unique_items, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(
        true,
        test_unique_items:is_valid([0, 1.0, true, <<"string">>, [], #{<<"key">> => <<"value">>}])
    ).

iso8601(_Conf) ->
    String = ncalendar:now(iso8601),
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"iso8601-datetime">>
    },
    DTO = ndto:generate(test_iso8601, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(true, test_iso8601:is_valid(String)).

base64(_Conf) ->
    String = base64:encode(<<"this is a test">>),
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"base64">>
    },
    DTO = ndto:generate(test_base64, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(true, test_base64:is_valid(String)).

petstore(_Conf) ->
    Schema = ndto_parser:parse(
        ndto_parser_json_schema_draft_04,
        test_oas_3_0,
        erlang:list_to_binary(code:lib_dir(ndto, priv) ++ "/oas/3.0/specs/oas_3_0.json")
    ),
    DTO = ndto:generate(test_oas_3_0, Schema),
    ok = ndto:load(DTO),

    {ok, PetstoreBin} = file:read_file(
        erlang:list_to_binary(code:lib_dir(ndto, priv) ++ "/oas/3.0/examples/petstore.json")
    ),
    Petstore = njson:decode(PetstoreBin),

    ?assertEqual(
        true,
        test_oas_3_0:is_valid(Petstore)
    ).
