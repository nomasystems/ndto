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
        additional_properties,
        {group, string_formats}
    ].

groups() ->
    [
        {types, [parallel], [
            any,
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

ref(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_ref(),
        Conf
    ).

enum(Conf) ->
    % dbg:tpl(ndto_generator, is_valid, 2),
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
                <<"type">> => Type
            },
            DTO1 = ndto:generate(test_nullable1, Schema1),
            ok = ndto:load(DTO1),

            false = test_nullable1:is_valid(undefined),

            Schema2 = #{
                <<"type">> => Type,
                <<"nullable">> => true
            },
            DTO2 = ndto:generate(test_nullable2, Schema2),
            ok = ndto:load(DTO2),

            true = test_nullable2:is_valid(undefined)
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

    false = test_one_of:is_valid(<<"0">>),
    false = test_one_of:is_valid(0),
    true = test_one_of:is_valid(0.0).

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

    false = test_any_of:is_valid(<<"0">>),
    true = test_any_of:is_valid(0),
    true = test_any_of:is_valid(0.0).

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

    false = test_all_of:is_valid(<<"1">>),
    false = test_all_of:is_valid(0),
    false = test_all_of:is_valid(1.0),
    true = test_all_of:is_valid(1).

'not'(_Conf) ->
    Schema = #{
        <<"not">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 0}
    },
    DTO = ndto:generate(test_not, Schema),
    ok = ndto:load(DTO),

    false = test_not:is_valid(0),
    true = test_not:is_valid(<<"0">>),
    true = test_not:is_valid(-1).

pattern(_Conf) ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"pattern">> => <<"[a-z]+@[a-z]+\.[a-z]+">>
    },
    DTO = ndto:generate(test_pattern, Schema),
    ok = ndto:load(DTO),

    true = test_pattern:is_valid(<<"test@ndto.erl">>).

additional_properties(_Conf) ->
    Schema1 = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"foo">> => #{}},
        <<"additionalProperties">> => false
    },
    DTO1 = ndto:generate(test_additional_properties1, Schema1),
    ok = ndto:load(DTO1),

    true = test_additional_properties1:is_valid(#{<<"foo">> => <<"bar">>}),
    false = test_additional_properties1:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>}),

    Schema2 = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"foo">> => #{}},
        <<"additionalProperties">> => true
    },
    DTO2 = ndto:generate(test_additional_properties2, Schema2),
    ok = ndto:load(DTO2),

    true = test_additional_properties2:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>}),
    true = test_additional_properties2:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"corge">>}),

    Schema3 = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"foo">> => #{}},
        <<"additionalProperties">> => #{<<"type">> => <<"boolean">>}
    },
    DTO3 = ndto:generate(test_additional_properties3, Schema3),
    ok = ndto:load(DTO3),

    true = test_additional_properties3:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => true}),
    false = test_additional_properties3:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>}).

unique_items(_Conf) ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"uniqueItems">> => true
    },
    DTO = ndto:generate(test_unique_items, Schema),
    ok = ndto:load(DTO),

    true = test_unique_items:is_valid([0, 1.0, true, <<"string">>, [], #{<<"key">> => <<"value">>}]).

iso8601(_Conf) ->
    String = ncalendar:now(iso8601),
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"iso8601-datetime">>
    },
    DTO = ndto:generate(test_iso8601, Schema),
    ok = ndto:load(DTO),

    true = test_iso8601:is_valid(String).

base64(_Conf) ->
    String = base64:encode(<<"this is a test">>),
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"base64">>
    },
    DTO = ndto:generate(test_base64, Schema),
    ok = ndto:load(DTO),

    true = test_base64:is_valid(String).
