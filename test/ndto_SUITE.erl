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
        enum,
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
            Schema2 = #{
                <<"type">> => Type,
                <<"nullable">> => true
            },
            false = ndto_test_util:is_valid(<<"test_nullable1">>, Schema1, undefined),
            true = ndto_test_util:is_valid(<<"test_nullable2">>, Schema2, undefined),
            true
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
    false = ndto_test_util:is_valid(<<"test_oneOf">>, Schema, <<"0">>),
    false = ndto_test_util:is_valid(<<"test_oneOf">>, Schema, 0),
    true = ndto_test_util:is_valid(<<"test_oneOf">>, Schema, 0.0).

anyOf(_Conf) ->
    Schema = #{
        <<"anyOf">> => [
            #{<<"type">> => <<"integer">>, <<"minimum">> => 0},
            #{<<"type">> => <<"integer">>, <<"minimum">> => 1},
            #{<<"type">> => <<"number">>, <<"minimum">> => 0}
        ]
    },
    false = ndto_test_util:is_valid(<<"test_anyOf">>, Schema, <<"0">>),
    true = ndto_test_util:is_valid(<<"test_anyOf">>, Schema, 0),
    true = ndto_test_util:is_valid(<<"test_anyOf">>, Schema, 0.0).

allOf(_Conf) ->
    Schema = #{
        <<"allOf">> => [
            #{<<"type">> => <<"integer">>, <<"minimum">> => 0},
            #{<<"type">> => <<"integer">>, <<"minimum">> => 1},
            #{<<"type">> => <<"number">>, <<"minimum">> => 0}
        ]
    },
    false = ndto_test_util:is_valid(<<"test_allOf">>, Schema, <<"1">>),
    false = ndto_test_util:is_valid(<<"test_allOf">>, Schema, 0),
    false = ndto_test_util:is_valid(<<"test_allOf">>, Schema, 1.0),
    true = ndto_test_util:is_valid(<<"test_allOf">>, Schema, 1).

'not'(_Conf) ->
    Schema = #{
        <<"not">> => #{<<"type">> => <<"integer">>, <<"minimum">> => 0}
    },
    false = ndto_test_util:is_valid(<<"test_not">>, Schema, 0),
    true = ndto_test_util:is_valid(<<"test_not">>, Schema, <<"0">>),
    true = ndto_test_util:is_valid(<<"test_not">>, Schema, -1).

enum(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_enum(),
        Conf
    ).

pattern(_Conf) ->
    Schema = #{
        <<"type">> => <<"string">>,
        <<"pattern">> => <<"[a-z]+@[a-z]+\.[a-z]+">>
    },
    true = ndto_test_util:is_valid(<<"test_base64">>, Schema, <<"test@ndto.erl">>).

additional_properties(_Conf) ->
    Schema1 = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"foo">> => #{}},
        <<"additionalProperties">> => false
    },
    true = ndto_test_util:is_valid(<<"test_additional_properties1">>, Schema1, #{
        <<"foo">> => <<"bar">>
    }),
    false = ndto_test_util:is_valid(<<"test_additional_properties1">>, Schema1, #{
        <<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>
    }),
    Schema2 = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"foo">> => #{}},
        <<"additionalProperties">> => true
    },
    true = ndto_test_util:is_valid(<<"test_additional_properties2">>, Schema2, #{
        <<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>
    }),
    true = ndto_test_util:is_valid(<<"test_additional_properties2">>, Schema2, #{
        <<"foo">> => <<"bar">>, <<"baz">> => <<"corge">>
    }),
    Schema3 = #{
        <<"type">> => <<"object">>,
        <<"properties">> => #{<<"foo">> => #{}},
        <<"additionalProperties">> => #{<<"type">> => <<"boolean">>}
    },
    true = ndto_test_util:is_valid(<<"test_additional_properties3">>, Schema3, #{
        <<"foo">> => <<"bar">>, <<"baz">> => true
    }),
    false = ndto_test_util:is_valid(<<"test_additional_properties3">>, Schema3, #{
        <<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>
    }).

unique_items(_Conf) ->
    Schema = #{
        <<"type">> => <<"array">>,
        <<"uniqueItems">> => true
    },
    true = ndto_test_util:is_valid(<<"test_unique_items">>, Schema, [
        0, 1.0, true, <<"string">>, [], #{<<"key">> => <<"value">>}
    ]).

iso8601(_Conf) ->
    String = ncalendar:now(iso8601),
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"iso8601-datetime">>
    },
    true = ndto_test_util:is_valid(<<"test_iso8601">>, Schema, String).

base64(_Conf) ->
    String = base64:encode(<<"this is a test">>),
    Schema = #{
        <<"type">> => <<"string">>,
        <<"format">> => <<"base64">>
    },
    true = ndto_test_util:is_valid(<<"test_base64">>, Schema, String).
