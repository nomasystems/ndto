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
        required,
        {group, string_formats},
        {group, examples}
    ].

groups() ->
    [
        {types, [parallel], [
            any,
            ref,
            enum,
            string,
            float,
            integer,
            boolean,
            array,
            object
        ]},
        {subschemas, [parallel], [
            one_of,
            any_of,
            all_of,
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

float(Conf) ->
    ct_property_test:quickcheck(
        ndto_properties:prop_float(),
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
                type => Type,
                nullable => true
            },
            DTO1 = ndto:generate(test_nullable1, Schema1),
            ok = ndto:load(DTO1),

            ?assertEqual(true, test_nullable1:is_valid(null)),

            Schema2 = #{
                type => Type
            },
            DTO2 = ndto:generate(test_nullable2, Schema2),
            ok = ndto:load(DTO2),

            <<Char:1/binary, _Rest/binary>> = TypeBin = erlang:atom_to_binary(Type),
            Article =
                case Char of
                    Vocal when Vocal =:= <<"a">>; Vocal =:= <<"o">>; Vocal =:= <<"i">> ->
                        <<"an">>;
                    _ ->
                        <<"a">>
                end,

            ?assertEqual(
                {false, {'$.type', <<"Value is not ", Article/binary, " ", TypeBin/binary>>}},
                test_nullable2:is_valid(null)
            )
        end,
        ndto_dom:types()
    ).

one_of(_Conf) ->
    Schema = #{
        one_of => [
            #{type => integer, minimum => 0},
            #{type => integer, minimum => 1},
            #{type => float, minimum => 0}
        ]
    },
    DTO = ndto:generate(test_one_of, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(
        {false, {'$.one_of', <<"Value is not matching exactly one condition. None matched.">>}},
        test_one_of:is_valid(<<"0">>)
    ),
    ?assertEqual(
        {false,
            {'$.one_of',
                <<"Value is not matching exactly one condition. More than one (conditions 0 and 1) matched.">>}},
        test_one_of:is_valid(1)
    ),
    ?assertEqual(true, test_one_of:is_valid(0.0)).

any_of(_Conf) ->
    Schema = #{
        any_of => [
            #{type => integer, minimum => 0},
            #{type => integer, minimum => 1},
            #{type => float, minimum => 0}
        ]
    },
    DTO = ndto:generate(test_any_of, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(
        {false, {'$.any_of', <<"Value is not matching at least one condition. None matched.">>}},
        test_any_of:is_valid(<<"0">>)
    ),
    ?assertEqual(true, test_any_of:is_valid(0)),
    ?assertEqual(true, test_any_of:is_valid(0.0)).

all_of(_Conf) ->
    Schema = #{
        all_of => [
            #{type => integer, minimum => 0},
            #{type => integer, minimum => 1}
        ]
    },
    DTO = ndto:generate(test_all_of, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(
        {false,
            {'$.all_of',
                <<"Value is not matching all conditions. Condition 1 failed because of schema path '$.all_of[1].type' : Value is not an integer">>}},
        test_all_of:is_valid(<<"1">>)
    ),
    ?assertEqual(
        {false,
            {'$.all_of',
                <<"Value is not matching all conditions. Condition 1 failed because of schema path '$.all_of[1].minimum' : Value is not a number greater or equal to 1">>}},
        test_all_of:is_valid(0)
    ),
    ?assertEqual(
        {false,
            {'$.all_of',
                <<"Value is not matching all conditions. Condition 1 failed because of schema path '$.all_of[1].type' : Value is not an integer">>}},
        test_all_of:is_valid(1.0)
    ),
    ?assertEqual(true, test_all_of:is_valid(1)).

'not'(_Conf) ->
    Schema = #{
        'not' => #{type => integer, minimum => 0}
    },
    DTO = ndto:generate(test_not, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(false, test_not:is_valid(0)),
    ?assertEqual(true, test_not:is_valid(<<"0">>)),
    ?assertEqual(true, test_not:is_valid(-1)).

pattern(_Conf) ->
    Schema = #{
        type => string,
        pattern => <<"[a-z]+@[a-z]+\.[a-z]+">>
    },
    DTO = ndto:generate(test_pattern, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(true, test_pattern:is_valid(<<"test@ndto.erl">>)).

pattern_properties(_Conf) ->
    Schema = #{
        type => object,
        pattern_properties => #{
            <<"[a-z]+">> => #{type => string}
        }
    },
    DTO = ndto:generate(test_pattern_properties, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(
        {false,
            {'$.pattern_properties.[a-z]+.type',
                <<"Property \"foo\" failed validation: Value is not a string">>}},
        test_pattern_properties:is_valid(#{<<"foo">> => 0})
    ),
    ?assertEqual(true, test_pattern_properties:is_valid(#{<<"foo">> => <<"bar">>})),
    ?assertEqual(true, test_pattern_properties:is_valid(#{<<"0">> => <<"foo">>})).

additional_properties(_Conf) ->
    Schema1 = #{
        type => object,
        properties => #{<<"foo">> => #{}},
        pattern_properties => #{<<"[a-z]+">> => #{type => string}},
        additional_properties => false
    },
    DTO1 = ndto:generate(test_additional_properties1, Schema1),
    ok = ndto:load(DTO1),

    ?assertEqual(true, test_additional_properties1:is_valid(#{<<"foo">> => <<"bar">>})),
    ?assertEqual(
        true,
        test_additional_properties1:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>})
    ),
    ?assertEqual(
        {false,
            {'$.pattern_properties.[a-z]+.type',
                <<"Property \"foobar\" failed validation: Value is not a string">>}},
        test_additional_properties1:is_valid(#{
            <<"foo">> => <<"bar">>, <<"baz">> => <<"qux">>, <<"foobar">> => 0
        })
    ),

    Schema2 = #{
        type => <<"object">>,
        properties => #{<<"foo">> => #{}},
        pattern_properties => #{<<"[a-z]+">> => #{type => string}},
        additional_properties => true
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
        type => object,
        properties => #{<<"foo">> => #{}},
        pattern_properties => #{<<"[a-z]+">> => #{type => string}},
        additional_properties => #{type => boolean}
    },
    DTO3 = ndto:generate(test_additional_properties3, Schema3),
    ok = ndto:load(DTO3),

    ?assertEqual(
        {false,
            {'$.pattern_properties.[a-z]+.type',
                <<"Property \"baz\" failed validation: Value is not a string">>}},
        test_additional_properties3:is_valid(#{<<"foo">> => <<"bar">>, <<"baz">> => true})
    ),
    ?assertEqual(
        {false,
            {'$.additional_properties.type',
                <<"Property \"1\" failed validation: Value is not a boolean">>}},
        test_additional_properties3:is_valid(#{<<"foo">> => <<"bar">>, <<"1">> => <<"baz">>})
    ),
    ?assertEqual(
        true, test_additional_properties3:is_valid(#{<<"foo">> => <<"bar">>, <<"1">> => true})
    ),

    Schema4 = #{
        type => object,
        pattern_properties => #{<<"^[A-Z]+$">> => true},
        additional_properties => false
    },
    DTO4 = ndto:generate(test_additional_properties4, Schema4),
    ok = ndto:load(DTO4),

    ?assertEqual(
        true, test_additional_properties4:is_valid(#{<<"FOO">> => true, <<"BAR">> => 1})
    ),

    ?assertEqual(
        {false, {'$.additional_properties', <<"Object has unsupported keys: \"Foo\"">>}},
        test_additional_properties4:is_valid(#{<<"Foo">> => true, <<"BAR">> => 1})
    ).

required(_Conf) ->
    Schema = #{
        type => object,
        properties => #{
            <<"foo">> => #{
                type => string
            },
            <<"bar">> => #{
                type => integer
            }
        },
        required => [<<"foo">>]
    },
    DTO = ndto:generate(test_required, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(true, test_required:is_valid(#{<<"foo">> => <<"foobar">>})),
    ok.

unique_items(_Conf) ->
    Schema = #{
        type => array,
        unique_items => true
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
        type => string,
        format => iso8601
    },
    DTO = ndto:generate(test_iso8601, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(true, test_iso8601:is_valid(String)).

base64(_Conf) ->
    String = base64:encode(<<"this is a test">>),
    Schema = #{
        type => string,
        format => base64
    },
    DTO = ndto:generate(test_base64, Schema),
    ok = ndto:load(DTO),

    ?assertEqual(true, test_base64:is_valid(String)).

petstore(_Conf) ->
    SpecPath = erlang:list_to_binary(
        filename:join(
            code:lib_dir(ndto, priv),
            "oas/3.0/specs/oas_3_0.json"
        )
    ),
    {ok, [{PetstoreDTO, _Schema} | _Rest] = Schemas} = ndto_parser:parse(
        ndto_parser_json_schema,
        SpecPath
    ),
    lists:foreach(
        fun({SchemaName, Schema}) ->
            DTO = ndto:generate(SchemaName, Schema),
            ok = ndto:load(DTO)
        end,
        Schemas
    ),

    {ok, PetstoreBin} = file:read_file(
        erlang:list_to_binary(
            filename:join(
                code:lib_dir(ndto, priv),
                "oas/3.0/examples/petstore.json"
            )
        )
    ),
    {ok, Petstore} = njson:decode(PetstoreBin),

    ?assertEqual(
        true,
        PetstoreDTO:is_valid(Petstore)
    ).
