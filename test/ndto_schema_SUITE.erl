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
-module(ndto_schema_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%% MACROS
-define(ENUM_SCHEMA, #{<<"enum">> => [1, <<"string">>, true]}).
-define(BOOLEAN_SCHEMA, #{<<"type">> => <<"boolean">>}).
-define(INTEGER_SCHEMA, #{
    <<"type">> => <<"integer">>,
    <<"minimum">> => 2,
    <<"exclusiveMinimum">> => true,
    <<"maximum">> => 6
}).
-define(NUMBER_SCHEMA, #{
    <<"type">> => <<"number">>,
    <<"minimum">> => 4,
    <<"exclusiveMinimum">> => true,
    <<"maximum">> => 8,
    <<"exclusiveMaximum">> => true
}).
-define(STRING_SCHEMA, #{
    <<"type">> => <<"string">>,
    <<"minLength">> => 3,
    <<"maxLength">> => 6,
    <<"pattern">> => <<"a{3}">>
}).
-define(ARRAY_SCHEMA, #{
    <<"type">> => <<"array">>,
    <<"items">> => ?NUMBER_SCHEMA,
    <<"minItems">> => 1,
    <<"maxItems">> => 3
}).
-define(OBJECT_SCHEMA, #{
    <<"type">> => <<"object">>,
    <<"properties">> => #{
        <<"foo">> => ?INTEGER_SCHEMA,
        <<"bar">> => ?STRING_SCHEMA
    },
    <<"minProperties">> => 3,
    <<"additionalProperties">> => true
}).
-define(INTERSECTION_SCHEMA, #{<<"allOf">> => [?INTEGER_SCHEMA, ?NUMBER_SCHEMA]}).
-define(UNION_SCHEMA, #{<<"anyOf">> => [?BOOLEAN_SCHEMA, ?STRING_SCHEMA]}).
-define(SYMMETRIC_DIFFERENCE_SCHEMA, #{<<"oneOf">> => [?INTEGER_SCHEMA, ?NUMBER_SCHEMA]}).
-define(COMPLEMENT_SCHEMA, #{<<"not">> => ?BOOLEAN_SCHEMA}).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        {group, properties},
        complement,
        intersection,
        union
    ].

groups() ->
    [
        {properties, [parallel], [
            prop_conmutative_intersection,
            prop_identity,
            prop_idempotent,
            prop_domination,
            prop_empty_schema_complement,
            prop_universal_schema_complement
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
prop_conmutative_intersection(Conf) ->
    ct_property_test:quickcheck(
        ndto_schema_properties:prop_conmutative_intersection(),
        Conf
    ).

prop_identity(Conf) ->
    ct_property_test:quickcheck(
        ndto_schema_properties:prop_identity(),
        Conf
    ).

prop_idempotent(Conf) ->
    ct_property_test:quickcheck(
        ndto_schema_properties:prop_idempotent(),
        Conf
    ).

prop_domination(Conf) ->
    ct_property_test:quickcheck(
        ndto_schema_properties:prop_domination(),
        Conf
    ).

prop_empty_schema_complement(_Conf) ->
    EmptySet = false,
    UniversalSchema = ndto_schema:universal_schema(),
    ?assertEqual(UniversalSchema, ndto_schema:complement(EmptySet)),
    ok.

prop_universal_schema_complement(_Conf) ->
    UniversalSchema1 = ndto_schema:universal_schema(),
    UniversalSchema2 = true,
    EmptySet = false,
    ?assertEqual(EmptySet, ndto_schema:complement(UniversalSchema1)),
    ?assertEqual(EmptySet, ndto_schema:complement(UniversalSchema2)),
    ok.

complement(_Conf) ->
    %% TODO: implement enum complement
    %% TODO: implement enum validation for non-strings
    % EnumComplement = ndto_schema:complement(?ENUM_SCHEMA),
    % ok = generate_and_load(enum_complement, EnumComplement),
    % false = enum_complement:is_valid(1),
    % true = enum_complement:is_valid(false),

    BooleanComplement = ndto_schema:complement(?BOOLEAN_SCHEMA),
    ok = generate_and_load(boolean_complement, BooleanComplement),
    ?assertEqual(false, boolean_complement:is_valid(false)),
    ?assertEqual(true, boolean_complement:is_valid(1)),

    IntegerComplement = ndto_schema:complement(?INTEGER_SCHEMA),
    ok = generate_and_load(integer_complement, IntegerComplement),
    ?assertEqual(false, integer_complement:is_valid(3)),
    ?assertEqual(true, integer_complement:is_valid(1)),
    ?assertEqual(true, integer_complement:is_valid(true)),

    NumberComplement = ndto_schema:complement(?NUMBER_SCHEMA),
    ok = generate_and_load(number_complement, NumberComplement),
    ?assertEqual(false, number_complement:is_valid(5)),
    ?assertEqual(false, number_complement:is_valid(5.5)),
    ?assertEqual(true, number_complement:is_valid(9)),
    ?assertEqual(true, number_complement:is_valid(true)),

    StringComplement = ndto_schema:complement(?STRING_SCHEMA),
    ok = generate_and_load(string_complement, StringComplement),
    ?assertEqual(false, string_complement:is_valid(<<"123aaa">>)),
    ?assertEqual(true, string_complement:is_valid(<<"123aa6">>)),
    ?assertEqual(true, string_complement:is_valid(<<"aaa4567">>)),
    ?assertEqual(true, string_complement:is_valid(true)),

    ArrayComplement = ndto_schema:complement(?ARRAY_SCHEMA),
    ok = generate_and_load(array_complement, ArrayComplement),
    ?assertEqual(false, array_complement:is_valid([5])),
    ?assertEqual(true, array_complement:is_valid([9])),
    ?assertEqual(true, array_complement:is_valid(true)),

    ObjectComplement = ndto_schema:complement(?OBJECT_SCHEMA),
    ok = generate_and_load(object_complement, ObjectComplement),
    ?assertEqual(
        false,
        object_complement:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),
    ?assertEqual(
        true,
        object_complement:is_valid(#{<<"foo">> => 1, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),
    ?assertEqual(
        true,
        object_complement:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"1aa">>, <<"baz">> => true})
    ),
    ?assertEqual(true, object_complement:is_valid(true)),

    IntersectionComplement = ndto_schema:complement(?INTERSECTION_SCHEMA),
    ok = generate_and_load(intersection_complement, IntersectionComplement),
    ?assertEqual(false, intersection_complement:is_valid(5)),
    ?assertEqual(true, intersection_complement:is_valid(3)),
    ?assertEqual(true, intersection_complement:is_valid(true)),

    UnionComplement = ndto_schema:complement(?UNION_SCHEMA),
    ok = generate_and_load(union_complement, UnionComplement),
    ?assertEqual(false, union_complement:is_valid(true)),
    ?assertEqual(false, union_complement:is_valid(<<"12aaa6">>)),
    ?assertEqual(true, union_complement:is_valid(<<"123456">>)),
    ?assertEqual(true, union_complement:is_valid(<<"aaa4567">>)),
    ?assertEqual(true, union_complement:is_valid([1, 2, 3])),

    SymmetricDifferenceComplement = ndto_schema:complement(?SYMMETRIC_DIFFERENCE_SCHEMA),
    ok = generate_and_load(symmetric_difference_complement, SymmetricDifferenceComplement),
    ?assertEqual(false, symmetric_difference_complement:is_valid(3)),
    ?assertEqual(false, symmetric_difference_complement:is_valid(7)),
    ?assertEqual(true, symmetric_difference_complement:is_valid(5)),
    ?assertEqual(true, symmetric_difference_complement:is_valid(true)),

    ComplementComplement = ndto_schema:complement(?COMPLEMENT_SCHEMA),
    ok = generate_and_load(complement_complement, ComplementComplement),
    ?assertEqual(false, complement_complement:is_valid(5)),
    ?assertEqual(true, complement_complement:is_valid(true)),

    ok.

intersection(_Conf) ->
    %% TODO: implement enum validation for non-strings
    % EnumIntersection = ndto_schema:intersection([?ENUM_SCHEMA, #{<<"enum">> => [true, #{<<"foo">> => <<"bar">>}]}]),
    % ok = generate_and_load(enum_intersection, EnumIntersection),
    % false = enum_intersection:is_valid(#{<<"foo">> => <<"bar">>}),
    % false = enum_intersection:is_valid([1, 2, 3]),
    % true = enum_intersection:is_valid(true),

    BooleanIntersection = ndto_schema:intersection([?BOOLEAN_SCHEMA, #{<<"type">> => <<"boolean">>}]),
    ok = generate_and_load(boolean_intersection, BooleanIntersection),
    ?assertEqual(false, boolean_intersection:is_valid(<<"string">>)),
    ?assertEqual(true, boolean_intersection:is_valid(true)),

    IntegerIntersection = ndto_schema:intersection([
        ?INTEGER_SCHEMA, #{<<"type">> => <<"integer">>, <<"minimum">> => 4}
    ]),
    ok = generate_and_load(integer_intersection, IntegerIntersection),
    ?assertEqual(false, integer_intersection:is_valid(3)),
    ?assertEqual(true, integer_intersection:is_valid(4)),

    NumberIntersection = ndto_schema:intersection([
        ?NUMBER_SCHEMA, #{<<"type">> => <<"number">>, <<"maximum">> => 10}
    ]),
    ok = generate_and_load(number_intersection, NumberIntersection),
    ?assertEqual(false, number_intersection:is_valid(1.0)),
    ?assertEqual(true, number_intersection:is_valid(7.0)),

    StringIntersection = ndto_schema:intersection([
        ?STRING_SCHEMA, #{<<"type">> => <<"string">>, <<"pattern">> => <<"b{3}">>}
    ]),
    ok = generate_and_load(string_intersection, StringIntersection),
    ?assertEqual(false, string_intersection:is_valid(<<"123aaa">>)),
    ?assertEqual(false, string_intersection:is_valid(<<"bbb">>)),
    ?assertEqual(true, string_intersection:is_valid(<<"aaabbb">>)),

    ArrayIntersection = ndto_schema:intersection([
        ?ARRAY_SCHEMA, #{<<"type">> => <<"array">>, <<"items">> => ?INTEGER_SCHEMA}
    ]),
    ok = generate_and_load(array_intersection, ArrayIntersection),
    ?assertEqual(false, array_intersection:is_valid([2])),
    ?assertEqual(false, array_intersection:is_valid([5.0, 5.1, 5.2])),
    ?assertEqual(true, array_intersection:is_valid([5, 5, 5])),

    ObjectIntersection = ndto_schema:intersection([
        ?OBJECT_SCHEMA, #{<<"type">> => <<"object">>, <<"maxProperties">> => 4}
    ]),
    ok = generate_and_load(object_intersection, ObjectIntersection),
    ?assertEqual(
        false,
        object_intersection:is_valid(#{
            <<"foo">> => 4,
            <<"bar">> => <<"aaa">>,
            <<"baz">> => true,
            <<"foobar">> => 1,
            <<"qux">> => <<"quux">>
        })
    ),
    ?assertEqual(
        false,
        object_intersection:is_valid(#{
            <<"foo">> => 4, <<"bar">> => false, <<"baz">> => true, <<"foobar">> => 1
        })
    ),
    ?assertEqual(
        true,
        object_intersection:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),

    IntersectionIntersection = ndto_schema:intersection([
        ?INTERSECTION_SCHEMA, #{<<"allOf">> => [#{<<"type">> => <<"integer">>, <<"minimum">> => 5}]}
    ]),
    ok = generate_and_load(intersection_intersection, IntersectionIntersection),
    ?assertEqual(false, intersection_intersection:is_valid(4)),
    ?assertEqual(false, intersection_intersection:is_valid(9)),
    ?assertEqual(true, intersection_intersection:is_valid(5)),

    UnionIntersection = ndto_schema:intersection([
        ?UNION_SCHEMA, #{<<"anyOf">> => [?BOOLEAN_SCHEMA, ?NUMBER_SCHEMA]}
    ]),
    ok = generate_and_load(union_intersection, UnionIntersection),
    ?assertEqual(false, union_intersection:is_valid(5.0)),
    ?assertEqual(false, union_intersection:is_valid(<<"foo">>)),
    ?assertEqual(true, union_intersection:is_valid(true)),

    SymmetricDifferenceIntersection = ndto_schema:intersection([
        ?SYMMETRIC_DIFFERENCE_SCHEMA, #{<<"oneOf">> => [?INTEGER_SCHEMA, ?STRING_SCHEMA]}
    ]),
    ok = generate_and_load(symmetric_difference_intersection, SymmetricDifferenceIntersection),
    ?assertEqual(false, symmetric_difference_intersection:is_valid(<<"foo">>)),
    ?assertEqual(false, symmetric_difference_intersection:is_valid(5)),
    ?assertEqual(true, symmetric_difference_intersection:is_valid(3)),

    ComplementIntersection = ndto_schema:intersection([
        ?COMPLEMENT_SCHEMA, #{<<"not">> => ?BOOLEAN_SCHEMA}
    ]),
    ok = generate_and_load(complement_intersection, ComplementIntersection),
    ?assertEqual(false, complement_intersection:is_valid(true)),
    ?assertEqual(true, complement_intersection:is_valid(<<"foo">>)),

    ok.

union(_Conf) ->
    %% TODO: implement enum validation for non-strings
    % EnumUnion = ndto_schema:union([?ENUM_SCHEMA, #{<<"enum">> => [#{<<"foo">> => <<"bar">>}]}]),
    % ok = generate_and_load(enum_union, EnumUnion),
    % false = enum_union:is_valid(false),
    % true = enum_union:is_valid(#{<<"foo">> => <<"bar">>}),

    BooleanUnion = ndto_schema:union([?BOOLEAN_SCHEMA, #{<<"type">> => <<"boolean">>}]),
    ok = generate_and_load(boolean_union, BooleanUnion),
    ?assertEqual(true, boolean_union:is_valid(true)),
    ?assertEqual(false, boolean_union:is_valid(1)),

    IntegerUnion = ndto_schema:union([
        ?INTEGER_SCHEMA, #{<<"type">> => <<"integer">>, <<"minimum">> => 6}
    ]),
    ok = generate_and_load(integer_union, IntegerUnion),
    ?assertEqual(true, integer_union:is_valid(4)),
    ?assertEqual(true, integer_union:is_valid(7)),
    ?assertEqual(false, integer_union:is_valid(1)),
    ?assertEqual(false, integer_union:is_valid(true)),

    NumberUnion = ndto_schema:union([
        ?NUMBER_SCHEMA, #{<<"type">> => <<"number">>, <<"maximum">> => 5}
    ]),
    ok = generate_and_load(number_union, NumberUnion),
    ?assertEqual(true, number_union:is_valid(1)),
    ?assertEqual(true, number_union:is_valid(7)),
    ?assertEqual(false, number_union:is_valid(9)),
    ?assertEqual(false, number_union:is_valid(true)),

    StringUnion = ndto_schema:union([
        ?STRING_SCHEMA, #{<<"type">> => <<"string">>, <<"pattern">> => <<"b{3}">>}
    ]),
    ok = generate_and_load(string_union, StringUnion),
    ?assertEqual(true, string_union:is_valid(<<"bbb">>)),
    ?assertEqual(true, string_union:is_valid(<<"aaa">>)),
    ?assertEqual(false, string_union:is_valid(<<"foo">>)),
    ?assertEqual(false, string_union:is_valid(true)),

    ArrayUnion = ndto_schema:union([
        ?ARRAY_SCHEMA, #{<<"type">> => <<"array">>, <<"items">> => ?BOOLEAN_SCHEMA}
    ]),
    ok = generate_and_load(array_union, ArrayUnion),
    ?assertEqual(true, array_union:is_valid([true])),
    ?assertEqual(true, array_union:is_valid([5.0, 6, 7.9999])),
    ?assertEqual(false, array_union:is_valid([<<"foo">>, <<"bar">>, <<"baz">>])),
    ?assertEqual(false, array_union:is_valid(true)),

    ObjectUnion = ndto_schema:union([
        ?OBJECT_SCHEMA,
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => #{
                <<"qux">> => #{<<"type">> => <<"boolean">>}
            },
            <<"required">> => [<<"qux">>]
        }
    ]),
    ok = generate_and_load(object_union, ObjectUnion),
    ?assertEqual(
        true, object_union:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),
    ?assertEqual(true, object_union:is_valid(#{<<"qux">> => true})),
    ?assertEqual(false, object_union:is_valid(#{<<"foo">> => <<"foobar">>})),
    ?assertEqual(false, object_union:is_valid(false)),

    IntersectionUnion = ndto_schema:union([
        ?INTERSECTION_SCHEMA, #{<<"allOf">> => [?BOOLEAN_SCHEMA]}
    ]),
    ok = generate_and_load(intersection_union, IntersectionUnion),
    ?assertEqual(true, intersection_union:is_valid(true)),
    ?assertEqual(true, intersection_union:is_valid(5)),
    ?assertEqual(false, intersection_union:is_valid(5.1)),
    ?assertEqual(false, intersection_union:is_valid(<<"foo">>)),

    UnionUnion = ndto_schema:union([
        ?UNION_SCHEMA, #{<<"anyOf">> => [?INTEGER_SCHEMA, ?NUMBER_SCHEMA]}
    ]),
    ok = generate_and_load(union_union, UnionUnion),
    ?assertEqual(true, union_union:is_valid(7.9)),
    ?assertEqual(true, union_union:is_valid(true)),
    ?assertEqual(false, union_union:is_valid(#{<<"foo">> => <<"bar">>})),

    SymmetricDifferenceUnion = ndto_schema:union([
        ?SYMMETRIC_DIFFERENCE_SCHEMA, #{<<"oneOf">> => [?BOOLEAN_SCHEMA, ?STRING_SCHEMA]}
    ]),
    ok = generate_and_load(symmetric_difference_union, SymmetricDifferenceUnion),
    ?assertEqual(true, symmetric_difference_union:is_valid(4)),
    ?assertEqual(true, symmetric_difference_union:is_valid(true)),
    ?assertEqual(false, symmetric_difference_union:is_valid(5)),
    ?assertEqual(false, symmetric_difference_union:is_valid(#{})),

    ComplementUnion = ndto_schema:union([?COMPLEMENT_SCHEMA, #{<<"not">> => ?OBJECT_SCHEMA}]),
    ok = generate_and_load(complement_union, ComplementUnion),
    ?assertEqual(true, complement_union:is_valid(true)),
    ?assertEqual(
        true,
        complement_union:is_valid(#{<<"foo">> => 4, <<"bar">> => <<"aaa">>, <<"baz">> => true})
    ),
    ?assertEqual(true, complement_union:is_valid(<<"foo">>)),

    ok.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
generate_and_load(Name, Schema) ->
    DTO = ndto:generate(Name, Schema),
    ok = ndto:load(DTO).
