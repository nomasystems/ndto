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
-module(ndto_schema).

%%% INCLUDE FILES
-include_lib("ndto/include/ndto_schema.hrl").

%%% EXTERNAL EXPORTS
-export([
    complement/1,
    empty_schema/0,
    intersection/1,
    symmetric_difference/1,
    union/1,
    universal_schema/0
]).

%%% UTIL EXPORTS
-export([
    multiples/3
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec complement(Schema) -> Complement when
    Schema :: ndto:schema(),
    Complement :: ndto:schema().
complement(#{} = Any) when map_size(Any) =:= 0 ->
    false;
complement(true) ->
    false;
complement(false) ->
    #{};
complement(#{<<"allOf">> := AllOf}) ->
    union([complement(Schema) || Schema <- AllOf]);
complement(#{<<"anyOf">> := AnyOf}) ->
    intersection([complement(Schema) || Schema <- AnyOf]);
complement(#{<<"oneOf">> := OneOf}) ->
    Schema1 = symmetric_difference(OneOf),
    complement(Schema1);
complement(#{<<"not">> := Not}) ->
    Not;
complement(#{<<"enum">> := _Values}) ->
    %% TODO: mutation
    erlang:throw(not_implemented);
complement(#{<<"type">> := <<"boolean">>}) ->
    union(lists:delete(#{<<"type">> => <<"boolean">>}, ?BASIC_SCHEMAS));
complement(#{<<"type">> := <<"number">>} = Schema) ->
    Minimum =
        case maps:get(<<"minimum">>, Schema, undefined) of
            undefined ->
                undefined;
            Min ->
                ExclusiveMin = maps:get(<<"exclusiveMinimum">>, Schema, false),
                #{
                    <<"type">> => <<"number">>,
                    <<"maximum">> => Min,
                    <<"exclusiveMaximum">> => not ExclusiveMin
                }
        end,
    Maximum =
        case maps:get(<<"maximum">>, Schema, undefined) of
            undefined ->
                undefined;
            Max ->
                ExclusiveMax = maps:get(<<"exclusiveMaximum">>, Schema, false),
                #{
                    <<"type">> => <<"number">>,
                    <<"minimum">> => Max,
                    <<"exclusiveMinimum">> => not ExclusiveMax
                }
        end,
    Schemas = lists:filter(fun(S) -> S =/= undefined end, [Minimum, Maximum]),
    union(
        Schemas ++
            lists:subtract(
                ?BASIC_SCHEMAS,
                [
                    #{<<"type">> => <<"number">>},
                    #{<<"type">> => <<"integer">>}
                ]
            )
    );
complement(#{<<"type">> := <<"integer">>} = Schema) ->
    Min = maps:get(<<"minimum">>, Schema, undefined),
    Max = maps:get(<<"maximum">>, Schema, undefined),
    Minimum =
        case Min of
            undefined ->
                undefined;
            Min ->
                ExclusiveMin = maps:get(<<"exclusiveMinimum">>, Schema, false),
                #{
                    <<"type">> => <<"number">>,
                    <<"maximum">> => Min,
                    <<"exclusiveMaximum">> => not ExclusiveMin
                }
        end,
    Maximum =
        case Max of
            undefined ->
                undefined;
            Max ->
                ExclusiveMax = maps:get(<<"exclusiveMaximum">>, Schema, false),
                #{
                    <<"type">> => <<"number">>,
                    <<"minimum">> => Max,
                    <<"exclusiveMinimum">> => not ExclusiveMax
                }
        end,
    Intervals =
        case maps:get(<<"multipleOf">>, Schema, undefined) of
            undefined ->
                [Minimum, Maximum];
            Mult ->
                Multiples = ndto_schema:multiples(Mult, Min, Max),
                exclude_integers(Multiples)
        end,
    Schemas = lists:filter(fun(S) -> S =/= undefined end, Intervals),
    %% TODO: remove integers in numbers instead of fully removing the numbers domain
    union(
        Schemas ++
            lists:subtract(
                ?BASIC_SCHEMAS,
                [
                    #{<<"type">> => <<"number">>},
                    #{<<"type">> => <<"integer">>}
                ]
            )
    );
complement(#{<<"type">> := <<"string">>} = Schema) ->
    MinLength =
        case maps:get(<<"maxLength">>, Schema, undefined) of
            undefined ->
                undefined;
            Max ->
                #{
                    <<"type">> => <<"string">>,
                    <<"minLength">> => Max + 1
                }
        end,
    MaxLength =
        case maps:get(<<"minLength">>, Schema, 0) of
            0 ->
                undefined;
            Min ->
                #{
                    <<"type">> => <<"string">>,
                    <<"maxLength">> => Min - 1
                }
        end,
    Format =
        %% NOTE: mutation
        %% TODO: replace mutation with regex
        case maps:get(<<"format">>, Schema, undefined) of
            undefined ->
                undefined;
            F ->
                Formats = lists:delete(F, ?FORMATS),
                Schema#{<<"format">> => random_pick(Formats)}
        end,
    Pattern =
        case maps:get(<<"pattern">>, Schema, undefined) of
            undefined ->
                undefined;
            P ->
                Schema#{<<"pattern">> => <<"^(?!.*", P/binary, ").*">>}
        end,
    Schemas = lists:filter(fun(S) -> S =/= undefined end, [MinLength, MaxLength, Format, Pattern]),
    union(
        Schemas ++
            lists:delete(#{<<"type">> => <<"string">>}, ?BASIC_SCHEMAS)
    );
complement(#{<<"type">> := <<"array">>} = Schema) ->
    Items =
        case maps:get(<<"items">>, Schema, undefined) of
            undefined ->
                undefined;
            I ->
                Schema#{<<"type">> => <<"array">>, <<"items">> => complement(I)}
        end,
    MinItems =
        case maps:get(<<"minItems">>, Schema, undefined) of
            undefined ->
                undefined;
            Min ->
                #{<<"type">> => <<"array">>, <<"maxItems">> => Min - 1}
        end,
    MaxItems =
        case maps:get(<<"maxItems">>, Schema, undefined) of
            undefined ->
                undefined;
            Max ->
                #{<<"type">> => <<"array">>, <<"minItems">> => Max + 1}
        end,
    %% TODO: mutation to enum with repeated items within min and max if max_size is at least 2
    UniqueItems = undefined,
    Schemas = lists:filter(fun(S) -> S =/= undefined end, [Items, MinItems, MaxItems, UniqueItems]),
    union(
        Schemas ++
            lists:delete(#{<<"type">> => <<"array">>}, ?BASIC_SCHEMAS)
    );
complement(#{<<"type">> := <<"object">>} = Schema) ->
    Required = maps:get(<<"required">>, Schema, []),
    Properties = maps:get(<<"properties">>, Schema, #{}),
    PropertiesSchemas =
        lists:map(
            fun({PropertyName, PropertySchema}) ->
                NewRequired =
                    case lists:member(PropertyName, Required) of
                        true ->
                            lists:delete(PropertyName, Required);
                        false ->
                            [PropertyName | Required]
                    end,
                Schema#{
                    <<"required">> => NewRequired,
                    <<"properties">> => Properties#{
                        PropertyName => complement(PropertySchema)
                    }
                }
            end,
            maps:to_list(Properties)
        ),
    MinProperties =
        case maps:get(<<"minProperties">>, Schema, undefined) of
            undefined ->
                undefined;
            Min ->
                #{
                    <<"type">> => <<"object">>,
                    <<"maxProperties">> => Min - 1
                }
        end,
    MaxProperties =
        case maps:get(<<"maxProperties">>, Schema, undefined) of
            undefined ->
                undefined;
            Max ->
                #{
                    <<"type">> => <<"object">>,
                    <<"minProperties">> => Max + 1
                }
        end,
    AdditionalProperties =
        %% NOTE: mutation
        case maps:get(<<"additionalProperties">>, Schema, true) of
            true ->
                undefined;
            false ->
                PropertyName = new_property_name(maps:keys(Properties)),
                Schema#{
                    <<"properties">> => Properties#{
                        PropertyName => #{}
                    }
                };
            AdditionalSchema ->
                PropertyName = new_property_name(maps:keys(Properties)),
                Schema#{
                    <<"properties">> => Properties#{
                        PropertyName => complement(AdditionalSchema)
                    }
                }
        end,
    Schemas = lists:filter(
        fun(S) -> S =/= undefined end,
        PropertiesSchemas ++
            [
                MinProperties,
                MaxProperties,
                AdditionalProperties
            ]
    ),
    union(
        Schemas ++
            lists:delete(#{<<"type">> => <<"object">>}, ?BASIC_SCHEMAS)
    ).

-spec empty_schema() -> EmptySchema when
    EmptySchema :: ndto:empty_schema().
empty_schema() ->
    false.

-spec intersection(Schemas) -> Intersection when
    Schemas :: [ndto:schema()],
    Intersection :: ndto:schema().
intersection(Schemas) ->
    lists:foldl(
        fun(S1, Acc) ->
            intersection(Acc, S1)
        end,
        #{},
        Schemas
    ).

-spec intersection(Schema1, Schema2) -> Intersection when
    Schema1 :: ndto:schema(),
    Schema2 :: ndto:schema(),
    Intersection :: ndto:schema().
intersection(Schema, Schema) ->
    Schema;
intersection(#{} = Any, Schema2) when map_size(Any) =:= 0 ->
    Schema2;
intersection(Schema1, #{} = Any) when map_size(Any) =:= 0 ->
    Schema1;
intersection(true, Schema2) ->
    Schema2;
intersection(Schema1, true) ->
    Schema1;
intersection(#{<<"allOf">> := AllOf1}, Schema2) ->
    Schema1 = intersection(AllOf1),
    intersection(Schema1, Schema2);
intersection(Schema1, #{<<"allOf">> := AllOf}) ->
    Schema2 = intersection(AllOf),
    intersection(Schema1, Schema2);
intersection(#{<<"anyOf">> := AnyOf}, Schema2) ->
    union([intersection(Schema2, AnyOfSchema) || AnyOfSchema <- AnyOf]);
intersection(Schema1, #{<<"anyOf">> := AnyOf}) ->
    union([intersection(Schema1, AnyOfSchema) || AnyOfSchema <- AnyOf]);
intersection(#{<<"oneOf">> := OneOf}, Schema2) ->
    Schema1 = symmetric_difference(OneOf),
    intersection(Schema1, Schema2);
intersection(Schema1, #{<<"oneOf">> := OneOf}) ->
    Schema2 = symmetric_difference(OneOf),
    intersection(Schema1, Schema2);
intersection(#{<<"not">> := Not}, Schema2) ->
    Schema1 = complement(Not),
    intersection(Schema1, Schema2);
intersection(Schema1, #{<<"not">> := Not}) ->
    Schema2 = complement(Not),
    intersection(Schema1, Schema2);
intersection(#{<<"enum">> := Enum1}, #{<<"enum">> := Enum2}) ->
    NewEnum = sets:to_list(
        sets:intersection(
            sets:from_list(Enum1),
            sets:from_list(Enum2)
        )
    ),
    #{<<"enum">> => NewEnum};
intersection(Schema1, #{<<"enum">> := _Enum} = Schema2) ->
    intersection(Schema2, Schema1);
intersection(#{<<"enum">> := Enum}, Schema2) ->
    Name = erlang:binary_to_atom(
        <<"intersection_enum_", (erlang:integer_to_binary(erlang:unique_integer()))/binary>>
    ),
    DTO = ndto:generate(Name, Schema2),
    ndto:load(DTO),
    NewEnum = lists:filter(fun Name:is_valid/1, Enum),
    #{<<"enum">> => NewEnum};
intersection(#{<<"type">> := <<"boolean">>} = Schema1, #{<<"type">> := <<"boolean">>}) ->
    Schema1;
intersection(#{<<"type">> := <<"integer">>} = Schema1, #{<<"type">> := <<"number">>} = Schema2) ->
    intersection(Schema1, Schema2#{<<"type">> => <<"integer">>});
intersection(#{<<"type">> := <<"number">>} = Schema1, #{<<"type">> := <<"integer">>} = Schema2) ->
    intersection(Schema1#{<<"type">> => <<"integer">>}, Schema2);
intersection(#{<<"type">> := Type} = Schema1, #{<<"type">> := Type} = Schema2) when
    Type =:= <<"integer">> orelse Type =:= <<"number">>
->
    Minimum1 = maps:get(<<"minimum">>, Schema1, undefined),
    Minimum2 = maps:get(<<"minimum">>, Schema2, undefined),
    ExclusiveMinimum1 = maps:get(<<"exclusiveMinimum">>, Schema1, undefined),
    ExclusiveMinimum2 = maps:get(<<"exclusiveMinimum">>, Schema2, undefined),
    {Minimum, ExclusiveMinimum} =
        case {Minimum1, Minimum2} of
            {Minimum1, undefined} ->
                {Minimum1, ExclusiveMinimum1};
            {undefined, Minimum2} ->
                {Minimum2, ExclusiveMinimum2};
            {Minimum1, Minimum1} ->
                ExcMin =
                    case {ExclusiveMinimum1, ExclusiveMinimum2} of
                        {ExclusiveMinimum1, undefined} ->
                            ExclusiveMinimum1;
                        {undefined, ExclusiveMinimum2} ->
                            ExclusiveMinimum2;
                        {ExclusiveMinimum1, ExclusiveMinimum2} ->
                            ExclusiveMinimum1 orelse ExclusiveMinimum2
                    end,
                {Minimum1, ExcMin};
            {Minimum1, Minimum2} when Minimum1 > Minimum2 ->
                {Minimum1, ExclusiveMinimum1};
            {Minimum1, Minimum2} ->
                {Minimum2, ExclusiveMinimum2}
        end,

    Maximum1 = maps:get(<<"maximum">>, Schema1, undefined),
    Maximum2 = maps:get(<<"maximum">>, Schema2, undefined),
    ExclusiveMaximum1 = maps:get(<<"exclusiveMaximum">>, Schema1, undefined),
    ExclusiveMaximum2 = maps:get(<<"exclusiveMaximum">>, Schema2, undefined),
    {Maximum, ExclusiveMaximum} =
        case {Maximum1, Maximum2} of
            {Maximum1, undefined} ->
                {Maximum1, ExclusiveMaximum1};
            {undefined, Maximum2} ->
                {Maximum2, ExclusiveMaximum2};
            {Maximum1, Maximum1} ->
                ExcMax =
                    case {ExclusiveMaximum1, ExclusiveMaximum2} of
                        {ExclusiveMaximum1, undefined} ->
                            ExclusiveMaximum1;
                        {undefined, ExclusiveMaximum2} ->
                            ExclusiveMaximum2;
                        {ExclusiveMaximum1, ExclusiveMaximum2} ->
                            ExclusiveMaximum1 orelse ExclusiveMaximum2
                    end,
                {Maximum1, ExcMax};
            {Maximum1, Maximum2} when Maximum1 < Maximum2 ->
                {Maximum1, ExclusiveMaximum1};
            {Maximum1, Maximum2} ->
                {Maximum2, ExclusiveMaximum2}
        end,
    MultipleOf1 = maps:get(<<"multipleOf">>, Schema1, undefined),
    MultipleOf2 = maps:get(<<"multipleOf">>, Schema2, undefined),
    MultipleOf =
        case {MultipleOf1, MultipleOf2} of
            {MultipleOf1, undefined} ->
                MultipleOf1;
            {undefined, MultipleOf2} ->
                MultipleOf2;
            {MultipleOf1, MultipleOf2} ->
                lcm(MultipleOf1, MultipleOf2)
        end,
    clean(#{
        <<"type">> => Type,
        <<"minimum">> => Minimum,
        <<"exclusiveMinimum">> => ExclusiveMinimum,
        <<"maximum">> => Maximum,
        <<"exclusiveMaximum">> => ExclusiveMaximum,
        <<"multipleOf">> => MultipleOf
    });
intersection(#{<<"type">> := <<"string">>} = Schema1, #{<<"type">> := <<"string">>} = Schema2) ->
    MinLength1 = maps:get(<<"minLength">>, Schema1, undefined),
    MinLength2 = maps:get(<<"minLength">>, Schema2, undefined),

    MinLength =
        case {MinLength1, MinLength2} of
            {MinLength1, undefined} ->
                MinLength1;
            {undefined, MinLength2} ->
                MinLength2;
            {MinLength1, MinLength2} when MinLength1 > MinLength2 ->
                MinLength1;
            {_MinLength1, MinLength2} ->
                MinLength2
        end,

    MaxLength1 = maps:get(<<"maxLength">>, Schema1, undefined),
    MaxLength2 = maps:get(<<"maxLength">>, Schema2, undefined),

    MaxLength =
        case {MaxLength1, MaxLength2} of
            {MaxLength1, undefined} ->
                MaxLength1;
            {undefined, MaxLength2} ->
                MaxLength2;
            {MaxLength1, MaxLength2} when MaxLength1 < MaxLength2 ->
                MaxLength1;
            {_MaxLength1, MaxLength2} ->
                MaxLength2
        end,

    Pattern1 = maps:get(<<"pattern">>, Schema1, undefined),
    Pattern2 = maps:get(<<"pattern">>, Schema2, undefined),

    Pattern =
        case {Pattern1, Pattern2} of
            {Pattern1, undefined} ->
                Pattern1;
            {undefined, Pattern2} ->
                Pattern2;
            {Pattern1, Pattern2} ->
                <<"^(?=.*", Pattern1/binary, ")(?=.*", Pattern2/binary, ").*">>
        end,

    Format1 = maps:get(<<"format">>, Schema1, undefined),
    Format2 = maps:get(<<"format">>, Schema2, undefined),

    Format =
        case {Format1, Format2} of
            {Format1, undefined} ->
                Format1;
            {_Format1, _Format2} ->
                Format2
        end,

    clean(#{
        <<"type">> => <<"string">>,
        <<"minLength">> => MinLength,
        <<"maxLength">> => MaxLength,
        <<"pattern">> => Pattern,
        <<"format">> => Format
    });
intersection(#{<<"type">> := <<"array">>} = Schema1, #{<<"type">> := <<"array">>} = Schema2) ->
    Items1 = maps:get(<<"items">>, Schema1, undefined),
    Items2 = maps:get(<<"items">>, Schema2, undefined),

    Items =
        case {Items1, Items2} of
            {Items1, undefined} ->
                Items1;
            {undefined, Items2} ->
                Items2;
            {Items1, Items2} ->
                intersection(Items1, Items2)
        end,

    MinItems1 = maps:get(<<"minItems">>, Schema1, undefined),
    MinItems2 = maps:get(<<"minItems">>, Schema2, undefined),

    MinItems =
        case {MinItems1, MinItems2} of
            {MinItems1, undefined} ->
                MinItems1;
            {undefined, MinItems2} ->
                MinItems2;
            {MinItems1, MinItems2} when MinItems1 > MinItems2 ->
                MinItems1;
            {_MinItems1, MinItems2} ->
                MinItems2
        end,

    MaxItems1 = maps:get(<<"maxItems">>, Schema1, undefined),
    MaxItems2 = maps:get(<<"maxItems">>, Schema2, undefined),

    MaxItems =
        case {MaxItems1, MaxItems2} of
            {MaxItems1, undefined} ->
                MaxItems1;
            {undefined, MaxItems2} ->
                MaxItems2;
            {MaxItems1, MaxItems2} when MaxItems1 < MaxItems2 ->
                MaxItems1;
            {_MaxItems1, MaxItems2} ->
                MaxItems2
        end,

    UniqueItems1 = maps:get(<<"uniqueItems">>, Schema1, undefined),
    UniqueItems2 = maps:get(<<"uniqueItems">>, Schema2, undefined),

    UniqueItems =
        case {UniqueItems1, UniqueItems2} of
            {UniqueItems1, undefined} ->
                UniqueItems1;
            {undefined, UniqueItems2} ->
                UniqueItems2;
            {UniqueItems1, UniqueItems2} ->
                UniqueItems1 orelse UniqueItems2
        end,
    clean(#{
        <<"type">> => <<"array">>,
        <<"items">> => Items,
        <<"minItems">> => MinItems,
        <<"maxItems">> => MaxItems,
        <<"uniqueItems">> => UniqueItems
    });
intersection(#{<<"type">> := <<"object">>} = Schema1, #{<<"type">> := <<"object">>} = Schema2) ->
    Properties1 = maps:get(<<"properties">>, Schema1, undefined),
    Properties2 = maps:get(<<"properties">>, Schema2, undefined),
    Properties =
        case {Properties1, Properties2} of
            {Properties1, undefined} ->
                Properties1;
            {undefined, Properties2} ->
                Properties2;
            {Properties1, Properties2} ->
                CommonProperties =
                    sets:to_list(
                        sets:intersection(
                            sets:from_list(maps:keys(Properties1)),
                            sets:from_list(maps:keys(Properties2))
                        )
                    ),
                PropertyList =
                    lists:map(
                        fun(PropertyName) ->
                            PropertySchema1 = maps:get(PropertyName, Properties1),
                            PropertySchema2 = maps:get(PropertyName, Properties2),
                            PropertySchema = intersection([PropertySchema1, PropertySchema2]),
                            {PropertyName, PropertySchema}
                        end,
                        CommonProperties
                    ),
                maps:from_list(PropertyList)
        end,

    Required1 = maps:get(<<"required">>, Schema1, undefined),
    Required2 = maps:get(<<"required">>, Schema2, undefined),
    Required =
        case {Required1, Required2} of
            {Required1, undefined} ->
                Required1;
            {undefined, Required2} ->
                Required2;
            {Required1, Required2} ->
                lists:uniq(lists:append(Required1, Required2))
        end,

    MinProperties1 = maps:get(<<"minProperties">>, Schema1, undefined),
    MinProperties2 = maps:get(<<"minProperties">>, Schema2, undefined),
    MinProperties =
        case {MinProperties1, MinProperties2} of
            {MinProperties1, undefined} ->
                MinProperties1;
            {undefined, MinProperties2} ->
                MinProperties2;
            {MinProperties1, MinProperties2} when MinProperties1 > MinProperties2 ->
                MinProperties1;
            {_MinProperties1, MinProperties2} ->
                MinProperties2
        end,

    MaxProperties1 = maps:get(<<"maxProperties">>, Schema1, undefined),
    MaxProperties2 = maps:get(<<"maxProperties">>, Schema2, undefined),
    MaxProperties =
        case {MaxProperties1, MaxProperties2} of
            {MaxProperties1, undefined} ->
                MaxProperties1;
            {undefined, MaxProperties2} ->
                MaxProperties2;
            {MaxProperties1, MaxProperties2} when MaxProperties1 < MaxProperties2 ->
                MaxProperties1;
            {_MaxProperties1, MaxProperties2} ->
                MaxProperties2
        end,

    AdditionalProperties1 = maps:get(<<"additionalProperties">>, Schema1, undefined),
    AdditionalProperties2 = maps:get(<<"additionalProperties">>, Schema2, undefined),
    AdditionalProperties =
        case {AdditionalProperties1, AdditionalProperties2} of
            {AdditionalProperties1, undefined} ->
                AdditionalProperties1;
            {undefined, AdditionalProperties2} ->
                AdditionalProperties2;
            {AdditionalProperties1, AdditionalProperties2} ->
                intersection(AdditionalProperties1, AdditionalProperties2)
        end,

    clean(#{
        <<"type">> => <<"object">>,
        <<"properties">> => Properties,
        <<"required">> => Required,
        <<"minProperties">> => MinProperties,
        <<"maxProperties">> => MaxProperties,
        <<"additionalProperties">> => AdditionalProperties
    });
intersection(_Schema1, _Schema2) ->
    false.

-spec symmetric_difference(Schemas) -> SymmetricDifference when
    Schemas :: [ndto:schema()],
    SymmetricDifference :: ndto:schema().
symmetric_difference(Schemas) ->
    lists:foldl(
        fun(S1, Acc) ->
            symmetric_difference(Acc, S1)
        end,
        false,
        Schemas
    ).

-spec symmetric_difference(Schema1, Schema2) -> SymmetricDifference when
    Schema1 :: ndto:schema(),
    Schema2 :: ndto:schema(),
    SymmetricDifference :: ndto:schema().
symmetric_difference(Schema1, Schema2) ->
    union([
        intersection(Schema1, complement(Schema2)),
        intersection(complement(Schema1), Schema2)
    ]).

-spec union(Schemas) -> Union when
    Schemas :: [ndto:schema()],
    Union :: ndto:schema().
union(Schemas) ->
    lists:foldl(
        fun(S1, Acc) ->
            union(Acc, S1)
        end,
        false,
        Schemas
    ).

-spec union(Schema1, Schema2) -> Union when
    Schema1 :: ndto:schema(),
    Schema2 :: ndto:schema(),
    Union :: ndto:schema().
union(Schema, Schema) ->
    Schema;
union(_Schema1, #{} = Any) when map_size(Any) =:= 0 ->
    universal_schema();
union(#{} = Any, _Schema2) when map_size(Any) =:= 0 ->
    universal_schema();
union(_Schema1, true) ->
    universal_schema();
union(true, _Schema2) ->
    universal_schema();
union(Schema1, false) ->
    Schema1;
union(false, Schema2) ->
    Schema2;
union(#{<<"allOf">> := AllOf1}, Schema2) ->
    Schema1 = intersection(AllOf1),
    union(Schema1, Schema2);
union(Schema1, #{<<"allOf">> := AllOf}) ->
    Schema2 = intersection(AllOf),
    union(Schema1, Schema2);
union(#{<<"anyOf">> := AnyOf1}, #{<<"anyOf">> := AnyOf2}) ->
    case lists:sort(lists:uniq(AnyOf1 ++ AnyOf2)) of
        [] ->
            false;
        [Schema] ->
            Schema;
        AnyOf ->
            #{<<"anyOf">> => AnyOf}
    end;
union(#{<<"anyOf">> := AnyOf1}, Schema2) ->
    case lists:sort(lists:uniq([Schema2 | AnyOf1])) of
        [] ->
            false;
        [Schema] ->
            Schema;
        AnyOf ->
            #{<<"anyOf">> => AnyOf}
    end;
union(Schema1, #{<<"anyOf">> := AnyOf2}) ->
    case lists:sort(lists:uniq([Schema1 | AnyOf2])) of
        [] ->
            false;
        [Schema] ->
            Schema;
        AnyOf ->
            #{<<"anyOf">> => AnyOf}
    end;
union(#{<<"oneOf">> := OneOf}, Schema2) ->
    Schema1 = symmetric_difference(OneOf),
    union(Schema1, Schema2);
union(Schema1, #{<<"oneOf">> := OneOf}) ->
    Schema2 = symmetric_difference(OneOf),
    union(Schema1, Schema2);
union(#{<<"not">> := Not}, Schema2) ->
    Schema1 = complement(Not),
    union(Schema1, Schema2);
union(Schema1, #{<<"not">> := Not}) ->
    Schema2 = complement(Not),
    union(Schema1, Schema2);
union(#{<<"enum">> := Enum1}, #{<<"enum">> := Enum2}) ->
    NewEnum = lists:sort(lists:uniq(Enum1 ++ Enum2)),
    #{<<"enum">> => NewEnum};
union(#{<<"type">> := <<"boolean">>}, #{<<"type">> := <<"boolean">>}) ->
    #{<<"type">> => <<"boolean">>};
union(Schema1, Schema2) ->
    #{<<"anyOf">> => lists:sort([Schema1, Schema2])}.

-spec universal_schema() -> UniversalSchema when
    UniversalSchema :: ndto:universal_schema().
universal_schema() ->
    #{}.

%%%-----------------------------------------------------------------------------
%%% UTIL EXPORTS
%%%-----------------------------------------------------------------------------
-spec multiples(MultipleOf, Min, Max) -> Multiples when
    MultipleOf :: integer(),
    Min :: undefined | integer(),
    Max :: undefined | integer(),
    Multiples :: [integer()].
multiples(MultipleOf, undefined, Max) ->
    multiples(MultipleOf, ?MIN_INT, Max);
multiples(MultipleOf, Min, undefined) ->
    multiples(MultipleOf, Min, ?MAX_INT);
multiples(MultipleOf, Min, Max) when MultipleOf =< 0 ->
    multiples(MultipleOf * -1, Min, Max);
multiples(MultipleOf, Min, Max) ->
    FirstMultiple = MultipleOf * ((Min + MultipleOf - 1) div MultipleOf),
    multiples(MultipleOf, Max, FirstMultiple, []).

multiples(_MultipleOf, Max, Current, Acc) when Current > Max ->
    lists:reverse(Acc);
multiples(MultipleOf, Max, Current, Acc) ->
    multiples(MultipleOf, Max, Current + MultipleOf, [Current | Acc]).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec clean(Map) -> Clear when
    Map :: #{binary() => undefined | term()},
    Clear :: #{binary() => term()}.
clean(Schema) ->
    maps:filter(
        fun(_K, V) -> V =/= undefined end,
        Schema
    ).

-spec exclude_integers(Integers) -> Schemas when
    Integers :: [integer()],
    Schemas :: [ndto:schema()].
exclude_integers([]) ->
    [#{<<"type">> => <<"integer">>}];
exclude_integers([Integer | Integers]) ->
    Interval = #{
        <<"type">> => <<"integer">>,
        <<"maximum">> => Integer,
        <<"exclusiveMaximum">> => true
    },
    exclude_integers(Integers, [Interval]).

exclude_integers([], [#{<<"maximum">> := Previous} | _Tl] = Acc) ->
    Interval = #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => Previous,
        <<"exclusiveMinimum">> => true
    },
    [Interval | Acc];
exclude_integers([Next | Rest], [#{<<"maximum">> := Previous} | _Tl] = Acc) ->
    Interval = #{
        <<"type">> => <<"integer">>,
        <<"minimum">> => Previous,
        <<"exclusiveMinimum">> => true,
        <<"maximum">> => Next,
        <<"exclusiveMaximum">> => true
    },
    exclude_integers(Rest, [Interval | Acc]).

-spec gcd(A, B) -> GCD when
    A :: integer(),
    B :: integer(),
    GCD :: integer().
gcd(A, 0) -> A;
gcd(A, B) when abs(B) > abs(A) -> gcd(B, A);
gcd(A, B) -> gcd(B, A rem B).

-spec lcm(A, B) -> LCM when
    A :: integer(),
    B :: integer(),
    LCM :: integer().
lcm(A, B) ->
    case gcd(A, B) of
        0 ->
            0;
        GCD ->
            LCM = A * (B / GCD),
            LCM
    end.

-spec new_property_name(ExcludedNames) -> Name when
    ExcludedNames :: [binary()],
    Name :: binary().
new_property_name(ExcludedNames) ->
    PropertyName = base64:encode(crypto:strong_rand_bytes(24)),
    case lists:member(PropertyName, ExcludedNames) of
        true ->
            new_property_name(ExcludedNames);
        false ->
            PropertyName
    end.

-spec random_pick(List) -> Element when
    List :: [term(), ...],
    Element :: term().
random_pick(List) ->
    lists:nth(rand:uniform(erlang:length(List)), List).
