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

%% @doc A <code>ndto</code> parser for draft-04 JSON Schema specifications.
-module(ndto_parser_json_schema_draft_04).

%%% BEHAVIOURS
-behaviour(ndto_parser_json_schema).

%%% EXTERNAL EXPORTS
-export([
    parse/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(Spec, CTX) -> Result when
    Spec :: ndto_parser_json_schema:spec(),
    CTX :: ndto_parser_json_schema:ctx(),
    Result :: {Schema, ExtraSchemas, NewCTX},
    Schema :: ndto:schema(),
    ExtraSchemas :: [{ndto:name(), ndto:schema()}],
    NewCTX :: ndto_parser_json_schema:ctx().
%% @doc Parses a JSONSchema draft-04 specification into a list of <code>ndto:schema()</code> values.
parse(false, CTX) ->
    Schema = false,
    {Schema, [], CTX};
parse(true, CTX) ->
    Schema = #{},
    {Schema, [], CTX};
parse(#{<<"$ref">> := Ref}, CTX) ->
    {RefName, RefSchema, RefCTX} = ndto_parser_json_schema:resolve_ref(Ref, CTX),
    Schema = #{ref => RefName},
    case lists:member(RefName, maps:get(resolved, CTX)) of
        true ->
            {Schema, [], CTX};
        false ->
            {NewSchema, NewExtraSchemas, NewCTX} = parse(RefSchema, RefCTX),
            {
                Schema,
                [{erlang:binary_to_atom(RefName), NewSchema} | NewExtraSchemas],
                CTX#{resolved => maps:get(resolved, NewCTX)}
            }
    end;
parse(#{<<"enum">> := Enum}, CTX) ->
    Schema = #{enum => Enum},
    {Schema, [], CTX};
parse(#{<<"type">> := <<"null">>}, CTX) ->
    Schema = #{enum => [<<"null">>]},
    {Schema, [], CTX};
parse(#{<<"type">> := <<"boolean">>}, CTX) ->
    Schema = #{type => boolean},
    {Schema, [], CTX};
parse(#{<<"type">> := <<"integer">>} = RawSchema, CTX) ->
    Minimum = maps:get(<<"minimum">>, RawSchema, undefined),
    ExclusiveMinimum = maps:get(<<"exclusiveMinimum">>, RawSchema, undefined),
    Maximum = maps:get(<<"maximum">>, RawSchema, undefined),
    ExclusiveMaximum = maps:get(<<"exclusiveMaximum">>, RawSchema, undefined),
    MultipleOf = maps:get(<<"multipleOf">>, RawSchema, undefined),
    Schema =
        #{
            type => integer,
            minimum => Minimum,
            exclusive_minimum => ExclusiveMinimum,
            maximum => Maximum,
            exclusive_maximum => ExclusiveMaximum,
            multiple_of => MultipleOf
        },
    {Schema, [], CTX};
parse(#{<<"type">> := <<"number">>} = RawSchema, CTX) ->
    Minimum = maps:get(<<"minimum">>, RawSchema, undefined),
    ExclusiveMinimum = maps:get(<<"exclusiveMinimum">>, RawSchema, undefined),
    Maximum = maps:get(<<"maximum">>, RawSchema, undefined),
    ExclusiveMaximum = maps:get(<<"exclusiveMaximum">>, RawSchema, undefined),
    MultipleOf = maps:get(<<"multipleOf">>, RawSchema, undefined),
    Schema =
        #{
            any_of => [
                #{
                    type => integer,
                    minimum => Minimum,
                    exclusive_minimum => ExclusiveMinimum,
                    maximum => Maximum,
                    exclusive_maximum => ExclusiveMaximum,
                    multiple_of => MultipleOf
                },
                #{
                    type => float,
                    minimum => Minimum,
                    exclusive_minimum => ExclusiveMinimum,
                    maximum => Maximum,
                    exclusive_maximum => ExclusiveMaximum
                }
            ]
        },
    {Schema, [], CTX};
parse(#{<<"type">> := <<"string">>} = RawSchema, CTX) ->
    MinLength = maps:get(<<"minLength">>, RawSchema, undefined),
    MaxLength = maps:get(<<"maxLength">>, RawSchema, undefined),
    Format =
        case maps:get(<<"format">>, RawSchema, undefined) of
            <<"iso8601">> ->
                iso8601;
            <<"byte">> ->
                base64;
            _Otherwise ->
                undefined
        end,
    Pattern = maps:get(<<"pattern">>, RawSchema, undefined),
    Schema =
        #{
            type => string,
            min_length => MinLength,
            max_length => MaxLength,
            format => Format,
            pattern => Pattern
        },
    {Schema, [], CTX};
parse(#{<<"type">> := <<"array">>} = RawSchema, CTX) ->
    {Items, ItemsExtraSchemas, ItemsCTX} =
        case maps:get(<<"items">>, RawSchema, undefined) of
            undefined ->
                {undefined, [], CTX};
            RawItems when is_list(RawItems) ->
                {IS, ES, CT} = lists:foldl(
                    fun(RawItemSchema, {ItemsAcc, ExtraSchemasAcc, CTXAcc}) ->
                        {ItemSchema, ExtraSchemas, NewCTX} = parse(
                            RawItemSchema, CTXAcc
                        ),
                        {
                            [ItemSchema | ItemsAcc],
                            ExtraSchemasAcc ++ ExtraSchemas,
                            CTXAcc#{resolved => maps:get(resolved, NewCTX)}
                        }
                    end,
                    {[], [], CTX},
                    RawItems
                ),
                {lists:reverse(IS), ES, CT};
            RawItems ->
                parse(RawItems, CTX)
        end,
    {AdditionalItems, AdditionalItemsExtraSchemas, AdditionalItemsCTX} =
        case maps:get(<<"additionalItems">>, RawSchema, undefined) of
            undefined ->
                {undefined, [], ItemsCTX};
            RawAdditionalItems ->
                parse(RawAdditionalItems, ItemsCTX)
        end,
    MinItems = maps:get(<<"minItems">>, RawSchema, undefined),
    MaxItems = maps:get(<<"maxItems">>, RawSchema, undefined),
    UniqueItems = maps:get(<<"uniqueItems">>, RawSchema, undefined),
    Schema =
        #{
            type => array,
            items => Items,
            additional_items => AdditionalItems,
            min_items => MinItems,
            max_items => MaxItems,
            unique_items => UniqueItems
        },
    {Schema, ItemsExtraSchemas ++ AdditionalItemsExtraSchemas, AdditionalItemsCTX};
parse(#{<<"type">> := <<"object">>} = RawSchema, CTX) ->
    {Properties, PropertiesExtraSchemas, PropertiesCTX} =
        case maps:get(<<"properties">>, RawSchema, undefined) of
            undefined ->
                {undefined, [], CTX};
            RawProperties ->
                lists:foldl(
                    fun({Property, RawPropertySchema}, {PropertiesAcc, ExtraSchemasAcc, CTXAcc}) ->
                        {PropertySchema, ExtraSchemas, NewCTX} = parse(
                            RawPropertySchema, CTXAcc
                        ),
                        {
                            PropertiesAcc#{Property => PropertySchema},
                            ExtraSchemasAcc ++ ExtraSchemas,
                            CTXAcc#{resolved => maps:get(resolved, NewCTX)}
                        }
                    end,
                    {#{}, [], CTX},
                    maps:to_list(RawProperties)
                )
        end,
    Required = maps:get(<<"required">>, RawSchema, undefined),
    MinProperties = maps:get(<<"minProperties">>, RawSchema, undefined),
    MaxProperties = maps:get(<<"maxProperties">>, RawSchema, undefined),
    {AdditionalProperties, AdditionalPropertiesExtraSchemas, AdditionalPropertiesCTX} =
        case maps:get(<<"additionalProperties">>, RawSchema, undefined) of
            undefined ->
                {undefined, [], PropertiesCTX};
            RawAdditionalProperties ->
                parse(RawAdditionalProperties, PropertiesCTX)
        end,
    {PatternProperties, PatternPropertiesExtraSchemas, PatternPropertiesCTX} =
        case maps:get(<<"patternProperties">>, RawSchema, undefined) of
            undefined ->
                {undefined, [], AdditionalPropertiesCTX};
            RawPatternProperties ->
                lists:foldl(
                    fun(
                        {Pattern, RawPatternSchema}, {PatternPropertiesAcc, ExtraSchemasAcc, CTXAcc}
                    ) ->
                        {PatternSchema, ExtraSchemas, NewCTX} = parse(
                            RawPatternSchema, CTXAcc
                        ),
                        {
                            PatternPropertiesAcc#{Pattern => PatternSchema},
                            ExtraSchemasAcc ++ ExtraSchemas,
                            CTXAcc#{resolved => maps:get(resolved, NewCTX)}
                        }
                    end,
                    {#{}, [], AdditionalPropertiesCTX},
                    maps:to_list(RawPatternProperties)
                )
        end,
    Schema =
        #{
            type => object,
            properties => Properties,
            required => Required,
            min_properties => MinProperties,
            max_properties => MaxProperties,
            additional_properties => AdditionalProperties,
            pattern_properties => PatternProperties
        },
    {Schema,
        PropertiesExtraSchemas ++ AdditionalPropertiesExtraSchemas ++ PatternPropertiesExtraSchemas,
        PatternPropertiesCTX};
parse(#{<<"anyOf">> := RawAnyOf}, CTX) ->
    {AnyOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun(RawSchema, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Schema, ExtraSchemas, NewCTX} = parse(RawSchema, CTXAcc),
                {[Schema | AnyOfAcc], ExtraSchemasAcc ++ ExtraSchemas, CTXAcc#{
                    resolved => maps:get(resolved, NewCTX)
                }}
            end,
            {[], [], CTX},
            RawAnyOf
        ),
    Schema = #{any_of => AnyOf},
    {Schema, ExtraSchemas, NewCTX};
parse(#{<<"allOf">> := RawAllOf}, CTX) ->
    {AllOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun(RawSchema, {AllOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Schema, ExtraSchemas, NewCTX} = parse(RawSchema, CTXAcc),
                {[Schema | AllOfAcc], ExtraSchemasAcc ++ ExtraSchemas, CTXAcc#{
                    resolved => maps:get(resolved, NewCTX)
                }}
            end,
            {[], [], CTX},
            RawAllOf
        ),
    Schema = #{all_of => AllOf},
    {Schema, ExtraSchemas, NewCTX};
parse(#{<<"not">> := RawNot}, CTX) ->
    {Not, ExtraSchemas, NewCTX} = parse(RawNot, CTX),
    Schema = #{'not' => Not},
    {Schema, ExtraSchemas, NewCTX};
parse(#{<<"oneOf">> := RawOneOf}, CTX) ->
    {OneOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun(RawSchema, {OneOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Schema, ExtraSchemas, NewCTX} = parse(RawSchema, CTXAcc),
                {[Schema | OneOfAcc], ExtraSchemasAcc ++ ExtraSchemas, CTXAcc#{
                    resolved => maps:get(resolved, NewCTX)
                }}
            end,
            {[], [], CTX},
            RawOneOf
        ),
    Schema = #{one_of => OneOf},
    {Schema, ExtraSchemas, NewCTX};
parse(UniversalSchema, CTX) ->
    RawSchema = attempt_type(UniversalSchema),
    parse(RawSchema, CTX).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec attempt_type(Schema) -> Result when
    Schema :: ndto_parser_json_schema:spec(),
    Result :: ndto_parser_json_schema:spec().
attempt_type(Schema) ->
    Base = #{
        boolean => #{<<"type">> => <<"boolean">>},
        number => #{<<"type">> => <<"number">>},
        string => #{<<"type">> => <<"string">>},
        array => #{<<"type">> => <<"array">>},
        object => #{<<"type">> => <<"object">>}
    },
    Keywords = attempt_type(maps:to_list(Schema), Base),
    Boolean = maps:get(boolean, Keywords, #{}),
    Number = maps:get(number, Keywords, #{}),
    String = maps:get(string, Keywords, #{}),
    Array = maps:get(array, Keywords, #{}),
    Object = maps:get(object, Keywords, #{}),
    #{
        <<"anyOf">> => [
            Boolean,
            Number,
            String,
            Array,
            Object
        ]
    }.

attempt_type([], Acc) ->
    Acc;
attempt_type([{<<"minimum">>, Minimum} | Rest], #{number := OldNumber} = OldAcc) ->
    Acc = OldAcc#{number => OldNumber#{<<"minimum">> => Minimum}},
    attempt_type(Rest, Acc);
attempt_type(
    [{<<"exclusiveMinimum">>, ExclusiveMinimum} | Rest], #{number := OldNumber} = OldAcc
) ->
    Acc = OldAcc#{number => OldNumber#{<<"exclusiveMinimum">> => ExclusiveMinimum}},
    attempt_type(Rest, Acc);
attempt_type([{<<"maximum">>, Maximum} | Rest], #{number := OldNumber} = OldAcc) ->
    Acc = OldAcc#{number => OldNumber#{<<"maximum">> => Maximum}},
    attempt_type(Rest, Acc);
attempt_type(
    [{<<"exclusiveMaximum">>, ExclusiveMaximum} | Rest], #{number := OldNumber} = OldAcc
) ->
    Acc = OldAcc#{number => OldNumber#{<<"exclusiveMaximum">> => ExclusiveMaximum}},
    attempt_type(Rest, Acc);
attempt_type([{<<"multipleOf">>, MultipleOf} | Rest], #{number := OldNumber} = OldAcc) ->
    Acc = OldAcc#{number => OldNumber#{<<"multipleOf">> => MultipleOf}},
    attempt_type(Rest, Acc);
attempt_type([{<<"minLength">>, MinLength} | Rest], #{string := OldString} = OldAcc) ->
    Acc = OldAcc#{string => OldString#{<<"minLength">> => MinLength}},
    attempt_type(Rest, Acc);
attempt_type([{<<"maxLength">>, MaxLength} | Rest], #{string := OldString} = OldAcc) ->
    Acc = OldAcc#{string => OldString#{<<"maxLength">> => MaxLength}},
    attempt_type(Rest, Acc);
attempt_type([{<<"format">>, Format} | Rest], #{string := OldString} = OldAcc) ->
    Acc = OldAcc#{string => OldString#{<<"format">> => Format}},
    attempt_type(Rest, Acc);
attempt_type([{<<"pattern">>, Pattern} | Rest], #{string := OldString} = OldAcc) ->
    Acc = OldAcc#{string => OldString#{<<"pattern">> => Pattern}},
    attempt_type(Rest, Acc);
attempt_type([{<<"minItems">>, MinItems} | Rest], #{array := OldArray} = OldAcc) ->
    Acc = OldAcc#{array => OldArray#{<<"minItems">> => MinItems}},
    attempt_type(Rest, Acc);
attempt_type([{<<"maxItems">>, MaxItems} | Rest], #{array := OldArray} = OldAcc) ->
    Acc = OldAcc#{array => OldArray#{<<"maxItems">> => MaxItems}},
    attempt_type(Rest, Acc);
attempt_type([{<<"uniqueItems">>, UniqueItems} | Rest], #{array := OldArray} = OldAcc) ->
    Acc = OldAcc#{array => OldArray#{<<"uniqueItems">> => UniqueItems}},
    attempt_type(Rest, Acc);
attempt_type([{<<"items">>, Items} | Rest], #{array := OldArray} = OldAcc) ->
    Acc = OldAcc#{array => OldArray#{<<"items">> => Items}},
    attempt_type(Rest, Acc);
attempt_type(
    [{<<"additionalItems">>, AdditionalItems} | Rest], #{array := OldArray} = OldAcc
) ->
    Acc = OldAcc#{array => OldArray#{<<"additionalItems">> => AdditionalItems}},
    attempt_type(Rest, Acc);
attempt_type([{<<"properties">>, Properties} | Rest], #{object := OldObject} = OldAcc) ->
    Acc = OldAcc#{object => OldObject#{<<"properties">> => Properties}},
    attempt_type(Rest, Acc);
attempt_type([{<<"required">>, Required} | Rest], #{object := OldObject} = OldAcc) ->
    Acc = OldAcc#{object => OldObject#{<<"required">> => Required}},
    attempt_type(Rest, Acc);
attempt_type(
    [{<<"minProperties">>, MinProperties} | Rest], #{object := OldObject} = OldAcc
) ->
    Acc = OldAcc#{object => OldObject#{<<"minProperties">> => MinProperties}},
    attempt_type(Rest, Acc);
attempt_type(
    [{<<"maxProperties">>, MaxProperties} | Rest], #{object := OldObject} = OldAcc
) ->
    Acc = OldAcc#{object => OldObject#{<<"maxProperties">> => MaxProperties}},
    attempt_type(Rest, Acc);
attempt_type(
    [{<<"additionalProperties">>, AdditionalProperties} | Rest], #{object := OldObject} = OldAcc
) ->
    Acc = OldAcc#{object => OldObject#{<<"additionalProperties">> => AdditionalProperties}},
    attempt_type(Rest, Acc);
attempt_type(
    [{<<"patternProperties">>, PatternProperties} | Rest], #{object := OldObject} = OldAcc
) ->
    Acc = OldAcc#{object => OldObject#{<<"patternProperties">> => PatternProperties}},
    attempt_type(Rest, Acc);
attempt_type([_UnknownKeyword | Rest], Acc) ->
    attempt_type(Rest, Acc).
