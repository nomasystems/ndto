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

%% @doc An <code>ndto_parser</code> for draft-04 JSON Schema specifications.
-module(ndto_parser_json_schema_draft_04).

%%% BEHAVIOURS
-behaviour(ndto_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/2
]).

%%% TYPES
-type json_schema() :: njson:t().

%%% RECORDS
-record(ctx, {
    base_path :: binary(),
    namespace :: binary(),
    resolved :: [binary()],
    spec :: json_schema()
}).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(Namespace, SpecPath) -> Result when
    Namespace :: atom(),
    SpecPath :: binary(),
    Result :: {ok, Schemas} | {error, Reason},
    Schemas :: [{ndto:name(), ndto:schema()}],
    Reason :: term().
%% @doc Parses a draft-04 JSON Schema specification into a list of <code>ndto:schema()</code> values.
parse(RawNamespace, SpecPath) ->
    Namespace = erlang:atom_to_binary(RawNamespace),
    case read_spec(SpecPath) of
        {ok, BinSpec} ->
            case deserialize_spec(BinSpec) of
                {ok, Spec} ->
                    CTX = #ctx{
                        base_path = filename:dirname(SpecPath),
                        namespace = Namespace,
                        resolved = [],
                        spec = Spec
                    },
                    {Schema, ExtraSchemas, _CTX} = parse_schemas(CTX, Spec),
                    RawSchemas = [{erlang:binary_to_atom(Namespace), Schema} | ExtraSchemas],
                    Schemas = [
                        {Name, clean_schema(RawSchema)}
                     || {Name, RawSchema} <- RawSchemas
                    ],
                    {ok, Schemas};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec clean_schema(RawSchema) -> Schema when
    RawSchema :: ndto:schema(),
    Schema :: ndto:schema().
clean_schema(RawSchema) when is_map(RawSchema) ->
    maps:fold(
        fun
            (_Key, undefined, Acc) ->
                Acc;
            (Key, List, Acc) when is_list(List) ->
                maps:put(Key, [clean_schema(Value) || Value <- List, Value =/= undefined], Acc);
            (Key, Value, Acc) ->
                maps:put(Key, clean_schema(Value), Acc)
        end,
        #{},
        RawSchema
    );
clean_schema(Schema) ->
    Schema.

-spec deserialize_spec(Bin) -> Result when
    Bin :: binary(),
    Result :: {ok, json_schema()} | {error, Reason},
    Reason :: term().
deserialize_spec(Bin) ->
    try
        Data = njson:decode(Bin),
        {ok, Data}
    catch
        _Error:Reason ->
            {error, {invalid_json, Reason}}
    end.

-spec get(Keys, Spec) -> Result when
    Keys :: [binary()],
    Spec :: map(),
    Result :: term().
get([], Spec) ->
    Spec;
get([Key | Keys], Spec) ->
    get(Keys, maps:get(Key, Spec)).

-spec parse_schemas(CTX, Spec) -> Result when
    CTX :: #ctx{},
    Spec :: json_schema(),
    Result :: {Schema, ExtraSchemas, NewCTX},
    Schema :: ndto:schema(),
    ExtraSchemas :: [{ndto:name(), ndto:schema()}],
    NewCTX :: #ctx{}.
parse_schemas(CTX, undefined) ->
    Schema = undefined,
    {Schema, [], CTX};
parse_schemas(CTX, false) ->
    Schema = false,
    {Schema, [], CTX};
parse_schemas(CTX, true) ->
    Schema = #{},
    {Schema, [], CTX};
parse_schemas(CTX, UniversalSchema) when is_map(UniversalSchema), map_size(UniversalSchema) =:= 0 ->
    Schema = #{},
    {Schema, [], CTX};
parse_schemas(CTX, #{<<"$ref">> := Ref}) ->
    {RefName, RefSchema, RefCTX} = resolve_ref(Ref, CTX),
    Schema = #{<<"$ref">> => RefName},
    case lists:member(RefName, CTX#ctx.resolved) of
        true ->
            {Schema, [], CTX};
        false ->
            {NewSchema, NewExtraSchemas, NewCTX} = parse_schemas(RefCTX, RefSchema),
            {Schema, [{erlang:binary_to_atom(RefName), NewSchema} | NewExtraSchemas], CTX#ctx{
                resolved = NewCTX#ctx.resolved
            }}
    end;
parse_schemas(CTX, #{<<"enum">> := Enum}) ->
    Schema = #{<<"enum">> => Enum},
    {Schema, [], CTX};
parse_schemas(CTX, #{<<"type">> := <<"boolean">>}) ->
    Schema = #{<<"type">> => <<"boolean">>},
    {Schema, [], CTX};
parse_schemas(CTX, #{<<"type">> := <<"integer">>} = RawSchema) ->
    Minimum = maps:get(<<"minimum">>, RawSchema, undefined),
    ExclusiveMinimum = maps:get(<<"exclusiveMinimum">>, RawSchema, undefined),
    Maximum = maps:get(<<"maximum">>, RawSchema, undefined),
    ExclusiveMaximum = maps:get(<<"exclusiveMaximum">>, RawSchema, undefined),
    MultipleOf = maps:get(<<"multipleOf">>, RawSchema, undefined),
    Schema =
        #{
            <<"type">> => <<"integer">>,
            <<"minimum">> => Minimum,
            <<"exclusiveMinimum">> => ExclusiveMinimum,
            <<"maximum">> => Maximum,
            <<"exclusiveMaximum">> => ExclusiveMaximum,
            <<"multipleOf">> => MultipleOf
        },
    {Schema, [], CTX};
parse_schemas(CTX, #{<<"type">> := <<"number">>} = RawSchema) ->
    Minimum = maps:get(<<"minimum">>, RawSchema, undefined),
    ExclusiveMinimum = maps:get(<<"exclusiveMinimum">>, RawSchema, undefined),
    Maximum = maps:get(<<"maximum">>, RawSchema, undefined),
    ExclusiveMaximum = maps:get(<<"exclusiveMaximum">>, RawSchema, undefined),
    MultipleOf = maps:get(<<"multipleOf">>, RawSchema, undefined),
    Schema =
        #{
            <<"type">> => <<"number">>,
            <<"minimum">> => Minimum,
            <<"exclusiveMinimum">> => ExclusiveMinimum,
            <<"maximum">> => Maximum,
            <<"exclusiveMaximum">> => ExclusiveMaximum,
            <<"multipleOf">> => MultipleOf
        },
    {Schema, [], CTX};
parse_schemas(CTX, #{<<"type">> := <<"string">>} = RawSchema) ->
    MinLength = maps:get(<<"minLength">>, RawSchema, undefined),
    MaxLength = maps:get(<<"maxLength">>, RawSchema, undefined),
    Format = maps:get(<<"format">>, RawSchema, undefined),
    Pattern = maps:get(<<"pattern">>, RawSchema, undefined),
    Schema =
        #{
            <<"type">> => <<"string">>,
            <<"minLength">> => MinLength,
            <<"maxLength">> => MaxLength,
            <<"format">> => Format,
            <<"pattern">> => Pattern
        },
    {Schema, [], CTX};
parse_schemas(CTX, #{<<"type">> := <<"array">>} = RawSchema) ->
    {Items, ItemsExtraSchemas, ItemsCTX} =
        case maps:get(<<"items">>, RawSchema, undefined) of
            undefined ->
                {undefined, []};
            RawItems ->
                parse_schemas(CTX, RawItems)
        end,
    MinItems = maps:get(<<"minItems">>, RawSchema, undefined),
    MaxItems = maps:get(<<"maxItems">>, RawSchema, undefined),
    UniqueItems = maps:get(<<"uniqueItems">>, RawSchema, undefined),
    Schema =
        #{
            <<"type">> => <<"array">>,
            <<"items">> => Items,
            <<"minItems">> => MinItems,
            <<"maxItems">> => MaxItems,
            <<"uniqueItems">> => UniqueItems
        },
    {Schema, ItemsExtraSchemas, ItemsCTX};
parse_schemas(CTX, #{<<"type">> := <<"object">>} = RawSchema) ->
    {Properties, PropertiesExtraSchemas, PropertiesCTX} =
        case maps:get(<<"properties">>, RawSchema, undefined) of
            undefined ->
                {undefined, [], CTX};
            RawProperties ->
                lists:foldl(
                    fun({Property, RawPropertySchema}, {PropertiesAcc, ExtraSchemasAcc, CTXAcc}) ->
                        {PropertySchema, ExtraSchemas, NewCTX} = parse_schemas(
                            CTXAcc, RawPropertySchema
                        ),
                        {
                            PropertiesAcc#{Property => PropertySchema},
                            ExtraSchemasAcc ++ ExtraSchemas,
                            CTXAcc#ctx{resolved = NewCTX#ctx.resolved}
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
                parse_schemas(PropertiesCTX, RawAdditionalProperties)
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
                        {PatternSchema, ExtraSchemas, NewCTX} = parse_schemas(
                            CTXAcc, RawPatternSchema
                        ),
                        {
                            PatternPropertiesAcc#{Pattern => PatternSchema},
                            ExtraSchemasAcc ++ ExtraSchemas,
                            CTXAcc#ctx{resolved = NewCTX#ctx.resolved}
                        }
                    end,
                    {#{}, [], AdditionalPropertiesCTX},
                    maps:to_list(RawPatternProperties)
                )
        end,
    Schema =
        #{
            <<"type">> => <<"object">>,
            <<"properties">> => Properties,
            <<"required">> => Required,
            <<"minProperties">> => MinProperties,
            <<"maxProperties">> => MaxProperties,
            <<"additionalProperties">> => AdditionalProperties,
            <<"patternProperties">> => PatternProperties
        },
    {Schema,
        PropertiesExtraSchemas ++ AdditionalPropertiesExtraSchemas ++ PatternPropertiesExtraSchemas,
        PatternPropertiesCTX};
parse_schemas(CTX, #{<<"anyOf">> := RawAnyOf}) ->
    {AnyOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun(RawSchema, {AnyOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Schema, ExtraSchemas, NewCTX} = parse_schemas(CTXAcc, RawSchema),
                {[Schema | AnyOfAcc], ExtraSchemasAcc ++ ExtraSchemas, CTXAcc#ctx{
                    resolved = NewCTX#ctx.resolved
                }}
            end,
            {[], [], CTX},
            RawAnyOf
        ),
    Schema = #{<<"anyOf">> => AnyOf},
    {Schema, ExtraSchemas, NewCTX};
parse_schemas(CTX, #{<<"allOf">> := RawAllOf}) ->
    {AllOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun(RawSchema, {AllOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Schema, ExtraSchemas, NewCTX} = parse_schemas(CTXAcc, RawSchema),
                {[Schema | AllOfAcc], ExtraSchemasAcc ++ ExtraSchemas, CTXAcc#ctx{
                    resolved = NewCTX#ctx.resolved
                }}
            end,
            {[], [], CTX},
            RawAllOf
        ),
    Schema = #{<<"allOf">> => AllOf},
    {Schema, ExtraSchemas, NewCTX};
parse_schemas(CTX, #{<<"not">> := RawNot}) ->
    {Not, ExtraSchemas, NewCTX} = parse_schemas(CTX, RawNot),
    Schema = #{<<"not">> => Not},
    {Schema, ExtraSchemas, NewCTX};
parse_schemas(CTX, #{<<"oneOf">> := RawOneOf}) ->
    {OneOf, ExtraSchemas, NewCTX} =
        lists:foldl(
            fun(RawSchema, {OneOfAcc, ExtraSchemasAcc, CTXAcc}) ->
                {Schema, ExtraSchemas, NewCTX} = parse_schemas(CTXAcc, RawSchema),
                {[Schema | OneOfAcc], ExtraSchemasAcc ++ ExtraSchemas, CTXAcc#ctx{
                    resolved = NewCTX#ctx.resolved
                }}
            end,
            {[], [], CTX},
            RawOneOf
        ),
    Schema = #{<<"oneOf">> => OneOf},
    {Schema, ExtraSchemas, NewCTX}.

-spec read_spec(SpecPath) -> Result when
    SpecPath :: binary(),
    Result :: {ok, BinSpec} | {error, Reason},
    BinSpec :: binary(),
    Reason :: term().
read_spec(SpecPath) ->
    case file:read_file(SpecPath) of
        {ok, BinSpec} ->
            {ok, BinSpec};
        {error, Reason} ->
            {error, {invalid_spec, Reason}}
    end.

-spec resolve_ref(Ref, CTX) -> Result when
    Ref :: binary(),
    CTX :: #ctx{},
    Result :: {NewResolved, NewSchema, NewCTX},
    NewResolved :: binary(),
    NewSchema :: ndto:schema(),
    NewCTX :: #ctx{}.
resolve_ref(Ref, #ctx{base_path = BasePath, namespace = Namespace, resolved = Resolved, spec = Spec}) ->
    [FilePath, ElementPath] = binary:split(Ref, <<"#">>, [global]),
    [<<>> | LocalPath] = binary:split(ElementPath, <<"/">>, [global]),
    {NewSpec, NewBasePath, NewNamespace} =
        case FilePath of
            <<>> ->
                {Spec, BasePath, Namespace};
            _FilePath ->
                AbsPath = filename:join(BasePath, FilePath),
                case read_spec(AbsPath) of
                    {ok, Bin} ->
                        case deserialize_spec(Bin) of
                            {ok, RefSpec} ->
                                RefBasePath = filename:dirname(AbsPath),
                                RefNamespace = lists:last(
                                    binary:split(FilePath, <<"/">>, [global])
                                ),
                                {RefSpec, RefBasePath, RefNamespace};
                            {error, Reason} ->
                                erlang:error({invalid_ref, Reason})
                        end;
                    {error, Reason} ->
                        erlang:error({invalid_ref, Reason})
                end
        end,
    NewResolved = <<NewNamespace/binary, "_", (lists:last(LocalPath))/binary>>,
    NewSchema = get(LocalPath, NewSpec),
    NewCTX = #ctx{
        base_path = NewBasePath,
        namespace = NewNamespace,
        resolved = [NewResolved | Resolved],
        spec = NewSpec
    },
    {NewResolved, NewSchema, NewCTX}.
