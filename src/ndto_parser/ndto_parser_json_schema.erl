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

%% @doc An <code>ndto</code> interface for parsing JSON Schemas.
-module(ndto_parser_json_schema).

%%% EXTERNAL EXPORTS
-export([
    parse/2
]).

%%% UTIL EXPORTS
-export([
    clean_optionals/1,
    get/2,
    parse_spec/1,
    resolve_ref/2
]).

%%% TYPES
-type ctx() :: #{
    base_path := binary(),
    base_name := binary(),
    resolved := [binary()],
    spec := spec()
}.
-type opts() :: #{
    name => atom()
}.
-type spec() :: njson:t().
-opaque t() :: module().
% A parser is a module that implements the <code>ndto_parser_json_schema</code> behaviour.

%%% EXPORT TYPES
-export_type([
    ctx/0,
    spec/0,
    t/0
]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-callback parse(Spec, CTX) -> Result when
    Spec :: spec(),
    CTX :: ctx(),
    Result :: {Schema, ExtraSchemas, NewCTX},
    Schema :: ndto:schema(),
    ExtraSchemas :: [{ndto:name(), ndto:schema()}],
    NewCTX :: ctx().

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(SpecPath, Opts) -> Result when
    SpecPath :: file:filename_all(),
    Opts :: opts(),
    Result :: {ok, Schemas} | {error, Reason},
    Schemas :: [{ndto:name(), ndto:schema()}],
    Reason :: term().
%% @doc Parses a JSONSchema specification into a list of <code>ndto:schema()</code> values.
parse(SpecPath, Opts) ->
    case parse_spec(SpecPath) of
        {ok, Spec} ->
            case parser(Spec) of
                {ok, Parser} ->
                    BasePath = filename:dirname(SpecPath),
                    BaseName = filename:rootname(filename:basename(SpecPath)),
                    CTX = #{
                        base_path => BasePath,
                        base_name => BaseName,
                        resolved => [],
                        spec => Spec
                    },
                    {Schema, ExtraSchemas, _NewCTX} = Parser:parse(Spec, CTX),
                    Name = maps:get(name, Opts, erlang:binary_to_atom(BaseName)),
                    RawSchemas = [{Name, Schema} | ExtraSchemas],
                    Schemas = [
                        {SchemaName, clean_optionals(RawSchema)}
                    || {SchemaName, RawSchema} <- RawSchemas
                    ],
                    {ok, Schemas};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%-----------------------------------------------------------------------------
%%% UTIL EXPORTS
%%%-----------------------------------------------------------------------------
-spec clean_optionals(RawValue) -> Value when
    RawValue :: term(),
    Value :: term().
clean_optionals(RawMap) when is_map(RawMap) ->
    maps:fold(
        fun
            (_Key, undefined, Acc) ->
                Acc;
            (Key, Value, Acc) ->
                maps:put(Key, clean_optionals(Value), Acc)
        end,
        #{},
        RawMap
    );
clean_optionals(RawList) when is_list(RawList) ->
    [clean_optionals(Value) || Value <- RawList, Value =/= undefined];
clean_optionals(Value) ->
    Value.

-spec get(Keys, Spec) -> Result when
    Keys :: [binary()],
    Spec :: map(),
    Result :: term().
get([], Spec) ->
    Spec;
get([Key | Keys], Spec) ->
    get(Keys, maps:get(Key, Spec)).

-spec parse_spec(SpecPath) -> Result when
    SpecPath :: binary(),
    Result :: {ok, Spec} | {error, Reason},
    Spec :: ndto_parser:spec(),
    Reason :: term().
parse_spec(SpecPath) ->
    case file:read_file(SpecPath) of
        {ok, BinSpec} ->
            case filename:extension(SpecPath) of
                JSON when JSON =:= <<".json">> orelse JSON =:= ".json" ->
                    case njson:decode(BinSpec) of
                        {ok, Spec} ->
                            {ok, Spec};
                        {error, Reason} ->
                            {error, {invalid_json, Reason}}
                    end;
                Extension ->
                    {error, {unsupported_extension, Extension}}
            end;
        {error, Reason} ->
            {error, {invalid_spec, Reason}}
    end.

-spec resolve_ref(Ref, CTX) -> Result when
    Ref :: binary(),
    CTX :: ctx(),
    Result :: {NewResolved, NewSchema, NewCTX},
    NewResolved :: binary(),
    NewSchema :: ndto:schema(),
    NewCTX :: ctx().
resolve_ref(Ref, CTX) ->
    BasePath = maps:get(base_path, CTX),
    BaseName = maps:get(base_name, CTX),
    Resolved = maps:get(resolved, CTX),
    Spec = maps:get(spec, CTX),

    [FilePath, ElementPath] = binary:split(Ref, <<"#">>, [global]),
    LocalPath = binary:split(ElementPath, <<"/">>, [global, trim_all]),
    {NewSpec, NewBasePath, NewBaseName} =
        case FilePath of
            <<>> ->
                {Spec, BasePath, BaseName};
            _FilePath ->
                AbsPath = filename:join(BasePath, FilePath),
                case parse_spec(AbsPath) of
                    {ok, RefSpec} ->
                        RefBasePath = filename:dirname(AbsPath),
                        RefBaseName = filename:rootname(filename:basename(AbsPath)),
                        {RefSpec, RefBasePath, RefBaseName};
                    {error, Reason} ->
                        erlang:error({invalid_ref, Reason})
                end
        end,
    NewResolved =
        case LocalPath of
            [] ->
                NewBaseName;
            _LocalPath ->
                <<NewBaseName/binary, "_", (lists:last(LocalPath))/binary>>
        end,
    NewSchema = get(LocalPath, NewSpec),
    NewCTX = #{
        base_path => NewBasePath,
        base_name => NewBaseName,
        resolved => [NewResolved | Resolved],
        spec => NewSpec
    },
    {NewResolved, NewSchema, NewCTX}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec parser(Spec) -> Result when
    Spec :: spec(),
    Result :: {ok, Parser} | {error, Reason},
    Parser :: t(),
    Reason :: term().
parser(#{<<"$schema">> := <<"http://json-schema.org/draft-04/schema#">>}) ->
    {ok, ndto_parser_json_schema_draft_04};
parser(_Schema) ->
    {error, unsupported_draft}.
