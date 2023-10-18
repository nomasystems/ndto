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

%% @doc <code>ndto</code>'s main module.
-module(ndto).

%%% EXTERNAL EXPORTS
-export([
    generate/2,
    load/1,
    load/2,
    write/2
]).

%% TYPES
-type name() :: atom().
-type schema() ::
    empty_schema()
    | universal_schema()
    | ref_schema()
    | enum_schema()
    | boolean_schema()
    | integer_schema()
    | float_schema()
    | string_schema()
    | array_schema()
    | object_schema()
    | union_schema()
    | intersection_schema()
    | complement_schema()
    | symmetric_difference_schema().
-type t() :: erl_syntax:syntaxTree().

-type empty_schema() :: false.
-type universal_schema() :: true | #{} | union_schema().
-type ref_schema() :: #{
    ref := binary()
}.
-type enum_schema() :: #{
    enum := [value()]
}.
-type boolean_schema() :: #{
    type := boolean
}.
-type integer_schema() :: #{
    type := integer,
    minimum => integer(),
    exclusive_minimum => boolean(),
    maximum => integer(),
    exclusive_maximum => boolean(),
    multiple_of => integer()
}.
-type float_schema() :: #{
    type := float,
    minimum => float(),
    exclusive_minimum => boolean(),
    maximum => float(),
    exclusive_maximum => boolean()
}.
-type string_schema() :: #{
    type := string,
    min_length => non_neg_integer(),
    max_length => non_neg_integer(),
    format => format(),
    pattern => pattern()
}.
-type array_schema() :: #{
    type := array,
    items => schema(),
    min_items => non_neg_integer(),
    max_items => non_neg_integer(),
    unique_items => boolean()
}.
-type object_schema() :: #{
    type := object,
    properties => #{binary() => schema()},
    required => [binary()],
    min_properties => non_neg_integer(),
    max_properties => non_neg_integer(),
    pattern_properties => #{pattern() => schema()},
    additional_properties => schema()
}.
-type union_schema() :: #{
    any_of := [schema()]
}.
-type intersection_schema() :: #{
    all_of := [schema()]
}.
-type complement_schema() :: #{
    'not' := schema()
}.
-type symmetric_difference_schema() :: #{
    one_of := [schema()]
}.

-type value() ::
    boolean()
    | integer()
    | float()
    | binary()
    | array()
    | object().
-type array() :: [value()].
-type object() :: #{binary() => value()}.
-type format() :: iso8601 | base64.
% TODO: use openapi defined formats
-type pattern() :: binary().

%%% TYPE EXPORTS
-export_type([
    name/0,
    schema/0,
    t/0,
    empty_schema/0,
    universal_schema/0,
    ref_schema/0,
    enum_schema/0,
    boolean_schema/0,
    integer_schema/0,
    float_schema/0,
    string_schema/0,
    array_schema/0,
    object_schema/0,
    union_schema/0,
    intersection_schema/0,
    complement_schema/0,
    symmetric_difference_schema/0,
    value/0,
    format/0,
    pattern/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(Name, Schema) -> Result when
    Name :: name(),
    Schema :: schema(),
    Result :: t().
%% @doc Generates an Erlang Syntax Tree of a DTO module from a schema
generate(Name, Schema) ->
    ndto_generator:generate(Name, Schema).

-spec load(DTO) -> Result when
    DTO :: t(),
    Result :: ok | {ok, Warnings} | error | {error, {Errors, Warnings}},
    Errors :: [term()],
    Warnings :: [term()].
%% @equiv load(DTO, [])
load(DTO) ->
    load(DTO, []).

-spec load(DTO, Options) -> Result when
    DTO :: t(),
    Options :: [compile:option()],
    Result :: ok | {ok, Warnings} | error | {error, {Errors, Warnings}},
    Errors :: [term()],
    Warnings :: [term()].
%% @doc Loads a DTO module into the Erlang Runtime System
load(DTO, Options) ->
    Forms = erl_syntax:revert_forms(DTO),
    case compile:forms(Forms, Options) of
        {ok, ModuleName, Bin} when is_atom(ModuleName) andalso is_binary(Bin) ->
            case load_binary(ModuleName, Bin) of
                ok ->
                    ok;
                {error, What} ->
                    {error, {[What], []}}
            end;
        {ok, ModuleName, Bin, Warnings} when is_atom(ModuleName) andalso is_binary(Bin) ->
            case load_binary(ModuleName, Bin) of
                ok ->
                    {ok, Warnings};
                {error, What} ->
                    {error, {[What], Warnings}}
            end;
        {error, Errors, Warnings} ->
            {error, {Errors, Warnings}};
        error ->
            error
    end.

-spec write(DTO, Filename) -> Result when
    DTO :: t(),
    Filename :: file:filename(),
    Result :: ok | {error, Reason},
    Reason :: invalid | file:posix() | badarg | terminated | system_limit.
%% @doc Writes a DTO module to a file
write(DTO, Filename) ->
    Module = erl_prettypr:format(DTO),
    case unicode:characters_to_binary(Module, utf8) of
        {error, _List, _RestData} ->
            {error, invalid};
        {incomplete, _List, _Binary} ->
            {error, invalid};
        Content ->
            file:write_file(Filename, Content)
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec load_binary(ModuleName, Bin) -> Result when
    ModuleName :: atom(),
    Bin :: binary(),
    Result :: ok | {error, What},
    What :: term().
load_binary(ModuleName, Bin) ->
    case
        code:load_binary(
            ModuleName, erlang:atom_to_list(ModuleName) ++ ".erl", Bin
        )
    of
        {module, ModuleName} ->
            ok;
        {error, What} ->
            {error, What}
    end.
