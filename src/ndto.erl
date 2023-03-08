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
%% limitations under the License
-module(ndto).

%%% INCLUDE FILES
-include("ndto.hrl").

%%% EXTERNAL EXPORTS
-export([
    generate/2,
    generate/3,
    load/1,
    load/2,
    write/2
]).

%%% TYPE EXPORTS
-export_type([dto/0]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(Name, Schema) -> Result when
    Name :: atom(),
    Schema :: schema(),
    Result :: dto().
generate(Name, Schema) ->
    generate(Name, Schema, openapi).

-spec generate(Name, Schema, Format) -> Result when
    Name :: atom(),
    Schema :: schema(),
    Format :: schema_format(),
    Result :: dto().
generate(Name, Schema, Format) ->
    ndto_generator:generate(Name, Schema, Format).

-spec load(DTO) -> Result when
    DTO :: dto(),
    Result :: ok | {ok, Warnings} | error | {error, {Errors, Warnings}},
    Errors :: [term()],
    Warnings :: [term()].
load(DTO) ->
    load(DTO, []).

-spec load(DTO, Options) -> Result when
    DTO :: dto(),
    Options :: [compile:option()],
    Result :: ok | error | {error, {Errors, Warnings}},
    Errors :: [term()],
    Warnings :: [term()].
load(DTO, Options) ->
    Forms = erl_syntax:revert_forms(DTO),
    case compile:forms(Forms, Options) of
        {ok, ModuleName, Bin} when is_binary(Bin), is_atom(ModuleName) ->
            case
                code:load_binary(
                    ModuleName, erlang:atom_to_list(ModuleName) ++ ".erl", Bin
                )
            of
                {module, ModuleName} ->
                    ok;
                {error, What} ->
                    {error, {[What], []}}
            end;
        {error, Errors, Warnings} ->
            {error, {Errors, Warnings}};
        error ->
            error
    end.

-spec write(DTO, Filename) -> Result when
    DTO :: dto(),
    Filename :: file:filename(),
    Result :: ok | {error, Reason},
    Reason :: file:posix() | badarg | terminated | system_limit.
write(DTO, Filename) ->
    Content = erl_prettypr:format(DTO),
    Bin = erlang:list_to_binary(Content),
    file:write_file(Filename, Bin).
