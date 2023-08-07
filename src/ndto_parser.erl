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

%% @doc An <code>ndto</code> behaviour for schema parsers.
-module(ndto_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/3
]).

%%% TYPES
-opaque t() :: module().
% A parser is a module that implements the <code>ndto_parser</code> behaviour.

%%% EXPORT TYPES
-export_type([
    t/0
]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-callback parse(Namespace, SpecPath) -> Result when
    Namespace :: atom(),
    SpecPath :: binary(),
    Result :: {ok, Schemas} | {error, Reason},
    Schemas :: [{SchemaName, ndto:schema()}],
    SchemaName :: ndto:name(),
    Reason :: term().
% Parses a specification into a <code>ndto:schema()</code>

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(Parser, Namespace, SpecPath) -> Result when
    Parser :: t(),
    Namespace :: atom(),
    SpecPath :: binary(),
    Result :: {ok, Schemas} | {error, Reason},
    Schemas :: [{SchemaName, ndto:schema()}],
    SchemaName :: ndto:name(),
    Reason :: term().
%% @doc Parses a schema specification into a <code>ndto:schema()</code> using the given parser.
parse(Parser, Namespace, SpecPath) ->
    Parser:parse(Namespace, SpecPath).
