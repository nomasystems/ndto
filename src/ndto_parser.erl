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

%% @doc An <code>ndto</code> interface and behaviour for schema parsers.
-module(ndto_parser).

%%% EXTERNAL EXPORTS
-export([
    parse/2,
    parse/3
]).

%%% TYPES
-type ctx() :: term().
-type spec() :: term().
-opaque t() :: module().
% A parser is a module that implements the <code>ndto_parser</code> behaviour.

%%% EXPORT TYPES
-export_type([
    ctx/0,
    spec/0,
    t/0
]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-callback parse(SpecPath, Opts) -> Result when
    SpecPath :: file:filename_all(),
    Opts :: map(),
    Result :: {ok, Schemas} | {error, Reason},
    Schemas :: [{ndto:name(), ndto:schema()}],
    Reason :: term().
% Parses a specification into a list of <code>ndto:schema()</code> values.

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec parse(Parser, SpecPath) -> Result when
    Parser :: t(),
    SpecPath :: file:filename_all(),
    Result :: {ok, Schemas} | {error, Reason},
    Schemas :: [{ndto:name(), ndto:schema()}],
    Reason :: term().
%% @equiv parse(Parser, SpecPath, #{})
parse(Parser, SpecPath) ->
    parse(Parser, SpecPath, #{}).

-spec parse(Parser, SpecPath, Opts) -> Result when
    Parser :: t(),
    SpecPath :: file:filename_all(),
    Opts :: map(),
    Result :: {ok, Schemas} | {error, Reason},
    Schemas :: [{ndto:name(), ndto:schema()}],
    Reason :: term().
%% @doc Parses a schema specification into a <code>ndto:schema()</code> using the given parser.
parse(Parser, SpecPath, Opts) ->
    Parser:parse(SpecPath, Opts).
