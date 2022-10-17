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
-module(ndto_openapi).

%%% BEHAVIOURS
-behaviour(ndto_format).

%%% EXTERNAL EXPORTS
-export([
    is_valid/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
is_valid(Prefix, #{<<"$ref">> := RawRef} = Schema) ->
    Ref =
        case filename:basename(RawRef) of
            ListBasename when is_list(ListBasename) ->
                erlang:list_to_binary(ListBasename);
            BinBasename ->
                BinBasename
        end,
    FunName = <<Prefix/binary, "ref_", Ref/binary>>,
    OptionalClause = ndto_generator:optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(ref),
            [
                erl_syntax:application(
                    erl_syntax:atom(erlang:binary_to_atom(Ref)),
                    erl_syntax:atom(is_valid),
                    [
                        erl_syntax:variable('Val')
                    ]
                )
            ]
        ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause]
    ),
    {[Fun], []};
is_valid(_Prefix, #{<<"type">> := <<"string">>} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"type">> := <<"number">>} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"type">> := <<"integer">>} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"type">> := <<"boolean">>} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"type">> := <<"array">>} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"type">> := <<"object">>} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"oneOf">> := _Subschemas} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"anyOf">> := _Subschemas} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"allOf">> := _Subschemas} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"not">> := _Subschemas} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(Prefix, Schema) ->
    FunName = <<Prefix/binary, "any">>,
    OptionalClause = ndto_generator:optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(any),
            [erl_syntax:atom(true)]
        ),
    FalseClause = ndto_generator:false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {[Fun], []}.
