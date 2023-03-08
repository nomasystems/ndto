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
-module(ndto_generator).

%%% INCLUDE FILES
-include("ndto.hrl").
-include("ndto_generator.hrl").

%%% EXTERNAL EXPORTS
-export([generate/3]).

%%% UTIL EXPORTS
-export([
    chain_conditions/2,
    false_clause/0,
    optional_clause/1,
    type_guard/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(Name, Schema, Format) -> Result when
    Name :: atom(),
    Schema :: schema(),
    Format :: schema_format(),
    Result :: dto().
generate(Name, Schema, Format) ->
    ModuleHeader = erl_syntax:comment(?COPYRIGHT ++ [?NOTE]),
    ModuleAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Name)]),
    ExportHeader = erl_syntax:comment([?EXPORTS_HEADER]),
    ExportAttr = erl_syntax:attribute(erl_syntax:atom(export), [
        erl_syntax:list([
            erl_syntax:arity_qualifier(erl_syntax:atom(is_valid), erl_syntax:integer(1))
        ])
    ]),
    ExportHeader2 = erl_syntax:comment([?CLINE, ?EXPORTS_HEADER, ?CLINE]),

    FormatModule = erlang:binary_to_atom(<<"ndto_", (erlang:atom_to_binary(Format))/binary>>),
    {IsValidFun, ExtraFuns} = FormatModule:is_valid(<<"is_valid_">>, Schema),
    Fun =
        erl_syntax:function(
            erl_syntax:atom(is_valid),
            [
                erl_syntax:clause(
                    [erl_syntax:variable('Val')],
                    none,
                    [
                        erl_syntax:application(
                            erl_syntax:function_name(IsValidFun),
                            [erl_syntax:variable('Val')]
                        )
                    ]
                )
            ]
        ),

    InternalHeader = erl_syntax:comment([?CLINE, ?INTERNAL_HEADER, ?CLINE]),

    erl_syntax:form_list(
        lists:append([
            [
                erl_syntax:set_precomments(
                    ModuleAttr,
                    [ModuleHeader]
                ),
                erl_syntax:set_precomments(
                    ExportAttr,
                    [ExportHeader]
                ),
                erl_syntax:set_precomments(
                    Fun,
                    [ExportHeader2]
                ),
                erl_syntax:set_precomments(
                    IsValidFun,
                    [InternalHeader]
                )
            ],
            ExtraFuns
        ])
    ).

%%%-----------------------------------------------------------------------------
%%% UTIL EXPORTS
%%%-----------------------------------------------------------------------------
chain_conditions(FunCalls, 'andalso' = Operator) ->
    chain_conditions(FunCalls, Operator, erl_syntax:atom(true));
chain_conditions(FunCalls, 'orelse' = Operator) ->
    chain_conditions(FunCalls, Operator, erl_syntax:atom(false));
chain_conditions(FunCalls, 'xor' = Operator) ->
    chain_conditions(FunCalls, Operator, erl_syntax:atom(false)).

false_clause() ->
    erl_syntax:clause(
        [erl_syntax:variable('_Val')],
        none,
        [erl_syntax:atom(false)]
    ).

optional_clause(#{<<"nullable">> := true}) ->
    [
        erl_syntax:clause(
            [erl_syntax:atom('undefined')],
            none,
            [erl_syntax:atom(true)]
        )
    ];
optional_clause(_Schema) ->
    [].

type_guard(Type) ->
    type_guard(Type, 'Val').

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
chain_conditions([], _Operator, Acc) ->
    Acc;
chain_conditions([FunCall | Rest], Operator, Acc) ->
    NewAcc = erl_syntax:infix_expr(
        Acc,
        erl_syntax:operator(Operator),
        FunCall
    ),
    chain_conditions(Rest, Operator, NewAcc).

guard(Pred, Var) ->
    erl_syntax:application(erl_syntax:atom(Pred), [erl_syntax:variable(Var)]).

type_guard(any, Var) ->
    erl_syntax:infix_expr(
        erl_syntax:variable(Var),
        erl_syntax:operator('=/='),
        erl_syntax:atom(undefined)
    );
type_guard(ref, _Var) ->
    none;
type_guard(string, Var) ->
    guard(is_binary, Var);
type_guard(number, Var) ->
    guard(is_number, Var);
type_guard(integer, Var) ->
    guard(is_integer, Var);
type_guard(boolean, Var) ->
    guard(is_boolean, Var);
type_guard(array, Var) ->
    guard(is_list, Var);
type_guard(object, Var) ->
    guard(is_map, Var).
