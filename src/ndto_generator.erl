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

%% @private
-module(ndto_generator).

%%% INCLUDE FILES
-include("ndto_generator.hrl").

%%% EXTERNAL EXPORTS
-export([generate/2]).

%%% GENERATION EXPORTS
-export([
    is_valid/2
]).

%%% UTIL EXPORTS
-export([
    chain_conditions/3,
    chain_conditions/4,
    clauses/1,
    false_clause/2,
    false_return/2,
    guard/2,
    literal/1,
    null_clause/1,
    optional_clause/1,
    type_guard/1,
    type_guard/2
]).

%%%-----------------------------------------------------------------------------
%%% BEHAVIOUR CALLBACKS
%%%-----------------------------------------------------------------------------
-callback is_valid(Prefix, Schema) -> Result when
    Prefix :: binary(),
    Schema :: ndto:schema(),
    Result :: {IsValidFun, ExtraFuns},
    IsValidFun :: erl_syntax:syntaxTree(),
    ExtraFuns :: [erl_syntax:syntaxTree()].

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(Name, Schema) -> Result when
    Name :: atom(),
    Schema :: ndto:schema(),
    Result :: ndto:t().
generate(Name, Schema) ->
    ModuleHeader = erl_syntax:comment(?COPYRIGHT ++ [?NOTE]),
    ModuleAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Name)]),
    ExportHeader = erl_syntax:comment([?EXPORTS_HEADER]),
    ExportAttr = erl_syntax:attribute(erl_syntax:atom(export), [
        erl_syntax:list([
            erl_syntax:arity_qualifier(erl_syntax:atom(is_valid), erl_syntax:integer(1))
        ])
    ]),
    ExportHeader2 = erl_syntax:comment([?CLINE, ?EXPORTS_HEADER, ?CLINE]),

    {IsValidFun, ExtraFuns} = is_valid(<<"$">>, Schema),
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
%%% GENERATION EXPORTS
%%%-----------------------------------------------------------------------------
-spec is_valid(Prefix, Schema) -> Result when
    Prefix :: binary(),
    Schema :: ndto:schema(),
    Result :: {IsValidFun, ExtraFuns},
    IsValidFun :: erl_syntax:syntaxTree(),
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid(Prefix, false) ->
    ndto_generator_empty:is_valid(Prefix, false);
is_valid(Prefix, #{ref := _Ref} = Schema) ->
    ndto_generator_ref:is_valid(Prefix, Schema);
is_valid(Prefix, #{enum := _Enum} = Schema) ->
    ndto_generator_enum:is_valid(Prefix, Schema);
is_valid(Prefix, #{type := string} = Schema) ->
    ndto_generator_string:is_valid(Prefix, Schema);
is_valid(Prefix, #{type := integer} = Schema) ->
    ndto_generator_integer:is_valid(Prefix, Schema);
is_valid(Prefix, #{type := float} = Schema) ->
    ndto_generator_float:is_valid(Prefix, Schema);
is_valid(Prefix, #{type := boolean} = Schema) ->
    ndto_generator_boolean:is_valid(Prefix, Schema);
is_valid(Prefix, #{type := array} = Schema) ->
    ndto_generator_array:is_valid(Prefix, Schema);
is_valid(Prefix, #{type := object} = Schema) ->
    ndto_generator_object:is_valid(Prefix, Schema);
is_valid(Prefix, #{one_of := _Subschemas} = Schema) when is_list(_Subschemas) ->
    ndto_generator_symmetric_difference:is_valid(Prefix, Schema);
is_valid(Prefix, #{any_of := _Subschemas} = Schema) when is_list(_Subschemas) ->
    ndto_generator_union:is_valid(Prefix, Schema);
is_valid(Prefix, #{all_of := _Subschemas} = Schema) when is_list(_Subschemas) ->
    ndto_generator_intersection:is_valid(Prefix, Schema);
is_valid(Prefix, #{'not' := _Subschema} = Schema) ->
    ndto_generator_complement:is_valid(Prefix, Schema);
is_valid(Prefix, _Schema) ->
    ndto_generator_universal:is_valid(Prefix, _Schema).

%%%-----------------------------------------------------------------------------
%%% UTIL EXPORTS
%%%-----------------------------------------------------------------------------
chain_conditions(FunName, ValidationConditions, Operator) ->
    chain_conditions(FunName, ValidationConditions, Operator, false).

chain_conditions(FunName, ValidationConditions, 'andalso', false) ->
    [
        erl_syntax:case_expr(
            erlang:hd(chain_conditions(FunName, ValidationConditions, 'andalso', true)),
            [
                erl_syntax:clause(
                    [erl_syntax:atom('true')],
                    none,
                    [erl_syntax:atom('true')]
                ),
                erl_syntax:clause(
                    [
                        erl_syntax:tuple([
                            erl_syntax:atom('false'),
                            erl_syntax:tuple([
                                erl_syntax:variable('Reason'),
                                erl_syntax:variable('_N')
                            ])
                        ])
                    ],
                    none,
                    [
                        erl_syntax:tuple([
                            erl_syntax:atom('false'),
                            erl_syntax:variable('Reason')
                        ])
                    ]
                )
            ]
        )
    ];
chain_conditions(FunName, ValidationConditions, 'xor', false) ->
    chain_conditions(FunName, ValidationConditions, 'xor', true);
chain_conditions(_FunName, ValidationConditions, Operator, true) ->
    [
        erl_syntax:application(
            erl_syntax:atom(ndto_validation),
            erl_syntax:atom(
                erlang:atom_to_list(Operator)
            ),
            [
                erl_syntax:list(ValidationConditions)
            ]
        )
    ].

clauses(Clauses) ->
    lists:filter(fun(Clause) -> Clause =/= undefined end, Clauses).

false_clause(FunName, ErrorMessage) ->
    erl_syntax:clause(
        [erl_syntax:variable('_Val')],
        none,
        [false_return(FunName, ErrorMessage)]
    ).

false_return(FunName, ErrorMessage) ->
    erl_syntax:tuple([
        erl_syntax:atom('false'),
        erl_syntax:tuple([
            erl_syntax:atom(erlang:binary_to_list(FunName)),
            erl_syntax:binary([
                erl_syntax:binary_field(
                    erl_syntax:string(ErrorMessage)
                )
            ])
        ])
    ]).

guard(Pred, Var) ->
    erl_syntax:application(erl_syntax:atom(Pred), [erl_syntax:variable(Var)]).

literal(null) ->
    erl_syntax:atom(null);
literal(Val) when is_boolean(Val) ->
    erl_syntax:atom(Val);
literal(Val) when is_integer(Val) ->
    erl_syntax:integer(Val);
literal(Val) when is_float(Val) ->
    erl_syntax:float(Val);
literal(Val) when is_binary(Val) ->
    erl_syntax:binary([
        erl_syntax:binary_field(erl_syntax:string(erlang:binary_to_list(Val)))
    ]);
literal(Val) when is_list(Val) ->
    erl_syntax:list([literal(V) || V <- Val]);
literal(Val) when is_map(Val) ->
    erl_syntax:map_expr([
        erl_syntax:map_field_exact(literal(K), literal(V))
     || {K, V} <- maps:to_list(Val)
    ]).

null_clause(#{nullable := true}) ->
    erl_syntax:clause(
        [erl_syntax:variable('Val')],
        erl_syntax:infix_expr(
            erl_syntax:variable('Val'),
            erl_syntax:operator('=:='),
            erl_syntax:atom(null)
        ),
        [erl_syntax:atom(true)]
    );
null_clause(_Schema) ->
    undefined.

optional_clause(#{optional := true}) ->
    erl_syntax:clause(
        [erl_syntax:atom('undefined')],
        none,
        [erl_syntax:atom(true)]
    );
optional_clause(_Schema) ->
    undefined.

type_guard(Type) ->
    type_guard(Type, 'Val').

type_guard(string, Var) ->
    guard(is_binary, Var);
type_guard(float, Var) ->
    guard(is_float, Var);
type_guard(integer, Var) ->
    guard(is_integer, Var);
type_guard(boolean, Var) ->
    guard(is_boolean, Var);
type_guard(array, Var) ->
    guard(is_list, Var);
type_guard(object, Var) ->
    guard(is_map, Var).
