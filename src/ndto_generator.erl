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

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-if(?OTP_RELEASE < 26).
-spec generate(Name, Schema) -> Result when
    Name :: atom(),
    Schema :: ndto:schema(),
    Result :: ndto:t().
generate(Name, Schema) ->
    ModuleHeader = erl_syntax:comment(?COPYRIGHT ++ [?NOTE]),
    ModuleAttr = erl_syntax:attribute(erl_syntax:atom(module), [erl_syntax:atom(Name)]),
    CompileAttr = erl_syntax:attribute(
        erl_syntax:atom('compile'),
        [erl_syntax:list([erl_syntax:atom('nowarn_unused_function')])]
    ),
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
                CompileAttr,
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
%%% GENERATION FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec is_valid(Prefix, Schema) -> Result when
    Prefix :: binary(),
    Schema :: ndto:schema(),
    Result :: {IsValidFun, ExtraFuns},
    IsValidFun :: erl_syntax:syntaxTree(),
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid(Prefix, false) ->
    FunName = <<Prefix/binary, "false">>,
    FalseClause = false_clause(FunName),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [FalseClause]
    ),
    {Fun, []};
is_valid(Prefix, #{ref := Ref} = Schema) ->
    FunName = <<Prefix/binary, "_ref_", Ref/binary>>,
    DTO = erlang:binary_to_atom(Ref),
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [
                erl_syntax:application(
                    erl_syntax:atom(DTO),
                    erl_syntax:atom(is_valid),
                    [
                        erl_syntax:variable('Val')
                    ]
                )
            ]
        ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, []};
is_valid(Prefix, #{enum := Enum} = Schema) ->
    FunName = <<Prefix/binary>>,
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClauses = lists:map(
        fun(EnumVal) ->
            erl_syntax:clause(
                [literal(EnumVal)],
                none,
                [erl_syntax:atom(true)]
            )
        end,
        Enum
    ),
    FalseClause = false_clause(FunName),
    Clauses = clauses(lists:flatten([OptionalClause, NullClause, TrueClauses, FalseClause])),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, []};
is_valid(Prefix, #{type := string} = Schema) ->
    FunName = <<Prefix/binary>>,
    ExtraFuns =
        lists:foldl(
            fun(Keyword, Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    Value ->
                        case is_valid_string(<<FunName/binary, "_">>, Keyword, Value) of
                            undefined ->
                                Acc;
                            Fun ->
                                [Fun | Acc]
                        end
                end
            end,
            [],
            [
                min_length,
                max_length,
                format,
                pattern
            ]
        ),
    BodyFunPieces = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(Fun),
                    erl_syntax:integer(erl_syntax:function_arity(Fun))
                )
            ),
            erl_syntax:variable('Val')
        ])
     || Fun <- ExtraFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(string),
            chain_conditions({mfa_call, BodyFunPieces}, 'andalso')
        ),
    FalseClause = false_clause(FunName),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{type := integer} = Schema) ->
    FunName = <<Prefix/binary>>,
    ExtraFuns = lists:foldl(
        fun(Keyword, Acc) ->
            case maps:get(Keyword, Schema, undefined) of
                undefined ->
                    Acc;
                Value ->
                    case
                        is_valid_number(integer, <<FunName/binary, "_">>, Keyword, Value, Schema)
                    of
                        undefined ->
                            Acc;
                        NewIsValidFun ->
                            [NewIsValidFun | Acc]
                    end
            end
        end,
        [],
        [
            minimum,
            maximum,
            multipleOf
        ]
    ),
    BodyFunPieces = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(Fun),
                    erl_syntax:integer(erl_syntax:function_arity(Fun))
                )
            ),
            erl_syntax:variable('Val')
        ])
     || Fun <- ExtraFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(integer),
            chain_conditions({mfa_call, BodyFunPieces}, 'andalso')
        ),
    FalseClause = false_clause(FunName),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{type := float} = Schema) ->
    FunName = <<Prefix/binary, "float">>,
    ExtraFuns = lists:foldl(
        fun(Keyword, Acc) ->
            case maps:get(Keyword, Schema, undefined) of
                undefined ->
                    Acc;
                Value ->
                    case is_valid_number(float, <<FunName/binary, "_">>, Keyword, Value, Schema) of
                        undefined ->
                            Acc;
                        NewIsValidFun ->
                            [NewIsValidFun | Acc]
                    end
            end
        end,
        [],
        [
            minimum,
            maximum,
            multipleOf
        ]
    ),
    BodyFunPieces = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(Fun),
                    erl_syntax:integer(erl_syntax:function_arity(Fun))
                )
            ),
            erl_syntax:variable('Val')
        ])
     || Fun <- ExtraFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(float),
            chain_conditions({mfa_call, BodyFunPieces}, 'andalso')
        ),
    FalseClause = false_clause(FunName),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{type := boolean} = Schema) ->
    FunName = <<Prefix/binary>>,
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(boolean),
            [erl_syntax:atom(true)]
        ),
    FalseClause = false_clause(FunName),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, []};
is_valid(Prefix, #{type := array} = Schema) ->
    FunName = <<Prefix/binary>>,
    {IsValidFuns, ExtraFuns} =
        lists:foldl(
            fun(Keyword, {IsValidFunsAcc, ExtraFunsAcc} = Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    _Value ->
                        case is_valid_array(<<FunName/binary, "[*]">>, Keyword, Schema) of
                            {undefined, _EmptyList} ->
                                Acc;
                            {NewIsValidFun, NewExtraFuns} ->
                                {
                                    [NewIsValidFun | IsValidFunsAcc],
                                    NewExtraFuns ++ ExtraFunsAcc
                                }
                        end
                end
            end,
            {[], []},
            [
                items,
                min_items,
                max_items,
                unique_items
            ]
        ),
    BodyFunPieces = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(Fun),
                    erl_syntax:integer(erl_syntax:function_arity(Fun))
                )
            ),
            erl_syntax:variable('Val')
        ])
     || Fun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(array),
            chain_conditions({mfa_call, BodyFunPieces}, 'andalso')
        ),
    FalseClause = false_clause(FunName),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{type := object} = Schema) ->
    FunName = <<Prefix/binary, ".">>,
    {IsValidFuns, ExtraFuns} =
        lists:foldl(
            fun(Keyword, {IsValidFunsAcc, ExtraFunsAcc} = Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    _Value ->
                        case is_valid_object(<<FunName/binary>>, Keyword, Schema) of
                            {undefined, _EmptyList} ->
                                Acc;
                            {NewIsValidFun, NewExtraFuns} ->
                                {
                                    [NewIsValidFun | IsValidFunsAcc],
                                    NewExtraFuns ++ ExtraFunsAcc
                                }
                        end
                end
            end,
            {[], []},
            [
                properties,
                required,
                min_properties,
                max_properties,
                pattern_properties,
                additional_properties
            ]
        ),
    BodyFunPieces = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(Fun),
                    erl_syntax:integer(erl_syntax:function_arity(Fun))
                )
            ),
            erl_syntax:variable('Val')
        ])
     || Fun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(object),
            chain_conditions({mfa_call, BodyFunPieces}, 'andalso')
        ),
    FalseClause = false_clause(FunName),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{one_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "_one_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {Idx, IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "[", (erlang:integer_to_binary(Idx))/binary, "]">>, Subschema
            ),
            {
                Idx + 1,
                [IsValidFun | IsValidFunsAcc],
                ExtraFuns ++ ExtraFunsAcc
            }
        end,
        {0, [], []},
        Subschemas
    ),
    BodyFunPieces = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(Fun),
                    erl_syntax:integer(erl_syntax:function_arity(Fun))
                )
            ),
            erl_syntax:variable('Val')
        ])
     || Fun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            chain_conditions({mfa_call, BodyFunPieces}, 'xor')
        ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{any_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "_any_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {RawIdx, IsValidFunsAcc, ExtraFunsAcc}) ->
            Idx = erlang:integer_to_binary(RawIdx),
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "[", Idx/binary, "]">>, Subschema
            ),
            {
                RawIdx + 1,
                [IsValidFun | IsValidFunsAcc],
                ExtraFuns ++ ExtraFunsAcc
            }
        end,
        {0, [], []},
        Subschemas
    ),
    BodyFunPieces = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(Fun),
                    erl_syntax:integer(erl_syntax:function_arity(Fun))
                )
            ),
            erl_syntax:variable('Val')
        ])
     || Fun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            chain_conditions({mfa_call, BodyFunPieces}, 'orelse')
        ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{all_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "_all_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {RawIdx, IsValidFunsAcc, ExtraFunsAcc}) ->
            Idx = erlang:integer_to_binary(RawIdx),
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "[", Idx/binary, "]">>, Subschema
            ),
            {
                RawIdx + 1,
                [IsValidFun | IsValidFunsAcc],
                ExtraFuns ++ ExtraFunsAcc
            }
        end,
        {0, [], []},
        Subschemas
    ),
    BodyFunPieces = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(Fun),
                    erl_syntax:integer(erl_syntax:function_arity(Fun))
                )
            ),
            erl_syntax:variable('Val')
        ])
     || Fun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            chain_conditions({mfa_call, BodyFunPieces}, 'andalso')
        ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{'not' := Subschema} = Schema) ->
    FunName = <<Prefix/binary, "not">>,
    {IsValidFun, ExtraFuns} = is_valid(<<FunName/binary, "_">>, Subschema),
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:function_name(IsValidFun),
                    [erl_syntax:variable('Val')]
                ),
                [
                    erl_syntax:clause(
                        [erl_syntax:atom('true')],
                        none,
                        [erl_syntax:atom('false')]
                    ),
                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:variable('_')
                            ])
                        ],
                        none,
                        [erl_syntax:atom('true')]
                    )
                ]
            )
        ]
    ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, [IsValidFun | ExtraFuns]};
is_valid(Prefix, _Schema) ->
    FunName = <<Prefix/binary, "any">>,
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('_Val')],
            none,
            [erl_syntax:atom(true)]
        ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, []}.
-else.
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
%%% GENERATION FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec is_valid(Prefix, Schema) -> Result when
    Prefix :: binary(),
    Schema :: ndto:schema(),
    Result :: {IsValidFun, ExtraFuns},
    IsValidFun :: erl_syntax:syntaxTree(),
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid(Prefix, #{ref := Ref} = Schema) ->
    FunName = <<Prefix/binary, "_ref_", Ref/binary>>,
    DTO = erlang:binary_to_atom(Ref),
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [
                erl_syntax:case_expr(
                    erl_syntax:application(
                        erl_syntax:atom(DTO),
                        erl_syntax:atom(is_valid),
                        [
                            erl_syntax:variable('Val')
                        ]
                    ),
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
                                        erl_syntax:variable('RefPrefix'),
                                        erl_syntax:variable('RefReason')
                                    ])
                                ])
                            ],
                            none,
                            [
                                erl_syntax:tuple([
                                    erl_syntax:atom('false'),
                                    erl_syntax:tuple([
                                        erl_syntax:application(
                                            erl_syntax:atom('binary_to_atom'),
                                            [
                                                erl_syntax:binary([
                                                    erl_syntax:binary_field(
                                                        erl_syntax:string(binary_to_list(Prefix)),
                                                        []
                                                    ),
                                                    erl_syntax:binary_field(
                                                        erl_syntax:application(
                                                            erl_syntax:atom('atom_to_binary'),
                                                            [erl_syntax:variable('RefPrefix')]
                                                        ),
                                                        [erl_syntax:atom('binary')]
                                                    )
                                                ])
                                            ]
                                        ),
                                        erl_syntax:variable('RefReason')
                                    ])
                                ])
                            ]
                        )
                    ]
                )
                
            ]
        ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, []};
is_valid(Prefix, #{enum := Enum} = Schema) ->
    FunName = <<Prefix/binary>>,
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClauses = lists:map(
        fun(EnumVal) ->
            erl_syntax:clause(
                [literal(EnumVal)],
                none,
                [erl_syntax:atom(true)]
            )
        end,
        Enum
    ),
    FalseClause = false_clause(FunName, "valid for this enum field"),
    Clauses = clauses(lists:flatten([OptionalClause, NullClause, TrueClauses, FalseClause])),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, []};
is_valid(Prefix, #{type := string} = Schema) ->
    FunName = <<Prefix/binary>>,
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(string),
            [erl_syntax:atom('true')]
        ),
    FalseClause = false_clause(<<FunName/binary>>, "string"),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    TypeFun =
        erl_syntax:function(
            erl_syntax:atom(erlang:binary_to_atom(<<FunName/binary, "_string">>)),
            Clauses
        ),
    ExtraFuns =
        [
            TypeFun
            | lists:foldl(
                fun(Keyword, Acc) ->
                    case maps:get(Keyword, Schema, undefined) of
                        undefined ->
                            Acc;
                        Value ->
                            case is_valid_string(<<FunName/binary, "_">>, Keyword, Value) of
                                undefined ->
                                    Acc;
                                Fun ->
                                    [Fun | Acc]
                            end
                    end
                end,
                [],
                [
                    min_length,
                    max_length,
                    format,
                    pattern
                ]
            )
        ],
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- ExtraFuns
    ],
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [
            erl_syntax:clause(
                [erl_syntax:variable('Val')],
                none,
                chain_conditions({mfa_call, BodyFunCalls}, 'andalso')
            )
        ]
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{type := integer} = Schema) ->
    FunName = <<Prefix/binary>>,
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(integer),
            [erl_syntax:atom('true')]
        ),
    FalseClause = false_clause(FunName, "integer"),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    TypeFun =
        erl_syntax:function(
            erl_syntax:atom(erlang:binary_to_atom(<<FunName/binary, "_integer">>)),
            Clauses
        ),
    ExtraFuns = [
        TypeFun
        | lists:foldl(
            fun(Keyword, Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    Value ->
                        case is_valid_number(integer, <<FunName/binary>>, Keyword, Value, Schema) of
                            undefined ->
                                Acc;
                            NewIsValidFun ->
                                [NewIsValidFun | Acc]
                        end
                end
            end,
            [],
            [
                minimum,
                maximum,
                multipleOf
            ]
        )
    ],
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- ExtraFuns
    ],

    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [
            erl_syntax:clause(
                [erl_syntax:variable('Val')],
                none,
                chain_conditions({mfa_call, BodyFunCalls}, 'andalso')
            )
        ]
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{type := float} = Schema) ->
    FunName = <<Prefix/binary>>,
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(float),
            [erl_syntax:atom('true')]
        ),
    FalseClause = false_clause(FunName, "float"),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    TypeFun =
        erl_syntax:function(
            erl_syntax:atom(erlang:binary_to_atom(<<FunName/binary, "_float">>)),
            Clauses
        ),
    ExtraFuns = [
        TypeFun
        | lists:foldl(
            fun(Keyword, Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    Value ->
                        case
                            is_valid_number(float, <<FunName/binary, "_">>, Keyword, Value, Schema)
                        of
                            undefined ->
                                Acc;
                            NewIsValidFun ->
                                [NewIsValidFun | Acc]
                        end
                end
            end,
            [],
            [
                minimum,
                maximum,
                multipleOf
            ]
        )
    ],
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- ExtraFuns
    ],
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [
            erl_syntax:clause(
                [erl_syntax:variable('Val')],
                none,
                chain_conditions({mfa_call, BodyFunCalls}, 'andalso')
            )
        ]
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{type := boolean} = Schema) ->
    FunName = <<Prefix/binary>>,
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(boolean),
            [erl_syntax:atom(true)]
        ),
    FalseClause = false_clause(FunName, "boolean"),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, []};
is_valid(Prefix, #{type := array} = Schema) ->
    FunName = <<Prefix/binary>>,
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(array),
            [erl_syntax:atom(true)]
        ),
    FalseClause = false_clause(FunName, "array"),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    TypeFun =
        erl_syntax:function(
            erl_syntax:atom(erlang:binary_to_atom(<<FunName/binary, "_array">>)),
            Clauses
        ),
    {IsValidFuns0, ExtraFuns} =
        lists:foldl(
            fun(Keyword, {IsValidFunsAcc, ExtraFunsAcc} = Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    _Value ->
                        case is_valid_array(<<FunName/binary>>, Keyword, Schema) of
                            {undefined, _EmptyList} ->
                                Acc;
                            {NewIsValidFun, NewExtraFuns} ->
                                {
                                    [NewIsValidFun | IsValidFunsAcc],
                                    NewExtraFuns ++ ExtraFunsAcc
                                }
                        end
                end
            end,
            {[], []},
            [
                items,
                min_items,
                max_items,
                unique_items
            ]
        ),
    IsValidFuns = [TypeFun | IsValidFuns0],
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- IsValidFuns
    ],
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [
            erl_syntax:clause(
                [erl_syntax:variable('Val')],
                none,
                chain_conditions({mfa_call, BodyFunCalls}, 'andalso')
            )
        ]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{type := object} = Schema) ->
    FunName = <<Prefix/binary, ".">>,
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(object),
            [erl_syntax:atom('true')]
        ),
    FalseClause = false_clause(FunName, "object"),
    Clauses = clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    TypeFun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(<<FunName/binary, "_object">>)),
        Clauses
    ),
    {IsValidFuns0, ExtraFuns} =
        lists:foldl(
            fun(Keyword, {IsValidFunsAcc, ExtraFunsAcc} = Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    _Value ->
                        case is_valid_object(<<FunName/binary>>, Keyword, Schema) of
                            {undefined, _EmptyList} ->
                                Acc;
                            {NewIsValidFun, NewExtraFuns} ->
                                {
                                    [NewIsValidFun | IsValidFunsAcc],
                                    NewExtraFuns ++ ExtraFunsAcc
                                }
                        end
                end
            end,
            {[], []},
            [
                properties,
                required,
                min_properties,
                max_properties,
                pattern_properties,
                additional_properties
            ]
        ),
    IsValidFuns = [TypeFun | IsValidFuns0],
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- IsValidFuns
    ],
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [
            erl_syntax:clause(
                [erl_syntax:variable('Val')],
                none,
                chain_conditions({mfa_call, BodyFunCalls}, 'andalso')
            )
        ]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{one_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "_one_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {Idx, IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "[", (erlang:integer_to_binary(Idx))/binary, "]">>, Subschema
            ),
            {
                Idx + 1,
                [IsValidFun | IsValidFunsAcc],
                ExtraFuns ++ ExtraFunsAcc
            }
        end,
        {0, [], []},
        Subschemas
    ),
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(IsValidFun),
            [erl_syntax:variable('Val')]
        )
     || IsValidFun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            chain_conditions({mfa_call, BodyFunCalls}, 'xor')
        ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{any_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "_any_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {RawIdx, IsValidFunsAcc, ExtraFunsAcc}) ->
            Idx = erlang:integer_to_binary(RawIdx),
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "[", Idx/binary, "]">>, Subschema
            ),
            {
                RawIdx + 1,
                [IsValidFun | IsValidFunsAcc],
                ExtraFuns ++ ExtraFunsAcc
            }
        end,
        {0, [], []},
        Subschemas
    ),
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(IsValidFun),
            [erl_syntax:variable('Val')]
        )
     || IsValidFun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            chain_conditions({mfa_call, BodyFunCalls}, 'orelse')
        ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{all_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "_all_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {RawIdx, IsValidFunsAcc, ExtraFunsAcc}) ->
            Idx = erlang:integer_to_binary(RawIdx),
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "[", Idx/binary, "]">>, Subschema
            ),
            {
                RawIdx + 1,
                [IsValidFun | IsValidFunsAcc],
                ExtraFuns ++ ExtraFunsAcc
            }
        end,
        {0, [], []},
        Subschemas
    ),
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(IsValidFun),
            [erl_syntax:variable('Val')]
        )
     || IsValidFun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            chain_conditions({mfa_call, BodyFunCalls}, 'andalso')
        ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{'not' := Subschema} = Schema) ->
    FunName = <<Prefix/binary, "not">>,
    {IsValidFun, ExtraFuns} = is_valid(<<FunName/binary, "_">>, Subschema),
    OptionalClause = optional_clause(Schema),
    NullClause = null_clause(Schema),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:function_name(IsValidFun),
                    [erl_syntax:variable('Val')]
                ),
                [
                    erl_syntax:clause(
                        [erl_syntax:atom('true')],
                        none,
                        [erl_syntax:atom('false')]
                    ),
                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:variable('_')
                            ])
                        ],
                        none,
                        [erl_syntax:atom('true')]
                    )
                ]
            )
        ]
    ),
    Clauses = clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, [IsValidFun | ExtraFuns]};
is_valid(Prefix, _Schema) ->
    FunName = <<Prefix/binary, "any">>,
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('_Val')],
            none,
            [erl_syntax:atom(true)]
        ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, []}.
-endif.

-spec is_valid_array(Prefix, Keyword, Value) -> Result when
    Prefix :: binary(),
    Keyword :: atom(),
    Value :: term(),
    Result :: {Fun, ExtraFuns},
    Fun :: erl_syntax:syntaxTree() | undefined,
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid_array(Prefix, items, #{items := Items}) when is_map(Items) ->
    FunName = <<Prefix/binary, "_items">>,
    {IsValidFun, ExtraFuns} = is_valid(<<Prefix/binary, "[]">>, Items),
    UndefinedClause = erl_syntax:clause(
        [erl_syntax:atom('undefined')],
        none,
        [
            erl_syntax:atom('true')
        ]
    ),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:atom(ndto_utils),
                    erl_syntax:atom(mfoldl),
                    [
                        erl_syntax:fun_expr([
                            erl_syntax:clause(
                                [
                                    erl_syntax:variable('Item'),
                                    erl_syntax:variable('Acc')
                                ],
                                none,
                                [
                                    erl_syntax:tuple([
                                        erl_syntax:application(
                                            erl_syntax:function_name(IsValidFun),
                                            [erl_syntax:variable('Item')]
                                        ),
                                        erl_syntax:infix_expr(
                                            erl_syntax:variable('Acc'),
                                            erl_syntax:operator('+'),
                                            erl_syntax:integer(1)
                                        )
                                    ])
                                ]
                            )
                        ]),
                        erl_syntax:integer(0),
                        erl_syntax:variable('Val')
                    ]
                ),
                [
                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('true'),
                                erl_syntax:variable('_Acc')
                            ])
                        ],
                        none,
                        [
                            erl_syntax:atom('true')
                        ]
                    ),
                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:variable('Acc'),
                                erl_syntax:variable('Reason')
                            ])
                        ],
                        none,
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:tuple([
                                    erl_syntax:atom(binary_to_list(Prefix)),
                                    erl_syntax:application(
                                        erl_syntax:atom(erlang),
                                        erl_syntax:atom(list_to_binary),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom('lists'),
                                                erl_syntax:atom('flatten'),
                                                [
                                                    erl_syntax:application(
                                                        erl_syntax:atom('io_lib'),
                                                        erl_syntax:atom('format'),
                                                        [
                                                            erl_syntax:string(
                                                                lists:flatten(
                                                                    io_lib:format(
                                                                        "~s[~~p] is invalid. ~~s",
                                                                        [binary_to_list(Prefix)]
                                                                    )
                                                                )
                                                            ),
                                                            erl_syntax:list([
                                                                erl_syntax:variable('Acc'),
                                                                erl_syntax:variable('Reason')
                                                            ])
                                                        ]
                                                    )
                                                ]
                                            )
                                        ]
                                    )
                                ])
                            ])
                        ]
                    )
                ]
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [UndefinedClause, TrueClause]
    ),
    {Fun, [IsValidFun | ExtraFuns]};
is_valid_array(Prefix, items, #{items := Items} = Schema) when is_list(Items) ->
    {_Size, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Item, {Idx, IsValidFunsAcc, ExtraFunsAcc}) ->
            ItemFunName = <<Prefix/binary, "item_", (erlang:integer_to_binary(Idx))/binary, "_">>,
            {ItemIsValidFun, ItemExtraFuns} = is_valid(ItemFunName, Item),
            {Idx + 1, [{Idx, ItemIsValidFun} | IsValidFunsAcc], ItemExtraFuns ++ ExtraFunsAcc}
        end,
        {1, [], []},
        Items
    ),
    FunName = <<Prefix/binary, "items">>,
    AdditionalItems = maps:get(additional_items, Schema, true),
    {IsValidAdditionalItemsFun, AdditionalItemsExtraFuns} =
        is_valid(<<FunName/binary, "_additional_items_">>, AdditionalItems),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:match_expr(
                erl_syntax:variable('FunsMap'),
                erl_syntax:map_expr(
                    lists:map(
                        fun({Idx, IsValidFun}) ->
                            erl_syntax:map_field_assoc(
                                erl_syntax:integer(Idx),
                                erl_syntax:fun_expr(erl_syntax:function_clauses(IsValidFun))
                            )
                        end,
                        IsValidFuns
                    )
                )
            ),
            erl_syntax:application(
                erl_syntax:atom(lists),
                erl_syntax:atom(all),
                [
                    erl_syntax:fun_expr([
                        erl_syntax:clause(
                            [
                                erl_syntax:tuple([
                                    erl_syntax:variable('Item'),
                                    erl_syntax:variable('FunKey')
                                ])
                            ],
                            none,
                            [
                                erl_syntax:case_expr(
                                    erl_syntax:application(
                                        erl_syntax:atom(maps),
                                        erl_syntax:atom(get),
                                        [
                                            erl_syntax:variable('FunKey'),
                                            erl_syntax:variable('FunsMap'),
                                            erl_syntax:atom(undefined)
                                        ]
                                    ),
                                    [
                                        erl_syntax:clause(
                                            [erl_syntax:atom(undefined)],
                                            none,
                                            [
                                                erl_syntax:application(
                                                    erl_syntax:function_name(
                                                        IsValidAdditionalItemsFun
                                                    ),
                                                    [erl_syntax:variable('Item')]
                                                )
                                            ]
                                        ),
                                        erl_syntax:clause(
                                            [erl_syntax:variable('IsValidItemFun')],
                                            none,
                                            [
                                                erl_syntax:application(
                                                    erl_syntax:variable(
                                                        'IsValidItemFun'
                                                    ),
                                                    [
                                                        erl_syntax:variable(
                                                            'Item'
                                                        )
                                                    ]
                                                )
                                            ]
                                        )
                                    ]
                                )
                            ]
                        )
                    ]),
                    erl_syntax:application(
                        erl_syntax:atom(lists),
                        erl_syntax:atom(zip),
                        [
                            erl_syntax:variable('Val'),
                            erl_syntax:application(
                                erl_syntax:atom(lists),
                                erl_syntax:atom(seq),
                                [
                                    erl_syntax:integer(1),
                                    erl_syntax:application(
                                        erl_syntax:atom(erlang),
                                        erl_syntax:atom(length),
                                        [erl_syntax:variable('Val')]
                                    )
                                ]
                            )
                        ]
                    )
                ]
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, ExtraFuns ++ [IsValidAdditionalItemsFun | AdditionalItemsExtraFuns]};
is_valid_array(Prefix, min_items, #{min_items := MinItems}) ->
    FunName = <<Prefix/binary, "_min_items">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        erl_syntax:infix_expr(
            erl_syntax:application(erl_syntax:atom(length), [erl_syntax:variable('Val')]),
            erl_syntax:operator('>='),
            erl_syntax:integer(MinItems)
        ),
        [erl_syntax:atom(true)]
    ),
    FalseClause = false_clause(FunName, lists:flatten(io_lib:format("array with at least ~p items", [MinItems]))),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    ),
    {Fun, []};
is_valid_array(Prefix, max_items, #{max_items := MaxItems}) ->
    FunName = <<Prefix/binary, "max_items">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        erl_syntax:infix_expr(
            erl_syntax:application(erl_syntax:atom(length), [erl_syntax:variable('Val')]),
            erl_syntax:operator('=<'),
            erl_syntax:integer(MaxItems)
        ),
        [erl_syntax:atom(true)]
    ),
    FalseClause = false_clause(FunName, lists:flatten(io_lib:format("array with at most ~p items", [MaxItems]))),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    ),
    {Fun, []};
is_valid_array(Prefix, unique_items, #{unique_items := true}) ->
    FunName = <<Prefix/binary, "_unique_items">>,
    UndefinedClause = erl_syntax:clause(
        [erl_syntax:atom('undefined')],
        none,
        [
            erl_syntax:atom('true')
        ]
    ),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:infix_expr(
                erl_syntax:application(erl_syntax:atom(length), [erl_syntax:variable('Val')]),
                erl_syntax:operator('=:='),
                erl_syntax:application(
                    erl_syntax:atom(sets),
                    erl_syntax:atom(size),
                    [
                        erl_syntax:application(
                            erl_syntax:atom(sets),
                            erl_syntax:atom(from_list),
                            [erl_syntax:variable('Val')]
                        )
                    ]
                )
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [UndefinedClause, TrueClause]
    ),
    {Fun, []};
is_valid_array(_Prefix, unique_items, #{unique_items := false}) ->
    {undefined, []}.

-spec is_valid_number(Type, Prefix, Keyword, Value, Schema) -> Result when
    Type :: integer | float,
    Prefix :: binary(),
    Keyword :: atom(),
    Value :: term(),
    Schema :: ndto:integer_schema() | ndto:float_schema(),
    Result :: undefined | erl_syntax:syntaxTree().
is_valid_number(_Type, Prefix, minimum, Minimum, Schema) ->
    FunName = <<Prefix/binary, "_minimum">>,
    MinimumSt =
        case Minimum of
            Integer when is_integer(Integer) ->
                erl_syntax:integer(Minimum);
            _Float ->
                erl_syntax:float(Minimum)
        end,
    ExclusiveMinimum = maps:get(exclusive_minimum, Schema, false),
    Operator =
        case ExclusiveMinimum of
            true ->
                erl_syntax:operator('>');
            _false ->
                erl_syntax:operator('>=')
        end,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        erl_syntax:infix_expr(
            erl_syntax:variable('Val'),
            Operator,
            MinimumSt
        ),
        [erl_syntax:atom(true)]
    ),
    ComparisonTerm =
        case ExclusiveMinimum of
            true ->
                "than";
            false ->
                "or equal to"
        end,
    FalseClause = false_clause(
        Prefix, lists:flatten(io_lib:format("number greater ~s ~p", [ComparisonTerm, Minimum]))
    ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    );
is_valid_number(_Type, Prefix, maximum, Maximum, Schema) ->
    FunName = <<Prefix/binary, "_maximum">>,
    MaximumSt =
        case Maximum of
            Integer when is_integer(Integer) ->
                erl_syntax:integer(Maximum);
            _Float ->
                erl_syntax:float(Maximum)
        end,
    ExclusiveMaximum = maps:get(exclusive_maximum, Schema, false),
    Operator =
        case ExclusiveMaximum of
            true ->
                erl_syntax:operator('<');
            _false ->
                erl_syntax:operator('=<')
        end,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        erl_syntax:infix_expr(
            erl_syntax:variable('Val'),
            Operator,
            MaximumSt
        ),
        [erl_syntax:atom(true)]
    ),
    ComparisonTerm =
        case ExclusiveMaximum of
            true ->
                "than";
            false ->
                "or equal to"
        end,
    FalseClause = false_clause(
        FunName, lists:flatten(io_lib:format("number lower ~s ~p", [ComparisonTerm, Maximum]))
    ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    );
is_valid_number(integer, Prefix, multiple_of, MultipleOf, _Schema) ->
    FunName = <<Prefix/binary, "multiple_of">>,
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [
                erl_syntax:infix_expr(
                    erl_syntax:infix_expr(
                        erl_syntax:variable('Val'),
                        erl_syntax:operator('rem'),
                        erl_syntax:integer(MultipleOf)
                    ),
                    erl_syntax:operator('=:='),
                    erl_syntax:integer(0)
                )
            ]
        ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    );
is_valid_number(_Number, _Prefix, multiple_of, _Value, _Schema) ->
    undefined.

-spec is_valid_object(Prefix, Keyword, Schema) -> Result when
    Prefix :: binary(),
    Keyword :: atom(),
    Schema :: ndto:object_schema(),
    Result :: {Fun, ExtraFuns},
    Fun :: erl_syntax:syntaxTree() | undefined,
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid_object(Prefix, properties, #{properties := Properties}) ->
    FunName = <<Prefix/binary, "*">>,
    {PropertiesFuns, ExtraFuns} = maps:fold(
        fun(PropertyName, Property, {IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidPropertyFun, ExtraPropertyFuns} =
                is_valid(<<Prefix/binary, PropertyName/binary>>, Property#{
                    optional => true
                }),
            {
                [{PropertyName, IsValidPropertyFun} | IsValidFunsAcc],
                ExtraFunsAcc ++ ExtraPropertyFuns
            }
        end,
        {[], []},
        Properties
    ),
    FunPieces = object_properties_fun_pieces(PropertiesFuns),
    FunBody = chain_conditions({mfa_call, FunPieces}, 'andalso'),

    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        FunBody
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {_PropertyNames, IsValidFuns} = lists:unzip(PropertiesFuns),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid_object(Prefix, required, #{required := Required}) ->
    FunName = <<Prefix/binary, "_required">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:atom(ndto_utils),
                    erl_syntax:atom(find),
                    [
                        erl_syntax:fun_expr([
                            erl_syntax:clause(
                                [erl_syntax:variable('Property')],
                                none,
                                [
                                    erl_syntax:prefix_expr(
                                        erl_syntax:operator('not'),
                                        erl_syntax:application(
                                            erl_syntax:atom(maps),
                                            erl_syntax:atom(is_key),
                                            [
                                                erl_syntax:variable('Property'),
                                                erl_syntax:variable('Val')
                                            ]
                                        )
                                    )
                                ]
                            )
                        ]),
                        erl_syntax:list(
                            lists:map(
                                fun(PropertyName) ->
                                    erl_syntax:binary([
                                        erl_syntax:binary_field(
                                            erl_syntax:string(erlang:binary_to_list(PropertyName))
                                        )
                                    ])
                                end,
                                Required
                            )
                        )
                    ]
                ),
                [
                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:atom('none')
                            ])
                        ],
                        none,
                        [erl_syntax:atom('true')]
                    ),
                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('true'),
                                erl_syntax:variable('MissingProperty')
                            ])
                        ],
                        none,
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:tuple([
                                    erl_syntax:atom(binary_to_list(Prefix)),
                                    erl_syntax:application(
                                        erl_syntax:atom(erlang),
                                        erl_syntax:atom(list_to_binary),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom('lists'),
                                                erl_syntax:atom('flatten'),
                                                [
                                                    erl_syntax:application(
                                                        erl_syntax:atom('io_lib'),
                                                        erl_syntax:atom('format'),
                                                        [
                                                            erl_syntax:string(
                                                                lists:flatten(
                                                                    io_lib:format(
                                                                        "~s is missing required property ~~p",
                                                                        [binary_to_list(Prefix)]
                                                                    )
                                                                )
                                                            ),
                                                            erl_syntax:list([
                                                                erl_syntax:variable('MissingProperty')
                                                            ])
                                                        ]
                                                    )
                                                ]
                                            )
                                        ]
                                    )
                                ])
                            ]
                        )]
                    )
                ]
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, []};
is_valid_object(Prefix, min_properties, #{min_properties := MinProperties}) ->
    FunName = <<Prefix/binary, "_min_properties">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:infix_expr(
                erl_syntax:application(
                    erl_syntax:atom(erlang),
                    erl_syntax:atom(length),
                    [
                        erl_syntax:application(
                            erl_syntax:atom(maps),
                            erl_syntax:atom(keys),
                            [erl_syntax:variable('Val')]
                        )
                    ]
                ),
                erl_syntax:operator('>='),
                erl_syntax:integer(MinProperties)
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, []};
is_valid_object(Prefix, max_properties, #{max_properties := MaxProperties}) ->
    FunName = <<Prefix/binary, "_max_properties">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:infix_expr(
                erl_syntax:application(
                    erl_syntax:atom(erlang),
                    erl_syntax:atom(length),
                    [
                        erl_syntax:application(
                            erl_syntax:atom(maps),
                            erl_syntax:atom(keys),
                            [erl_syntax:variable('Val')]
                        )
                    ]
                ),
                erl_syntax:operator('=<'),
                erl_syntax:integer(MaxProperties)
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, []};
is_valid_object(Prefix, pattern_properties, #{pattern_properties := PatternProperties}) ->
    FunName = <<Prefix/binary, "_pattern_properties">>,
    {IsValidPatterns, ExtraFuns} = lists:foldl(
        fun({PropertyPattern, PropertySchema}, {IsValidPatternsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = is_valid(
                <<Prefix/binary, PropertyPattern/binary>>, PropertySchema
            ),
            {[{PropertyPattern, IsValidFun} | IsValidPatternsAcc], ExtraFuns ++ ExtraFunsAcc}
        end,
        {[], []},
        maps:to_list(PatternProperties)
    ),
    Conditions = lists:map(
        fun({PropertyPattern, IsValidFun}) ->
            Filter =
                erl_syntax:application(
                    erl_syntax:atom(lists),
                    erl_syntax:atom(filter),
                    [
                        erl_syntax:fun_expr([
                            erl_syntax:clause(
                                [
                                    erl_syntax:tuple([
                                        erl_syntax:variable('PropertyName'),
                                        erl_syntax:variable('_PropertyValue')
                                    ])
                                ],
                                none,
                                [
                                    erl_syntax:case_expr(
                                        erl_syntax:application(
                                            erl_syntax:atom(re),
                                            erl_syntax:atom(run),
                                            [
                                                erl_syntax:variable('PropertyName'),
                                                erl_syntax:binary([
                                                    erl_syntax:binary_field(
                                                        erl_syntax:string(
                                                            erlang:binary_to_list(
                                                                PropertyPattern
                                                            )
                                                        )
                                                    )
                                                ])
                                            ]
                                        ),
                                        [
                                            erl_syntax:clause(
                                                [
                                                    erl_syntax:tuple([
                                                        erl_syntax:atom(match),
                                                        erl_syntax:variable('_Captured')
                                                    ])
                                                ],
                                                none,
                                                [erl_syntax:atom(true)]
                                            ),
                                            erl_syntax:clause(
                                                [erl_syntax:variable('_nomatch')],
                                                none,
                                                [erl_syntax:atom(false)]
                                            )
                                        ]
                                    )
                                ]
                            )
                        ]),
                        erl_syntax:application(
                            erl_syntax:atom(maps),
                            erl_syntax:atom(to_list),
                            [erl_syntax:variable('Val')]
                        )
                    ]
                ),
            FunBody =
                erl_syntax:case_expr(
                    erl_syntax:application(
                        erl_syntax:atom(lists),
                        erl_syntax:atom(all),
                        [
                            erl_syntax:fun_expr([
                                erl_syntax:clause(
                                    [
                                        erl_syntax:tuple([
                                            erl_syntax:variable('_PropertyName'),
                                            erl_syntax:variable('PropertyValue')
                                        ])
                                    ],
                                    none,
                                    [
                                        erl_syntax:infix_expr(
                                            erl_syntax:atom('true'),
                                            erl_syntax:operator('=='),
                                            erl_syntax:application(
                                                erl_syntax:function_name(IsValidFun),
                                                [erl_syntax:variable('PropertyValue')]
                                            )
                                        )
                                    ]
                                )
                            ]),
                            Filter
                        ]
                    ),
                    [
                        erl_syntax:clause(
                            [erl_syntax:atom('true')],
                            none,
                            [erl_syntax:atom('true')]
                        ),
                        erl_syntax:clause(
                            [erl_syntax:atom('false')],
                            none,
                            [false_return(Prefix, "value supported by the schema")]
                        )
                    ]
                ),
            erl_syntax:fun_expr([
                erl_syntax:clause(none, [FunBody])
            ])
        end,
        IsValidPatterns
    ),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        chain_conditions({fun_call, Conditions}, 'andalso')
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    IsValidFuns = [IsValidFun || {_PatternName, IsValidFun} <- IsValidPatterns],
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid_object(
    Prefix,
    additional_properties,
    #{additional_properties := false} = Schema
) ->
    FunName = <<Prefix/binary, "_additional_properties">>,
    UndefinedClause = erl_syntax:clause(
        [erl_syntax:atom('undefined')],
        none,
        [
            erl_syntax:atom('true')
        ]
    ),
    Properties = maps:get(properties, Schema, #{}),
    PatternProperties = maps:get(pattern_properties, Schema, #{}),
    PatternPropertiesList = erl_syntax:application(
        erl_syntax:atom(lists),
        erl_syntax:atom(filter),
        [
            erl_syntax:fun_expr([
                erl_syntax:clause(
                    [erl_syntax:variable('PropertyName')],
                    none,
                    [
                        erl_syntax:application(
                            erl_syntax:atom(lists),
                            erl_syntax:atom(any),
                            [
                                erl_syntax:fun_expr([
                                    erl_syntax:clause(
                                        [erl_syntax:variable('Pattern')],
                                        none,
                                        [
                                            erl_syntax:case_expr(
                                                erl_syntax:application(
                                                    erl_syntax:atom(re),
                                                    erl_syntax:atom(run),
                                                    [
                                                        erl_syntax:variable('PropertyName'),
                                                        erl_syntax:variable('Pattern')
                                                    ]
                                                ),
                                                [
                                                    erl_syntax:clause(
                                                        [
                                                            erl_syntax:tuple([
                                                                erl_syntax:atom(match),
                                                                erl_syntax:variable('_Captured')
                                                            ])
                                                        ],
                                                        none,
                                                        [erl_syntax:atom(true)]
                                                    ),
                                                    erl_syntax:clause(
                                                        [erl_syntax:variable('_nomatch')],
                                                        none,
                                                        [erl_syntax:atom(false)]
                                                    )
                                                ]
                                            )
                                        ]
                                    )
                                ]),
                                erl_syntax:list(
                                    lists:map(
                                        fun(PropertyPattern) ->
                                            erl_syntax:binary([
                                                erl_syntax:binary_field(
                                                    erl_syntax:string(
                                                        erlang:binary_to_list(PropertyPattern)
                                                    )
                                                )
                                            ])
                                        end,
                                        maps:keys(PatternProperties)
                                    )
                                )
                            ]
                        )
                    ]
                )
            ]),
            erl_syntax:application(
                erl_syntax:atom(maps),
                erl_syntax:atom(keys),
                [erl_syntax:variable('Val')]
            )
        ]
    ),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:atom(sets),
                    erl_syntax:atom(is_subset),
                    [
                        erl_syntax:application(
                            erl_syntax:atom(sets),
                            erl_syntax:atom(from_list),
                            [
                                erl_syntax:application(
                                    erl_syntax:atom(maps),
                                    erl_syntax:atom(keys),
                                    [erl_syntax:variable('Val')]
                                )
                            ]
                        ),
                        erl_syntax:application(
                            erl_syntax:atom(sets),
                            erl_syntax:atom(from_list),
                            [
                                erl_syntax:application(
                                    erl_syntax:atom(lists),
                                    erl_syntax:atom(append),
                                    [
                                        erl_syntax:list(
                                            lists:map(
                                                fun(PropertyName) ->
                                                    erl_syntax:binary([
                                                        erl_syntax:binary_field(
                                                            erl_syntax:string(
                                                                erlang:binary_to_list(PropertyName)
                                                            )
                                                        )
                                                    ])
                                                end,
                                                maps:keys(Properties)
                                            )
                                        ),
                                        PatternPropertiesList
                                    ]
                                )
                            ]
                        )
                    ]
                ),
                [
                    erl_syntax:clause(
                        [erl_syntax:atom('true')],
                        none,
                        [erl_syntax:atom('true')]
                    ),
                    erl_syntax:clause(
                        [erl_syntax:atom('false')],
                        none,
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:tuple([
                                    erl_syntax:atom(binary_to_list(Prefix)),
                                    erl_syntax:application(
                                        erl_syntax:atom(erlang),
                                        erl_syntax:atom(list_to_binary),
                                        [
                                            erl_syntax:string(
                                                lists:flatten(
                                                    io_lib:format(
                                                        "~s has unsupported keys",
                                                        [binary_to_list(Prefix)]
                                                    )
                                                )
                                            )
                                        ]
                                    )
                                ])
                            ])
                        ]
                    )
                ]
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [UndefinedClause, TrueClause]
    ),
    {Fun, []};
is_valid_object(
    Prefix,
    additional_properties,
    #{additional_properties := AdditionalProperties} = Schema
) ->
    FunName = <<Prefix/binary, "_additional_properties">>,
    UndefinedClause = erl_syntax:clause(
        [erl_syntax:atom('undefined')],
        none,
        [
            erl_syntax:atom('true')
        ]
    ),
    {IsValidFun, ExtraFuns} = is_valid(<<FunName/binary, "_">>, AdditionalProperties),
    Properties = maps:get(properties, Schema, #{}),
    PatternProperties = maps:get(pattern_properties, Schema, #{}),
    PatternPropertiesList = erl_syntax:application(
        erl_syntax:atom(lists),
        erl_syntax:atom(filter),
        [
            erl_syntax:fun_expr([
                erl_syntax:clause(
                    [erl_syntax:variable('PropertyName')],
                    none,
                    [
                        erl_syntax:application(
                            erl_syntax:atom(lists),
                            erl_syntax:atom(any),
                            [
                                erl_syntax:fun_expr([
                                    erl_syntax:clause(
                                        [erl_syntax:variable('Pattern')],
                                        none,
                                        [
                                            erl_syntax:case_expr(
                                                erl_syntax:application(
                                                    erl_syntax:atom(re),
                                                    erl_syntax:atom(run),
                                                    [
                                                        erl_syntax:variable('PropertyName'),
                                                        erl_syntax:variable('Pattern')
                                                    ]
                                                ),
                                                [
                                                    erl_syntax:clause(
                                                        [
                                                            erl_syntax:tuple([
                                                                erl_syntax:atom(match),
                                                                erl_syntax:variable('_Captured')
                                                            ])
                                                        ],
                                                        none,
                                                        [erl_syntax:atom(true)]
                                                    ),
                                                    erl_syntax:clause(
                                                        [erl_syntax:variable('_nomatch')],
                                                        none,
                                                        [erl_syntax:atom(false)]
                                                    )
                                                ]
                                            )
                                        ]
                                    )
                                ]),
                                erl_syntax:list(
                                    lists:map(
                                        fun(PropertyPattern) ->
                                            erl_syntax:binary([
                                                erl_syntax:binary_field(
                                                    erl_syntax:string(
                                                        erlang:binary_to_list(PropertyPattern)
                                                    )
                                                )
                                            ])
                                        end,
                                        maps:keys(PatternProperties)
                                    )
                                )
                            ]
                        )
                    ]
                )
            ]),
            erl_syntax:application(
                erl_syntax:atom(maps),
                erl_syntax:atom(keys),
                [erl_syntax:variable('Val')]
            )
        ]
    ),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:atom(lists),
                    erl_syntax:atom(all),
                    [
                        erl_syntax:fun_expr([
                            erl_syntax:clause(
                                [erl_syntax:variable('Property')],
                                none,
                                [
                                    erl_syntax:infix_expr(
                                        erl_syntax:atom('true'),
                                        erl_syntax:operator('=='),
                                        erl_syntax:application(
                                            erl_syntax:function_name(IsValidFun),
                                            [
                                                erl_syntax:application(
                                                    erl_syntax:atom(maps),
                                                    erl_syntax:atom(get),
                                                    [
                                                        erl_syntax:variable('Property'),
                                                        erl_syntax:variable('Val'),
                                                        erl_syntax:atom(undefined)
                                                    ]
                                                )
                                            ]
                                        )
                                    )
                                ]
                            )
                        ]),
                        erl_syntax:application(
                            erl_syntax:atom(lists),
                            erl_syntax:atom(subtract),
                            [
                                erl_syntax:application(
                                    erl_syntax:atom(maps),
                                    erl_syntax:atom(keys),
                                    [erl_syntax:variable('Val')]
                                ),
                                erl_syntax:application(
                                    erl_syntax:atom(lists),
                                    erl_syntax:atom(append),
                                    [
                                        erl_syntax:list(
                                            lists:map(
                                                fun(PropertyName) ->
                                                    erl_syntax:binary([
                                                        erl_syntax:binary_field(
                                                            erl_syntax:string(
                                                                erlang:binary_to_list(PropertyName)
                                                            )
                                                        )
                                                    ])
                                                end,
                                                maps:keys(Properties)
                                            )
                                        ),
                                        PatternPropertiesList
                                    ]
                                )
                            ]
                        )
                    ]
                ),
                [
                    erl_syntax:clause(
                        [erl_syntax:atom(true)],
                        none,
                        [erl_syntax:atom(true)]
                    ),
                    erl_syntax:clause(
                        [erl_syntax:atom(false)],
                        none,
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:tuple([
                                    erl_syntax:atom(binary_to_list(Prefix)),
                                    erl_syntax:application(
                                        erl_syntax:atom(erlang),
                                        erl_syntax:atom(list_to_binary),
                                        [
                                            erl_syntax:string(
                                                lists:flatten(
                                                    io_lib:format(
                                                        "~s has unsupported keys",
                                                        [binary_to_list(Prefix)]
                                                    )
                                                )
                                            )
                                        ]
                                    )
                                ])
                            ])
                        ]
                    )
                ]
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [UndefinedClause, TrueClause]
    ),
    {Fun, [IsValidFun | ExtraFuns]};
is_valid_object(_Prefix, additional_properties, _Schema) ->
    {undefined, []}.

-spec is_valid_string(Prefix, Keyword, Value) -> Result when
    Prefix :: binary(),
    Keyword :: atom(),
    Value :: term(),
    Result :: undefined | erl_syntax:syntaxTree().
is_valid_string(Prefix, min_length, MinLength) ->
    FunName = <<Prefix/binary, "_min_length">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:infix_expr(
                erl_syntax:application(erl_syntax:atom(string), erl_syntax:atom(length), [
                    erl_syntax:variable('Val')
                ]),
                erl_syntax:operator('>='),
                erl_syntax:integer(MinLength)
            )
        ]
    ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    );
is_valid_string(Prefix, max_length, MaxLength) ->
    FunName = <<Prefix/binary, "_max_length">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:infix_expr(
                erl_syntax:application(erl_syntax:atom(string), erl_syntax:atom(length), [
                    erl_syntax:variable('Val')
                ]),
                erl_syntax:operator('=<'),
                erl_syntax:integer(MaxLength)
            )
        ]
    ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    );
is_valid_string(Prefix, pattern, Pattern) ->
    FunName = <<Prefix/binary, "_pattern">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:atom(re),
                    erl_syntax:atom(run),
                    [
                        erl_syntax:variable('Val'),
                        erl_syntax:binary([
                            erl_syntax:binary_field(
                                erl_syntax:string(erlang:binary_to_list(Pattern))
                            )
                        ])
                    ]
                ),
                [
                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom(match), erl_syntax:variable('_Captured')
                            ])
                        ],
                        none,
                        [erl_syntax:atom(true)]
                    ),
                    erl_syntax:clause(
                        [erl_syntax:variable('_nomatch')],
                        none,
                        [erl_syntax:atom(false)]
                    )
                ]
            )
        ]
    ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    );
is_valid_string(Prefix, format, iso8601) ->
    FunName = <<Prefix/binary, "_format">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:application(
                erl_syntax:atom(ncalendar),
                erl_syntax:atom(is_valid),
                [
                    erl_syntax:atom(iso8601),
                    erl_syntax:variable('Val')
                ]
            )
        ]
    ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    );
is_valid_string(Prefix, format, base64) ->
    FunName = <<Prefix/binary, "_format">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(erl_syntax:atom(string), erl_syntax:atom(length), [
                    erl_syntax:variable('Val')
                ]),
                [
                    erl_syntax:clause(
                        [erl_syntax:variable('Length')],
                        erl_syntax:infix_expr(
                            erl_syntax:infix_expr(
                                erl_syntax:variable('Length'),
                                erl_syntax:operator('rem'),
                                erl_syntax:integer(4)
                            ),
                            erl_syntax:operator('=:='),
                            erl_syntax:integer(0)
                        ),
                        [
                            erl_syntax:match_expr(
                                erl_syntax:variable('Unpad'),
                                erl_syntax:application(
                                    erl_syntax:atom(string),
                                    erl_syntax:atom(trim),
                                    [
                                        erl_syntax:variable('Val'),
                                        erl_syntax:atom(trailing),
                                        erl_syntax:list([erl_syntax:char($=)])
                                    ]
                                )
                            ),
                            erl_syntax:application(
                                erl_syntax:atom(lists),
                                erl_syntax:atom(all),
                                [
                                    erl_syntax:fun_expr([
                                        erl_syntax:clause(
                                            [erl_syntax:integer(43)],
                                            none,
                                            [erl_syntax:atom(true)]
                                        ),
                                        erl_syntax:clause(
                                            [erl_syntax:variable('Char')],
                                            erl_syntax:infix_expr(
                                                erl_syntax:infix_expr(
                                                    erl_syntax:variable('Char'),
                                                    erl_syntax:operator('>='),
                                                    erl_syntax:integer(47)
                                                ),
                                                erl_syntax:operator('andalso'),
                                                erl_syntax:infix_expr(
                                                    erl_syntax:variable('Char'),
                                                    erl_syntax:operator('=<'),
                                                    erl_syntax:integer(57)
                                                )
                                            ),
                                            [erl_syntax:atom(true)]
                                        ),
                                        erl_syntax:clause(
                                            [erl_syntax:variable('Char')],
                                            erl_syntax:infix_expr(
                                                erl_syntax:infix_expr(
                                                    erl_syntax:variable('Char'),
                                                    erl_syntax:operator('>='),
                                                    erl_syntax:integer(65)
                                                ),
                                                erl_syntax:operator('andalso'),
                                                erl_syntax:infix_expr(
                                                    erl_syntax:variable('Char'),
                                                    erl_syntax:operator('=<'),
                                                    erl_syntax:integer(90)
                                                )
                                            ),
                                            [erl_syntax:atom(true)]
                                        ),
                                        erl_syntax:clause(
                                            [erl_syntax:variable('Char')],
                                            erl_syntax:infix_expr(
                                                erl_syntax:infix_expr(
                                                    erl_syntax:variable('Char'),
                                                    erl_syntax:operator('>='),
                                                    erl_syntax:integer(97)
                                                ),
                                                erl_syntax:operator('andalso'),
                                                erl_syntax:infix_expr(
                                                    erl_syntax:variable('Char'),
                                                    erl_syntax:operator('=<'),
                                                    erl_syntax:integer(122)
                                                )
                                            ),
                                            [erl_syntax:atom(true)]
                                        ),
                                        erl_syntax:clause(
                                            [erl_syntax:variable('_Char')],
                                            none,
                                            [erl_syntax:atom(false)]
                                        )
                                    ]),
                                    erl_syntax:application(
                                        erl_syntax:atom(string),
                                        erl_syntax:atom(to_graphemes),
                                        [erl_syntax:variable('Unpad')]
                                    )
                                ]
                            )
                        ]
                    ),
                    erl_syntax:clause(
                        [erl_syntax:variable('_Length')],
                        none,
                        [
                            erl_syntax:atom(false)
                        ]
                    )
                ]
            )
        ]
    ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    );
is_valid_string(_Prefix, format, _Format) ->
    undefined.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-if(?OTP_RELEASE < 26).
chain_conditions({_, []}, _EvalueMode) ->
    [erl_syntax:atom('true')];
chain_conditions({FunPiecesType, FunPieces}, EvalueMode) ->
    [
        erl_syntax:application(
            erl_syntax:atom(ndto_utils),
            erl_syntax:atom(evalue_conditions),
            [
                erl_syntax:tuple([
                    erl_syntax:atom(FunPiecesType),
                    erl_syntax:list(FunPieces)
                ]),
                erl_syntax:atom(EvalueMode)
            ]
        )
    ].

object_properties_fun_pieces(PropertiesFuns) ->
    [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(PropertyFun),
                    erl_syntax:integer(erl_syntax:function_arity(PropertyFun))
                )
            ),
            erl_syntax:application(
                erl_syntax:atom(maps),
                erl_syntax:atom(get),
                [
                    erl_syntax:binary([
                        erl_syntax:binary_field(
                            erl_syntax:string(erlang:binary_to_list(PropertyName))
                        )
                    ]),
                    erl_syntax:variable('Val'),
                    erl_syntax:atom(undefined)
                ]
            )
        ])
     || {PropertyName, PropertyFun} <- PropertiesFuns
    ].

-else.
chain_conditions({mfa_call, BodyFunCalls}, 'andalso') ->
    [
        erl_syntax:maybe_expr(
            [
                erl_syntax:maybe_match_expr(
                    erl_syntax:atom('true'),
                    BodyFunCall
                )
             || BodyFunCall <- BodyFunCalls
            ] ++ [erl_syntax:atom('true')]
        )
    ];
chain_conditions({mfa_call, BodyFunCalls}, 'xor') ->
    BodyFunCallsCount = length(BodyFunCalls),
    [{FirstFunCall, 1} | IndexedFunCalls] = lists:zip(
        BodyFunCalls, lists:seq(1, BodyFunCallsCount)
    ),
    [
        erl_syntax:maybe_expr(
            [
                erl_syntax:match_expr(
                    erl_syntax:variable('Next1'),
                    FirstFunCall
                )
            ] ++
                [
                    erl_syntax:maybe_match_expr(
                        erl_syntax:tuple([
                            erl_syntax:atom('continue'),
                            erl_syntax:variable(list_to_atom("Next" ++ integer_to_list(N)))
                        ]),
                        xor_maybe_entry(
                            N,
                            erl_syntax:variable(list_to_atom("Next" ++ integer_to_list(N - 1))),
                            BodyFunCall
                        )
                    )
                 || {BodyFunCall, N} <- IndexedFunCalls
                ] ++
                [
                    erl_syntax:infix_expr(
                        erl_syntax:variable(
                            list_to_atom(
                                "Next" ++ integer_to_list(BodyFunCallsCount)
                            )
                        ),
                        erl_syntax:operator('orelse'),
                        erl_syntax:tuple([
                            erl_syntax:atom('false'),
                            erl_syntax:tuple([
                                erl_syntax:atom("."),
                                erl_syntax:atom('none_matched')
                            ])
                        ])
                    )
                ]
        )
    ];
chain_conditions({mfa_call, BodyFunCalls}, 'orelse') ->
    [
        erl_syntax:maybe_expr(
            [
                erl_syntax:maybe_match_expr(
                    erl_syntax:tuple([
                        erl_syntax:atom('false'),
                        erl_syntax:variable('_')
                    ]),
                    FunCall
                )
             || FunCall <- BodyFunCalls
            ] ++
                [
                    erl_syntax:atom(false)
                ]
        )
    ];
chain_conditions({fun_call, BodyFunCalls}, 'andalso') ->
    IndexedBodyFunCalls = lists:zip(lists:seq(1, length(BodyFunCalls)), BodyFunCalls),
    FunCallNames = [
        {list_to_atom("F" ++ integer_to_list(Index)), BodyFunCall}
     || {Index, BodyFunCall} <- IndexedBodyFunCalls
    ],
    FunCallsAsignations = [
        erl_syntax:match_expr(
            erl_syntax:variable(FunCallName),
            BodyFunCall
        )
     || {FunCallName, BodyFunCall} <- FunCallNames
    ],

    FunCallsAsignations ++
        [
            erl_syntax:maybe_expr(
                [
                    erl_syntax:maybe_match_expr(
                        erl_syntax:atom('true'),
                        erl_syntax:application(
                            erl_syntax:variable(FunCallName),
                            []
                        )
                    )
                 || {FunCallName, _} <- FunCallNames
                ] ++ [erl_syntax:atom('true')]
            )
        ].

xor_maybe_entry(N, Opt1, Opt2) ->
    erl_syntax:case_expr(
        erl_syntax:tuple([
            Opt1,
            Opt2
        ]),
        [
            erl_syntax:clause(
                [
                    erl_syntax:tuple([
                        erl_syntax:atom('true'),
                        erl_syntax:atom('true')
                    ])
                ],
                none,
                [
                    erl_syntax:tuple([
                        erl_syntax:atom('false'),
                        erl_syntax:tuple([
                            erl_syntax:atom('.'),
                            erl_syntax:atom('many_matched')
                        ])
                    ])
                ]
            ),
            erl_syntax:clause(
                [
                    erl_syntax:tuple([
                        erl_syntax:variable(list_to_atom("False1_" ++ integer_to_list(N))),
                        erl_syntax:variable(list_to_atom("False2_" ++ integer_to_list(N)))
                    ])
                ],
                [
                    erl_syntax:infix_expr(
                        erl_syntax:variable(list_to_atom("False1_" ++ integer_to_list(N))),
                        erl_syntax:operator('=/='),
                        erl_syntax:atom('true')
                    ),
                    erl_syntax:infix_expr(
                        erl_syntax:variable(list_to_atom("False2_" ++ integer_to_list(N))),
                        erl_syntax:operator('=/='),
                        erl_syntax:atom('true')
                    )
                ],
                [
                    erl_syntax:tuple([
                        erl_syntax:atom('continue'),
                        erl_syntax:atom('false')
                    ])
                ]
            ),
            erl_syntax:clause(
                [
                    erl_syntax:tuple([
                        erl_syntax:variable(list_to_atom("_B1_" ++ integer_to_list(N))),
                        erl_syntax:variable(list_to_atom("_B2_" ++ integer_to_list(N)))
                    ])
                ],
                none,
                [
                    erl_syntax:tuple([
                        erl_syntax:atom('continue'),
                        erl_syntax:atom('true')
                    ])
                ]
            )
        ]
    ).

object_properties_fun_pieces(PropertiesFuns) ->
    [
        erl_syntax:application(
            erl_syntax:function_name(PropertyFun),
            [
                erl_syntax:application(
                    erl_syntax:atom(maps),
                    erl_syntax:atom(get),
                    [
                        erl_syntax:binary([
                            erl_syntax:binary_field(
                                erl_syntax:string(erlang:binary_to_list(PropertyName))
                            )
                        ]),
                        erl_syntax:variable('Val'),
                        erl_syntax:atom(undefined)
                    ]
                )
            ]
        )
     || {PropertyName, PropertyFun} <- PropertiesFuns
    ].

-endif.

clauses(Clauses) ->
    lists:filter(fun(Clause) -> Clause =/= undefined end, Clauses).

false_clause(FunName, ExpectedDescription) ->
    erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [false_return(FunName, ExpectedDescription)]
    ).

false_return(FunName, ExpectedDescription) ->
    ErrorMessage = erl_syntax:application(
        erl_syntax:atom(erlang),
        erl_syntax:atom(list_to_binary),
        [
            erl_syntax:application(
                erl_syntax:atom('io_lib'),
                erl_syntax:atom('format'),
                [
                    erl_syntax:string("\~p is not a " ++ ExpectedDescription),
                    erl_syntax:list([
                        erl_syntax:variable('Val')
                    ])
                ]
            )
        ]
    ),
    erl_syntax:tuple([
        erl_syntax:atom('false'),
        erl_syntax:tuple([
            erl_syntax:atom(binary_to_list(FunName)),
            ErrorMessage
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
        [erl_syntax:variable('null')],
        none,
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
