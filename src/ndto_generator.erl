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

    {IsValidFun, ExtraFuns} = is_valid(<<"is_valid_">>, Schema),
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
    FunName = <<Prefix/binary, "ref_", Ref/binary>>,
    DTO = erlang:binary_to_atom(Ref),
    OptionalClause = optional_clause(Schema),
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
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause]
    ),
    {Fun, []};
is_valid(Prefix, #{enum := Enum} = Schema) ->
    FunName = <<Prefix/binary, "enum">>,
    OptionalClause = optional_clause(Schema),
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
    FalseClause = false_clause(),
    Clauses = OptionalClause ++ TrueClauses ++ [FalseClause],
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, []};
is_valid(Prefix, #{type := string} = Schema) ->
    FunName = <<Prefix/binary, "string">>,
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
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- ExtraFuns
    ],
    OptionalClause = optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(string),
            [chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{type := integer} = Schema) ->
    FunName = <<Prefix/binary, "integer">>,
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
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- ExtraFuns
    ],
    OptionalClause = optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(integer),
            [chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
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
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- ExtraFuns
    ],
    OptionalClause = optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(float),
            [chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{type := boolean} = Schema) ->
    FunName = <<Prefix/binary, "boolean">>,
    OptionalClause = optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(boolean),
            [erl_syntax:atom(true)]
        ),
    FalseClause = false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {Fun, []};
is_valid(Prefix, #{type := array} = Schema) ->
    FunName = <<Prefix/binary, "array">>,
    {IsValidFuns, ExtraFuns} =
        lists:foldl(
            fun(Keyword, {IsValidFunsAcc, ExtraFunsAcc} = Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    Value ->
                        case is_valid_array(<<FunName/binary, "_">>, Keyword, Value) of
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
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(array),
            [chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{type := object} = Schema) ->
    FunName = <<Prefix/binary, "object">>,
    {IsValidFuns, ExtraFuns} =
        lists:foldl(
            fun(Keyword, {IsValidFunsAcc, ExtraFunsAcc} = Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    _Value ->
                        case is_valid_object(<<FunName/binary, "_">>, Keyword, Schema) of
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
    BodyFunCalls = [
        erl_syntax:application(
            erl_syntax:function_name(Fun),
            [erl_syntax:variable('Val')]
        )
     || Fun <- IsValidFuns
    ],
    OptionalClause = optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            type_guard(object),
            [chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{one_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "one_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {Idx, IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "_", (erlang:integer_to_binary(Idx))/binary, "_">>, Subschema
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
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [chain_conditions(BodyFunCalls, 'xor')]
        ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{any_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "any_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {RawIdx, IsValidFunsAcc, ExtraFunsAcc}) ->
            Idx = erlang:integer_to_binary(RawIdx),
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "_", Idx/binary, "_">>, Subschema
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
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [chain_conditions(BodyFunCalls, 'orelse')]
        ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{all_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "all_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {RawIdx, IsValidFunsAcc, ExtraFunsAcc}) ->
            Idx = erlang:integer_to_binary(RawIdx),
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "_", Idx/binary, "_">>, Subschema
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
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [chain_conditions(BodyFunCalls, 'andalso')]
        ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{'not' := Subschema} = Schema) ->
    FunName = <<Prefix/binary, "not">>,
    {IsValidFun, ExtraFuns} = is_valid(<<FunName/binary, "_">>, Subschema),
    OptionalClause = optional_clause(Schema),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:prefix_expr(
                erl_syntax:operator('not'),
                erl_syntax:application(
                    erl_syntax:function_name(IsValidFun),
                    [erl_syntax:variable('Val')]
                )
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause]
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

-spec is_valid_array(Prefix, Keyword, Value) -> Result when
    Prefix :: binary(),
    Keyword :: atom(),
    Value :: term(),
    Result :: {Fun, ExtraFuns},
    Fun :: erl_syntax:syntaxTree() | undefined,
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid_array(Prefix, items, Items) ->
    FunName = <<Prefix/binary, "items">>,
    {IsValidFun, ExtraFuns} = is_valid(<<FunName/binary, "_">>, Items),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:application(
                erl_syntax:atom(lists),
                erl_syntax:atom(all),
                [
                    erl_syntax:fun_expr([
                        erl_syntax:clause(
                            [erl_syntax:variable('Item')],
                            none,
                            [
                                erl_syntax:application(
                                    erl_syntax:function_name(IsValidFun),
                                    [erl_syntax:variable('Item')]
                                )
                            ]
                        )
                    ]),
                    erl_syntax:variable('Val')
                ]
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, [IsValidFun | ExtraFuns]};
is_valid_array(Prefix, min_items, MinItems) ->
    FunName = <<Prefix/binary, "min_items">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        erl_syntax:infix_expr(
            erl_syntax:application(erl_syntax:atom(length), [erl_syntax:variable('Val')]),
            erl_syntax:operator('>='),
            erl_syntax:integer(MinItems)
        ),
        [erl_syntax:atom(true)]
    ),
    FalseClause = false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    ),
    {Fun, []};
is_valid_array(Prefix, max_items, MaxItems) ->
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
    FalseClause = false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    ),
    {Fun, []};
is_valid_array(Prefix, unique_items, true) ->
    FunName = <<Prefix/binary, "unique_items">>,
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
        [TrueClause]
    ),
    {Fun, []};
is_valid_array(_Prefix, unique_items, false) ->
    {undefined, []}.

-spec is_valid_number(Type, Prefix, Keyword, Value, Schema) -> Result when
    Type :: integer | float,
    Prefix :: binary(),
    Keyword :: atom(),
    Value :: term(),
    Schema :: ndto:integer_schema() | ndto:float_schema(),
    Result :: undefined | erl_syntax:syntaxTree().
is_valid_number(_Type, Prefix, minimum, Minimum, Schema) ->
    FunName = <<Prefix/binary, "minimum">>,
    MinimumSt =
        case Minimum of
            Integer when is_integer(Integer) ->
                erl_syntax:integer(Minimum);
            _Float ->
                erl_syntax:float(Minimum)
        end,
    Operator =
        case maps:get(exclusive_minimum, Schema, false) of
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
    FalseClause = false_clause(),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    );
is_valid_number(_Type, Prefix, maximum, Maximum, Schema) ->
    FunName = <<Prefix/binary, "maximum">>,
    MaximumSt =
        case Maximum of
            Integer when is_integer(Integer) ->
                erl_syntax:integer(Maximum);
            _Float ->
                erl_syntax:float(Maximum)
        end,
    Operator =
        case maps:get(exclusive_maximum, Schema, false) of
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
    FalseClause = false_clause(),
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
    FunName = <<Prefix/binary, "properties">>,
    {PropertiesFuns, ExtraFuns} = maps:fold(
        fun(PropertyName, Property, {IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidPropertyFun, ExtraPropertyFuns} =
                is_valid(<<FunName/binary, "_", PropertyName/binary, "_">>, Property),
            {
                [{PropertyName, IsValidPropertyFun} | IsValidFunsAcc],
                ExtraFunsAcc ++ ExtraPropertyFuns
            }
        end,
        {[], []},
        Properties
    ),
    FunCalls = [
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
    ],
    FunBody = [chain_conditions(FunCalls, 'andalso')],
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
    FunName = <<Prefix/binary, "required">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:application(
                erl_syntax:atom(lists),
                erl_syntax:atom(all),
                [
                    erl_syntax:fun_expr([
                        erl_syntax:clause(
                            [erl_syntax:variable('Property')],
                            none,
                            [
                                erl_syntax:application(
                                    erl_syntax:atom(maps),
                                    erl_syntax:atom(is_key),
                                    [
                                        erl_syntax:variable('Property'),
                                        erl_syntax:variable('Val')
                                    ]
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
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, []};
is_valid_object(Prefix, min_properties, #{min_properties := MinProperties}) ->
    FunName = <<Prefix/binary, "min_properties">>,
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
    FunName = <<Prefix/binary, "max_properties">>,
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
    FunName = <<Prefix/binary, "pattern_properties">>,
    {IsValidPatterns, ExtraFuns} = lists:foldl(
        fun({PropertyPattern, PropertySchema}, {IsValidPatternsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "_", PropertyPattern/binary, "_">>, PropertySchema
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
                                erl_syntax:application(
                                    erl_syntax:function_name(IsValidFun),
                                    [erl_syntax:variable('PropertyValue')]
                                )
                            ]
                        )
                    ]),
                    Filter
                ]
            )
        end,
        IsValidPatterns
    ),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [chain_conditions(Conditions, 'andalso')]
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
    FunName = <<Prefix/binary, "additional_properties">>,
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
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, []};
is_valid_object(
    Prefix,
    additional_properties,
    #{additional_properties := AdditionalProperties} = Schema
) ->
    FunName = <<Prefix/binary, "additional_properties">>,
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
            erl_syntax:application(
                erl_syntax:atom(lists),
                erl_syntax:atom(all),
                [
                    erl_syntax:fun_expr([
                        erl_syntax:clause(
                            [erl_syntax:variable('Property')],
                            none,
                            [
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
            )
        ]
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
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
    FunName = <<Prefix/binary, "min_length">>,
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
    FunName = <<Prefix/binary, "max_length">>,
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
    FunName = <<Prefix/binary, "pattern">>,
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
    FunName = <<Prefix/binary, "format">>,
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
    FunName = <<Prefix/binary, "format">>,
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
chain_conditions(FunCalls, 'andalso' = Operator) ->
    chain_conditions(FunCalls, Operator, erl_syntax:atom(true));
chain_conditions(FunCalls, 'orelse' = Operator) ->
    chain_conditions(FunCalls, Operator, erl_syntax:atom(false));
chain_conditions(FunCalls, 'xor' = Operator) ->
    chain_conditions(FunCalls, Operator, erl_syntax:atom(false)).

chain_conditions([], _Operator, Acc) ->
    Acc;
chain_conditions([FunCall | Rest], Operator, Acc) ->
    NewAcc = erl_syntax:infix_expr(
        Acc,
        erl_syntax:operator(Operator),
        FunCall
    ),
    chain_conditions(Rest, Operator, NewAcc).

false_clause() ->
    erl_syntax:clause(
        [erl_syntax:variable('_Val')],
        none,
        [erl_syntax:atom(false)]
    ).

guard(Pred, Var) ->
    erl_syntax:application(erl_syntax:atom(Pred), [erl_syntax:variable(Var)]).

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

optional_clause(#{nullable := true}) ->
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
