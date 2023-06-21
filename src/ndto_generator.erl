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
is_valid(Prefix, undefined) ->
    FunName = <<Prefix/binary, "undefined">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:atom(undefined)],
        none,
        [erl_syntax:atom(true)]
    ),
    FalseClause = false_clause(),
    Clauses = [TrueClause, FalseClause],
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, []};
is_valid(Prefix, #{<<"$ref">> := Ref} = Schema) ->
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
is_valid(Prefix, #{<<"enum">> := Enum}) ->
    FunName = <<Prefix/binary, "enum">>,
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
    Clauses = lists:append(TrueClauses, [FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, []};
is_valid(Prefix, #{<<"type">> := <<"string">>} = Schema) ->
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
                <<"minLength">>,
                <<"maxLength">>,
                <<"format">>,
                <<"pattern">>
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
is_valid(Prefix, #{<<"type">> := Type} = Schema) when
    is_binary(Type) andalso (Type =:= <<"number">> orelse Type =:= <<"integer">>)
->
    FunName = <<Prefix/binary, Type/binary>>,
    ExtraFuns = lists:foldl(
        fun(Keyword, Acc) ->
            case maps:get(Keyword, Schema, undefined) of
                undefined ->
                    Acc;
                Value ->
                    case is_valid_number(Type, <<FunName/binary, "_">>, Keyword, Value, Schema) of
                        undefined ->
                            Acc;
                        NewIsValidFun ->
                            [NewIsValidFun | Acc]
                    end
            end
        end,
        [],
        [
            <<"minimum">>,
            <<"maximum">>,
            <<"multipleOf">>
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
            type_guard(erlang:binary_to_atom(Type)),
            [chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{<<"type">> := <<"boolean">>} = Schema) ->
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
is_valid(Prefix, #{<<"type">> := <<"array">>} = Schema) ->
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
                                    % TODO: replace with `++` for consistency
                                    % Currently `gradualizer` complains:
                                    % - The operator '++' on line 165 at column 81 is expected to have type
                                    % _TyVar-576460752303422687 which is too precise to be statically checked
                                    lists:append(NewExtraFuns, ExtraFunsAcc)
                                }
                        end
                end
            end,
            {[], []},
            [
                <<"items">>,
                <<"minItems">>,
                <<"maxItems">>,
                <<"uniqueItems">>
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
is_valid(Prefix, #{<<"type">> := <<"object">>} = Schema) ->
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
                                    % TODO: replace with `++` for consistency
                                    % Currently `gradualizer` complains:
                                    % - The operator '++' on line 165 at column 81 is expected to have type
                                    % _TyVar-576460752303422687 which is too precise to be statically checked
                                    lists:append(NewExtraFuns, ExtraFunsAcc)
                                }
                        end
                end
            end,
            {[], []},
            [
                <<"properties">>,
                <<"required">>,
                <<"minProperties">>,
                <<"maxProperties">>,
                <<"patternProperties">>,
                <<"additionalProperties">>
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
is_valid(Prefix, #{<<"oneOf">> := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "oneOf">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {Idx, IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "_", (erlang:integer_to_binary(Idx))/binary, "_">>, Subschema
            ),
            {
                Idx + 1,
                [IsValidFun | IsValidFunsAcc],
                % TODO: replace with `++` for consistency
                % Currently `gradualizer` complains:
                % The operator '++' on line 274 at column 27 is expected to have type
                % _TyVar-576460752303421471 which is too precise to be statically checked
                lists:append(ExtraFuns, ExtraFunsAcc)
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
is_valid(Prefix, #{<<"anyOf">> := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "anyOf">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {Idx, IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "_", Idx/integer, "_">>, Subschema
            ),
            {
                Idx + 1,
                [IsValidFun | IsValidFunsAcc],
                % TODO: replace with `++` for consistency
                % Currently `gradualizer` complains:
                % The operator '++' on line 312 at column 27 is expected to have type
                % _TyVar-576460752303420927 which is too precise to be statically checked
                lists:append(ExtraFuns, ExtraFunsAcc)
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
is_valid(Prefix, #{<<"allOf">> := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, "allOf">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {Idx, IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = is_valid(
                <<FunName/binary, "_", Idx/integer, "_">>, Subschema
            ),
            {
                Idx + 1,
                [IsValidFun | IsValidFunsAcc],
                % TODO: replace with `++` for consistency
                % Currently `gradualizer` complains:
                % The operator '++' on line 350 at column 27 is expected to have type
                % _TyVar-576460752303420383 which is too precise to be statically checked
                lists:append(ExtraFuns, ExtraFunsAcc)
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
is_valid(Prefix, #{<<"not">> := Subschema} = Schema) ->
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
    Keyword :: binary(),
    Value :: term(),
    Result :: {Fun, ExtraFuns},
    Fun :: erl_syntax:syntaxTree() | undefined,
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid_array(Prefix, <<"items">>, Items) ->
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
is_valid_array(Prefix, <<"minItems">>, MinItems) ->
    FunName = <<Prefix/binary, "minItems">>,
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
is_valid_array(Prefix, <<"maxItems">>, MaxItems) ->
    FunName = <<Prefix/binary, "maxItems">>,
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
is_valid_array(Prefix, <<"uniqueItems">>, true) ->
    FunName = <<Prefix/binary, "uniqueItems">>,
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
is_valid_array(_Prefix, <<"uniqueItems">>, false) ->
    {undefined, []}.

-spec is_valid_number(Type, Prefix, Keyword, Value, Schema) -> Result when
    Type :: binary(),
    Prefix :: binary(),
    Keyword :: binary(),
    Value :: term(),
    Schema :: ndto:number_schema(),
    Result :: erl_syntax:syntaxTree().
is_valid_number(_Type, Prefix, <<"minimum">>, Minimum, Schema) ->
    FunName = <<Prefix/binary, "minimum">>,
    MinimumSt =
        case Minimum of
            Integer when is_integer(Integer) ->
                erl_syntax:integer(Minimum);
            _Float ->
                erl_syntax:float(Minimum)
        end,
    Operator =
        case maps:get(<<"exclusiveMinimum">>, Schema, false) of
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
is_valid_number(_Type, Prefix, <<"maximum">>, Maximum, Schema) ->
    FunName = <<Prefix/binary, "maximum">>,
    MaximumSt =
        case Maximum of
            Integer when is_integer(Integer) ->
                erl_syntax:integer(Maximum);
            _Float ->
                erl_syntax:float(Maximum)
        end,
    Operator =
        case maps:get(<<"exclusiveMaximum">>, Schema, false) of
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
is_valid_number(<<"integer">>, Prefix, <<"multipleOf">>, MultipleOf, _Schema) ->
    FunName = <<Prefix/binary, "multipleOf">>,
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
is_valid_number(_Number, _Prefix, <<"multipleOf">>, _Value, _Schema) ->
    undefined.

-spec is_valid_object(Prefix, Keyword, Schema) -> Result when
    Prefix :: binary(),
    Keyword :: binary(),
    Schema :: ndto:object_schema(),
    Result :: {Fun, ExtraFuns},
    Fun :: erl_syntax:syntaxTree() | undefined,
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid_object(Prefix, <<"properties">>, #{<<"properties">> := Properties}) ->
    FunName = <<Prefix/binary, "properties">>,
    {PropertiesFuns, ExtraFuns} = maps:fold(
        fun(PropertyName, Property, {IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidPropertyFun, ExtraPropertyFuns} =
                is_valid(<<FunName/binary, "_", PropertyName/binary, "_">>, Property),
            {
                [{PropertyName, IsValidPropertyFun} | IsValidFunsAcc],
                % TODO: replace with `++` for consistency
                % Currently `gradualizer` complains:
                % - The operator '++' on line 567 at column 30 is expected to have type
                % _TyVar-576460752303420927 which is too precise to be statically checked
                lists:append(ExtraFunsAcc, ExtraPropertyFuns)
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
is_valid_object(Prefix, <<"required">>, #{<<"required">> := Required}) ->
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
is_valid_object(Prefix, <<"minProperties">>, #{<<"minProperties">> := MinProperties}) ->
    FunName = <<Prefix/binary, "minProperties">>,
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
is_valid_object(Prefix, <<"maxProperties">>, #{<<"maxProperties">> := MaxProperties}) ->
    FunName = <<Prefix/binary, "maxProperties">>,
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
is_valid_object(Prefix, <<"patternProperties">>, #{<<"patternProperties">> := PatternProperties}) ->
    FunName = <<Prefix/binary, "patternProperties">>,
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
    <<"additionalProperties">>,
    #{<<"additionalProperties">> := false, <<"properties">> := Properties} = Schema
) ->
    FunName = <<Prefix/binary, "additionalProperties">>,
    PatternProperties = maps:get(<<"patternProperties">>, Schema, #{}),
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
    <<"additionalProperties">>,
    #{<<"additionalProperties">> := AdditionalProperties, <<"properties">> := Properties} = Schema
) ->
    FunName = <<Prefix/binary, "additionalProperties">>,
    {IsValidFun, ExtraFuns} = is_valid(<<FunName/binary, "_">>, AdditionalProperties),
    PatternProperties = maps:get(<<"patternProperties">>, Schema, #{}),
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
is_valid_object(_Prefix, <<"additionalProperties">>, _Schema) ->
    {undefined, []}.

-spec is_valid_string(Prefix, Keyword, Value) -> Result when
    Prefix :: binary(),
    Keyword :: binary(),
    Value :: term(),
    Result :: undefined | erl_syntax:syntaxTree().
is_valid_string(Prefix, <<"minLength">>, MinLength) ->
    FunName = <<Prefix/binary, "minLength">>,
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
is_valid_string(Prefix, <<"maxLength">>, MaxLength) ->
    FunName = <<Prefix/binary, "maxLength">>,
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
is_valid_string(Prefix, <<"pattern">>, Pattern) ->
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
is_valid_string(Prefix, <<"format">>, <<"iso8601-datetime">>) ->
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
is_valid_string(Prefix, <<"format">>, <<"base64">>) ->
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
is_valid_string(_Prefix, <<"format">>, _Format) ->
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
