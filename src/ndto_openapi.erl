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

%%% INCLUDE FILES
-include("ndto.hrl").

%%% EXTERNAL EXPORTS
-export([
    is_valid/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
is_valid(Prefix, #{<<"$ref">> := Ref} = Schema) ->
    [_Path, <<DTO/binary>>] = string:split(Ref, <<"/">>, trailing),
    FunName = <<Prefix/binary, "ref_", DTO/binary>>,
    OptionalClause = ndto_generator:optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(ref),
            [
                erl_syntax:application(
                    erl_syntax:atom(erlang:binary_to_atom(DTO)),
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
is_valid(Prefix, #{<<"type">> := <<"string">>} = Schema) ->
    FunName = <<Prefix/binary, "string">>,
    ExtraFuns =
        lists:foldl(
            fun(Keyword, Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    Value ->
                        [is_valid_string(<<FunName/binary, "_">>, Keyword, Value) | Acc]
                end
            end,
            [],
            [
                <<"enum">>,
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
    OptionalClause = ndto_generator:optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(string),
            [ndto_generator:chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = ndto_generator:false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{<<"type">> := Type} = Schema) when Type == <<"number">>; Type == <<"integer">> ->
    FunName = <<Prefix/binary, Type/binary>>,
    ExtraFuns = lists:foldl(
        fun(Keyword, Acc) ->
            case maps:get(Keyword, Schema, undefined) of
                undefined ->
                    Acc;
                Value ->
                    [is_valid_number(Type, <<FunName/binary, "_">>, Keyword, Value, Schema) | Acc]
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
    OptionalClause = ndto_generator:optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(erlang:binary_to_atom(Type)),
            [ndto_generator:chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = ndto_generator:false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {Fun, ExtraFuns};
is_valid(Prefix, #{<<"type">> := <<"boolean">>} = Schema) ->
    FunName = <<Prefix/binary, "boolean">>,
    OptionalClause = ndto_generator:optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(boolean),
            [erl_syntax:atom(true)]
        ),
    FalseClause = ndto_generator:false_clause(),
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
    OptionalClause = ndto_generator:optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(array),
            [ndto_generator:chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = ndto_generator:false_clause(),
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
    OptionalClause = ndto_generator:optional_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(object),
            [ndto_generator:chain_conditions(BodyFunCalls, 'andalso')]
        ),
    FalseClause = ndto_generator:false_clause(),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        OptionalClause ++ [TrueClause, FalseClause]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{<<"oneOf">> := Subschemas} = _Schema) ->
    FunName = <<Prefix/binary, "oneOf">>,
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
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [ndto_generator:chain_conditions(BodyFunCalls, 'xor')]
        ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{<<"anyOf">> := Subschemas} = _Schema) ->
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
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [ndto_generator:chain_conditions(BodyFunCalls, 'orelse')]
        ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{<<"allOf">> := Subschemas} = _Schema) ->
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
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [ndto_generator:chain_conditions(BodyFunCalls, 'andalso')]
        ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    {Fun, IsValidFuns ++ ExtraFuns};
is_valid(Prefix, #{<<"not">> := Subschema} = _Schema) ->
    FunName = <<Prefix/binary, "not">>,
    {IsValidFun, ExtraFuns} = is_valid(<<FunName/binary, "_">>, Subschema),
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
        [TrueClause]
    ),
    {Fun, [IsValidFun | ExtraFuns]};
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
    {Fun, []}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
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
    OptionalClause = ndto_generator:optional_clause(Items),
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
        OptionalClause ++ [TrueClause]
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
    FalseClause = ndto_generator:false_clause(),
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
    FalseClause = ndto_generator:false_clause(),
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
    Schema :: schema(),
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
    FalseClause = ndto_generator:false_clause(),
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
    FalseClause = ndto_generator:false_clause(),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    );
is_valid_number(Type, Prefix, <<"multipleOf">>, MultipleOf, _Schema) ->
    FunName = <<Prefix/binary, "multipleOf">>,
    MultipleOfST =
        case MultipleOf of
            Integer when is_integer(Integer) ->
                erl_syntax:integer(MultipleOf);
            _Float ->
                erl_syntax:float(MultipleOf)
        end,
    TrueClause =
        case Type of
            <<"integer">> ->
                erl_syntax:clause(
                    [erl_syntax:variable('Val')],
                    none,
                    [
                        erl_syntax:infix_expr(
                            erl_syntax:infix_expr(
                                erl_syntax:variable('Val'),
                                erl_syntax:operator('rem'),
                                MultipleOfST
                            ),
                            erl_syntax:operator('=:='),
                            erl_syntax:integer(0)
                        )
                    ]
                );
            <<"number">> ->
                erl_syntax:clause(
                    [erl_syntax:variable('Val')],
                    erl_syntax:infix_expr(
                        erl_syntax:variable('Val'),
                        erl_syntax:operator('>='),
                        MultipleOfST
                    ),
                    [
                        erl_syntax:match_expr(
                            erl_syntax:variable('Quotient'),
                            erl_syntax:infix_expr(
                                erl_syntax:variable('Val'),
                                erl_syntax:operator('/'),
                                MultipleOfST
                            )
                        ),
                        erl_syntax:infix_expr(
                            erl_syntax:variable('Quotient'),
                            erl_syntax:operator('=='),
                            erl_syntax:application(
                                erl_syntax:atom(erlang),
                                erl_syntax:atom(trunc),
                                [erl_syntax:variable('Quotient')]
                            )
                        )
                    ]
                )
        end,
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ).

-spec is_valid_object(Prefix, Keyword, Schema) -> Result when
    Prefix :: binary(),
    Keyword :: binary(),
    Schema :: schema(),
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
    FunBody = [ndto_generator:chain_conditions(FunCalls, 'andalso')],
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
is_valid_object(Prefix, <<"additionalProperties">>, #{
    <<"additionalProperties">> := false, <<"properties">> := Properties
}) ->
    FunName = <<Prefix/binary, "additionalProperties">>,
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
    merl:print(Fun),
    {Fun, []};
is_valid_object(_Prefix, <<"additionalProperties">>, #{<<"additionalProperties">> := true}) ->
    {undefined, []};
is_valid_object(Prefix, <<"additionalProperties">>, #{
    <<"additionalProperties">> := AdditionalProperties, <<"properties">> := Properties
}) ->
    FunName = <<Prefix/binary, "additionalProperties">>,
    {IsValidFun, ExtraFuns} = is_valid(<<FunName/binary, "_">>, AdditionalProperties),
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
    {Fun, [IsValidFun | ExtraFuns]}.

-spec is_valid_string(Prefix, Keyword, Value) -> Result when
    Prefix :: binary(),
    Keyword :: binary(),
    Value :: term(),
    Result :: erl_syntax:syntaxTree().
is_valid_string(Prefix, <<"enum">>, Enum) ->
    FunName = <<Prefix/binary, "enum">>,
    TrueClauses = lists:map(
        fun(EnumVal) ->
            erl_syntax:clause(
                [
                    erl_syntax:binary([
                        erl_syntax:binary_field(erl_syntax:string(erlang:binary_to_list(EnumVal)))
                    ])
                ],
                none,
                [erl_syntax:atom(true)]
            )
        end,
        Enum
    ),
    FalseClause = ndto_generator:false_clause(),
    Clauses = lists:append(TrueClauses, [FalseClause]),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    );
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
    ).
