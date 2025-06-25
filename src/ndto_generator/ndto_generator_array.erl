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
-module(ndto_generator_array).

%%% BEHAVIOR
-behaviour(ndto_generator).

%%% EXTERNAL EXPORTS
-export([is_valid/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
is_valid(Prefix, #{type := array} = Schema) ->
    FunName = Prefix,
    {IsValidFuns, ExtraFuns} =
        lists:foldl(
            fun(Keyword, {IsValidFunsAcc, ExtraFunsAcc} = Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    _Value ->
                        case is_valid_(FunName, Keyword, Schema) of
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
    ValidationConditions = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(Fun),
                    erl_syntax:integer(erl_syntax:function_arity(Fun))
                )
            ),
            erl_syntax:list([erl_syntax:variable('Val')])
        ])
     || Fun <- IsValidFuns
    ],
    OptionalClause = ndto_generator:optional_clause(Schema),
    NullClause = ndto_generator:null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(array),
            ndto_generator:chain_conditions(FunName, ValidationConditions, 'andalso')
        ),
    FalseClause = ndto_generator:false_clause(<<FunName/binary, ".type">>, "Value is not an array"),
    Clauses = ndto_generator:clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns}.

-spec is_valid_(Prefix, Keyword, Value) -> Result when
    Prefix :: binary(),
    Keyword :: atom(),
    Value :: term(),
    Result :: {Fun, ExtraFuns},
    Fun :: erl_syntax:syntaxTree() | undefined,
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid_(Prefix, items, #{items := Items} = Schema) when is_map(Items) ->
    FunName = <<Prefix/binary, ".items">>,
    {IsValidFun, ExtraFuns} = ndto_generator:is_valid(<<FunName/binary, "[*]">>, Items),
    OptionalClause = ndto_generator:optional_clause(Schema),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:atom(ndto_validation),
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
                                erl_syntax:tuple([
                                    erl_syntax:variable('Function'),
                                    erl_syntax:variable('Reason')
                                ])
                            ])
                        ],
                        none,
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:tuple([
                                    erl_syntax:variable('Function'),
                                    erl_syntax:application(
                                        erl_syntax:atom(erlang),
                                        erl_syntax:atom(list_to_binary),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom('io_lib'),
                                                erl_syntax:atom('format'),
                                                [
                                                    erl_syntax:string(
                                                        "Item ~p in ~ts is invalid. ~s"
                                                    ),
                                                    erl_syntax:list([
                                                        erl_syntax:variable('Acc'),
                                                        erl_syntax:string(
                                                            binary_to_list(Prefix)
                                                        ),
                                                        erl_syntax:variable('Reason')
                                                    ])
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
        ndto_generator:clauses([OptionalClause, TrueClause])
    ),
    {Fun, [IsValidFun | ExtraFuns]};
is_valid_(Prefix, items, #{items := Items} = Schema) when is_list(Items) ->
    FunName = <<Prefix/binary, ".items">>,
    {_Size, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Item, {Idx, IsValidFunsAcc, ExtraFunsAcc}) ->
            ItemFunName = <<FunName/binary, "[", (erlang:integer_to_binary(Idx))/binary, "]">>,
            {ItemIsValidFun, ItemExtraFuns} = ndto_generator:is_valid(ItemFunName, Item),
            {Idx + 1, [{Idx, ItemIsValidFun} | IsValidFunsAcc], ItemExtraFuns ++ ExtraFunsAcc}
        end,
        {1, [], []},
        Items
    ),
    AdditionalItems = maps:get(additional_items, Schema, true),
    {IsValidAdditionalItemsFun, AdditionalItemsExtraFuns} =
        ndto_generator:is_valid(<<FunName/binary, ".additional_items">>, AdditionalItems),
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
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:atom(ndto_validation),
                    erl_syntax:atom(find),
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
                                    erl_syntax:match_expr(
                                        erl_syntax:variable('Result'),
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
                                                        erl_syntax:case_expr(
                                                            erl_syntax:application(
                                                                erl_syntax:function_name(
                                                                    IsValidAdditionalItemsFun
                                                                ),
                                                                [erl_syntax:variable('Item')]
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
                                                                            erl_syntax:atom(
                                                                                'false'
                                                                            ),
                                                                            erl_syntax:variable(
                                                                                '_FalseReason'
                                                                            )
                                                                        ])
                                                                    ],
                                                                    none,
                                                                    [
                                                                        erl_syntax:tuple([
                                                                            erl_syntax:atom(
                                                                                'false'
                                                                            ),
                                                                            erl_syntax:tuple([
                                                                                erl_syntax:atom(
                                                                                    erlang:binary_to_atom(
                                                                                        FunName
                                                                                    )
                                                                                ),
                                                                                erl_syntax:application(
                                                                                    erl_syntax:atom(
                                                                                        'list_to_binary'
                                                                                    ),
                                                                                    [
                                                                                        erl_syntax:application(
                                                                                            erl_syntax:atom(
                                                                                                io_lib
                                                                                            ),
                                                                                            erl_syntax:atom(
                                                                                                format
                                                                                            ),
                                                                                            [
                                                                                                erl_syntax:string(
                                                                                                    "Value at position ~p does not match the schema"
                                                                                                ),
                                                                                                erl_syntax:list(
                                                                                                    [
                                                                                                        erl_syntax:variable(
                                                                                                            'FunKey'
                                                                                                        )
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
                                    ),
                                    erl_syntax:case_expr(
                                        erl_syntax:variable('Result'),
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
                                                        erl_syntax:variable('FalseResult')
                                                    ])
                                                ],
                                                none,
                                                [
                                                    erl_syntax:tuple([
                                                        erl_syntax:atom('true'),
                                                        erl_syntax:variable('FalseResult')
                                                    ])
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
                                erl_syntax:tuple([
                                    erl_syntax:variable('_Item'),
                                    erl_syntax:variable('_FunKey')
                                ]),
                                erl_syntax:tuple([
                                    erl_syntax:variable('Prefix'),
                                    erl_syntax:variable('Reason')
                                ])
                            ])
                        ],
                        none,
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:tuple([
                                    erl_syntax:variable('Prefix'),
                                    erl_syntax:variable('Reason')
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
        [TrueClause]
    ),
    {Fun, ExtraFuns ++ [IsValidAdditionalItemsFun | AdditionalItemsExtraFuns]};
is_valid_(Prefix, min_items, #{min_items := MinItems}) ->
    FunName = <<Prefix/binary, ".min_items">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        erl_syntax:infix_expr(
            erl_syntax:application(erl_syntax:atom(length), [erl_syntax:variable('Val')]),
            erl_syntax:operator('>='),
            erl_syntax:integer(MinItems)
        ),
        [erl_syntax:atom(true)]
    ),
    FalseClause = ndto_generator:false_clause(
        FunName,
        unicode:characters_to_list(
            io_lib:format("Array does not have at least ~p items", [MinItems])
        )
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    ),
    {Fun, []};
is_valid_(Prefix, max_items, #{max_items := MaxItems}) ->
    FunName = <<Prefix/binary, ".max_items">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        erl_syntax:infix_expr(
            erl_syntax:application(erl_syntax:atom(length), [erl_syntax:variable('Val')]),
            erl_syntax:operator('=<'),
            erl_syntax:integer(MaxItems)
        ),
        [erl_syntax:atom(true)]
    ),
    FalseClause = ndto_generator:false_clause(
        FunName,
        unicode:characters_to_list(
            io_lib:format("Array does not have at most ~p items", [MaxItems])
        )
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    ),
    {Fun, []};
is_valid_(Prefix, unique_items, #{unique_items := true}) ->
    FunName = <<Prefix/binary, ".unique_items">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:match_expr(
                erl_syntax:variable('ArraySize'),
                erl_syntax:application(erl_syntax:atom(length), [erl_syntax:variable('Val')])
            ),
            erl_syntax:case_expr(
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
                ),
                [
                    erl_syntax:clause(
                        [erl_syntax:variable('ArraySize')],
                        none,
                        [erl_syntax:atom('true')]
                    ),
                    erl_syntax:clause(
                        [erl_syntax:variable('_')],
                        none,
                        [
                            ndto_generator:false_return(
                                FunName,
                                unicode:characters_to_list(
                                    io_lib:format("Array has non unique items", [])
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
    {Fun, []};
is_valid_(_Prefix, unique_items, #{unique_items := false}) ->
    {undefined, []}.
