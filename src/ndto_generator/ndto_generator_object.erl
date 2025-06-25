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
-module(ndto_generator_object).

%%% BEHAVIOR
-behaviour(ndto_generator).

%%% EXTERNAL EXPORTS
-export([is_valid/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
is_valid(Prefix, #{type := object} = Schema) ->
    FunName = Prefix,
    {IsValidFuns, ExtraFuns} =
        lists:foldl(
            fun(Keyword, {IsValidFunsAcc, ExtraFunsAcc} = Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    _Value ->
                        case is_valid_(<<Prefix/binary, ".">>, Keyword, Schema) of
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
            ndto_generator:type_guard(object),
            ndto_generator:chain_conditions(FunName, ValidationConditions, 'andalso')
        ),
    FalseClause = ndto_generator:false_clause(<<FunName/binary, ".type">>, "Value is not an object"),
    Clauses = ndto_generator:clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec is_valid_(Prefix, Keyword, Schema) -> Result when
    Prefix :: binary(),
    Keyword :: atom(),
    Schema :: ndto:object_schema(),
    Result :: {Fun, ExtraFuns},
    Fun :: erl_syntax:syntaxTree() | undefined,
    ExtraFuns :: [erl_syntax:syntaxTree()].
is_valid_(Prefix, properties, Schema) ->
    is_valid_properties(Prefix, Schema);
is_valid_(Prefix, required, Schema) ->
    is_valid_required(Prefix, Schema);
is_valid_(Prefix, min_properties, Schema) ->
    is_valid_min_properties(Prefix, Schema);
is_valid_(Prefix, max_properties, Schema) ->
    is_valid_max_properties(Prefix, Schema);
is_valid_(Prefix, pattern_properties, Schema) ->
    is_valid_pattern_properties(Prefix, Schema);
is_valid_(Prefix, additional_properties, Schema) ->
    is_valid_additional_properties(Prefix, Schema).

is_valid_properties(Prefix, #{properties := Properties}) ->
    FunName = <<Prefix/binary, "properties">>,
    {PropertiesFuns, ExtraFuns} = maps:fold(
        fun(PropertyName, Property, {IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidPropertyFun, ExtraPropertyFuns} =
                ndto_generator:is_valid(<<FunName/binary, ".", PropertyName/binary>>, Property#{
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
    ValidationConditions = [
        erl_syntax:tuple([
            erl_syntax:implicit_fun(
                erl_syntax:arity_qualifier(
                    erl_syntax:function_name(PropertyFun),
                    erl_syntax:integer(erl_syntax:function_arity(PropertyFun))
                )
            ),
            erl_syntax:list([
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
        ])
     || {PropertyName, PropertyFun} <- PropertiesFuns
    ],
    FunBody = ndto_generator:chain_conditions(FunName, ValidationConditions, 'andalso'),

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
    {Fun, IsValidFuns ++ ExtraFuns}.

is_valid_required(Prefix, #{required := Required}) ->
    FunName = <<Prefix/binary, "required">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:atom(ndto_validation),
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
                                    erl_syntax:atom(erlang:binary_to_list(Prefix)),
                                    erl_syntax:application(
                                        erl_syntax:atom(erlang),
                                        erl_syntax:atom(list_to_binary),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom('io_lib'),
                                                erl_syntax:atom('format'),
                                                [
                                                    erl_syntax:string(
                                                        lists:flatten(
                                                            io_lib:format(
                                                                "~ts is missing required property ~~p",
                                                                [Prefix]
                                                            )
                                                        )
                                                    ),
                                                    erl_syntax:list([
                                                        erl_syntax:variable(
                                                            'MissingProperty'
                                                        )
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
        [TrueClause]
    ),
    {Fun, []}.

is_valid_min_properties(Prefix, #{min_properties := MinProperties}) ->
    FunName = <<Prefix/binary, "min_properties">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
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
                [
                    erl_syntax:clause(
                        [erl_syntax:variable('N')],
                        [
                            erl_syntax:infix_expr(
                                erl_syntax:variable('N'),
                                erl_syntax:operator('>='),
                                erl_syntax:integer(MinProperties)
                            )
                        ],
                        [erl_syntax:atom('true')]
                    ),
                    erl_syntax:clause(
                        [erl_syntax:variable('_')],
                        none,
                        [
                            ndto_generator:false_return(
                                FunName,
                                unicode:characters_to_list(
                                    io_lib:format(
                                        "Object has less properties than required minimum (~p)", [
                                            MinProperties
                                        ]
                                    )
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
    {Fun, []}.

is_valid_max_properties(Prefix, #{max_properties := MaxProperties}) ->
    FunName = <<Prefix/binary, "max_properties">>,
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
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
                [
                    erl_syntax:clause(
                        [erl_syntax:variable('N')],
                        [
                            erl_syntax:infix_expr(
                                erl_syntax:variable('N'),
                                erl_syntax:operator('=<'),
                                erl_syntax:integer(MaxProperties)
                            )
                        ],
                        [erl_syntax:atom('true')]
                    ),
                    erl_syntax:clause(
                        [erl_syntax:variable('_')],
                        none,
                        [
                            ndto_generator:false_return(
                                FunName,
                                unicode:characters_to_list(
                                    io_lib:format(
                                        "Object has more properties than allowed maximum (~p)", [
                                            MaxProperties
                                        ]
                                    )
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
    {Fun, []}.

is_valid_pattern_properties(Prefix, #{pattern_properties := PatternProperties}) ->
    FunName = <<Prefix/binary, "pattern_properties">>,
    {IsValidPatterns, ExtraFuns} = lists:foldl(
        fun({PropertyPattern, PropertySchema}, {IsValidPatternsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = ndto_generator:is_valid(
                <<FunName/binary, ".", PropertyPattern/binary>>, PropertySchema
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
                        erl_syntax:atom(ndto_validation),
                        erl_syntax:atom(find),
                        [
                            erl_syntax:fun_expr([
                                erl_syntax:clause(
                                    [
                                        erl_syntax:tuple([
                                            erl_syntax:variable('PropertyName'),
                                            erl_syntax:variable('PropertyValue')
                                        ])
                                    ],
                                    none,
                                    [
                                        erl_syntax:match_expr(
                                            erl_syntax:variable('Result'),
                                            erl_syntax:application(
                                                erl_syntax:function_name(IsValidFun),
                                                [erl_syntax:variable('PropertyValue')]
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
                                                            erl_syntax:tuple([
                                                                erl_syntax:variable('Prefix'),
                                                                erl_syntax:variable('Reason')
                                                            ])
                                                        ])
                                                    ],
                                                    none,
                                                    [
                                                        erl_syntax:tuple([
                                                            erl_syntax:atom('true'),
                                                            erl_syntax:tuple([
                                                                erl_syntax:tuple([
                                                                    erl_syntax:variable(
                                                                        'PropertyName'
                                                                    ),
                                                                    erl_syntax:variable(
                                                                        'PropertyValue'
                                                                    )
                                                                ]),
                                                                erl_syntax:tuple([
                                                                    erl_syntax:variable('Prefix'),
                                                                    erl_syntax:variable('Reason')
                                                                ])
                                                            ])
                                                        ])
                                                    ]
                                                )
                                            ]
                                        )
                                    ]
                                )
                            ]),
                            Filter
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
                                        erl_syntax:tuple([
                                            erl_syntax:variable('PropertyName'),
                                            erl_syntax:variable('_PropertyValue')
                                        ]),
                                        erl_syntax:tuple([
                                            erl_syntax:variable('Prefix'),
                                            erl_syntax:variable('Reason')
                                        ])
                                    ])
                                ])
                            ],
                            none,
                            [
                                erl_syntax:tuple([
                                    erl_syntax:atom('false'),
                                    erl_syntax:tuple([
                                        erl_syntax:variable('Prefix'),
                                        erl_syntax:application(
                                            erl_syntax:atom(erlang),
                                            erl_syntax:atom(list_to_binary),
                                            [
                                                erl_syntax:application(
                                                    erl_syntax:atom('io_lib'),
                                                    erl_syntax:atom('format'),
                                                    [
                                                        erl_syntax:string(
                                                            "Property \"~ts\" failed validation: ~ts"
                                                        ),
                                                        erl_syntax:list([
                                                            erl_syntax:variable('PropertyName'),
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
                ),
            erl_syntax:tuple([
                erl_syntax:fun_expr([
                    erl_syntax:clause(none, [FunBody])
                ]),
                erl_syntax:list([])
            ])
        end,
        IsValidPatterns
    ),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        ndto_generator:chain_conditions(FunName, Conditions, 'andalso')
    ),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ),
    IsValidFuns = [IsValidFun || {_PatternName, IsValidFun} <- IsValidPatterns],
    {Fun, IsValidFuns ++ ExtraFuns}.

is_valid_additional_properties(Prefix, #{additional_properties := false} = Schema) ->
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
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:atom(sets),
                    erl_syntax:atom(to_list),
                    [
                        erl_syntax:application(
                            erl_syntax:atom(sets),
                            erl_syntax:atom(subtract),
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
                                                                        erlang:binary_to_list(
                                                                            PropertyName
                                                                        )
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
                [
                    erl_syntax:clause(
                        [erl_syntax:list([])],
                        none,
                        [erl_syntax:atom('true')]
                    ),
                    erl_syntax:clause(
                        [erl_syntax:variable('UnsupportedKeys')],
                        none,
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:tuple([
                                    erl_syntax:atom(erlang:binary_to_list(FunName)),
                                    erl_syntax:application(
                                        erl_syntax:atom(erlang),
                                        erl_syntax:atom(list_to_binary),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom(io_lib),
                                                erl_syntax:atom(format),
                                                [
                                                    erl_syntax:string(
                                                        "Object has unsupported keys: ~ts"
                                                    ),
                                                    erl_syntax:list([
                                                        erl_syntax:application(
                                                            erl_syntax:atom(ndto_validation),
                                                            erl_syntax:atom(format_properties),
                                                            [
                                                                erl_syntax:variable(
                                                                    'UnsupportedKeys'
                                                                )
                                                            ]
                                                        )
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
        [TrueClause]
    ),
    {Fun, []};
is_valid_additional_properties(Prefix, #{additional_properties := AdditionalProperties} = Schema) ->
    FunName = <<Prefix/binary, "additional_properties.*">>,
    {IsValidFun, ExtraFuns} = ndto_generator:is_valid(
        <<Prefix/binary, "additional_properties">>, AdditionalProperties
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
                    erl_syntax:atom(ndto_validation),
                    erl_syntax:atom(find),
                    [
                        erl_syntax:fun_expr([
                            erl_syntax:clause(
                                [erl_syntax:variable('Property')],
                                none,
                                [
                                    erl_syntax:match_expr(
                                        erl_syntax:variable('Result'),
                                        erl_syntax:application(
                                            erl_syntax:function_name(IsValidFun),
                                            [
                                                erl_syntax:application(
                                                    erl_syntax:atom(maps),
                                                    erl_syntax:atom(get),
                                                    [
                                                        erl_syntax:variable('Property'),
                                                        erl_syntax:variable('Val')
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
                                                        erl_syntax:tuple([
                                                            erl_syntax:variable('Prefix'),
                                                            erl_syntax:variable('Reason')
                                                        ])
                                                    ])
                                                ],
                                                none,
                                                [
                                                    erl_syntax:tuple([
                                                        erl_syntax:atom('true'),
                                                        erl_syntax:tuple([
                                                            erl_syntax:variable('Property'),
                                                            erl_syntax:tuple([
                                                                erl_syntax:variable('Prefix'),
                                                                erl_syntax:variable('Reason')
                                                            ])
                                                        ])
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
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:atom('none')
                            ])
                        ],
                        none,
                        [erl_syntax:atom(true)]
                    ),
                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('true'),
                                erl_syntax:tuple([
                                    erl_syntax:variable('PropertyName'),
                                    erl_syntax:tuple([
                                        erl_syntax:variable('Prefix'),
                                        erl_syntax:variable('Reason')
                                    ])
                                ])
                            ])
                        ],
                        none,
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:tuple([
                                    erl_syntax:variable('Prefix'),
                                    erl_syntax:application(
                                        erl_syntax:atom(erlang),
                                        erl_syntax:atom(list_to_binary),
                                        [
                                            erl_syntax:application(
                                                erl_syntax:atom('io_lib'),
                                                erl_syntax:atom('format'),
                                                [
                                                    erl_syntax:string(
                                                        "Property \"~ts\" failed validation: ~ts"
                                                    ),
                                                    erl_syntax:list([
                                                        erl_syntax:variable('PropertyName'),
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
        [TrueClause]
    ),
    {Fun, [IsValidFun | ExtraFuns]};
is_valid_additional_properties(_Prefix, _Schema) ->
    {undefined, []}.
