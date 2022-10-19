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
is_valid(Prefix, #{<<"type">> := <<"string">>} = Schema) ->
    Funs =
        lists:foldl(
            fun(Keyword, Acc) ->
                case maps:get(Keyword, Schema, undefined) of
                    undefined ->
                        Acc;
                    Value ->
                        [is_valid_string(Prefix, Keyword, Value) | Acc]
                end
            end,
            [],
            [<<"enum">>, <<"minLength">>, <<"maxLength">>, <<"format">>, <<"pattern">>]
        ),
    {Funs, []};
is_valid(_Prefix, #{<<"type">> := <<"number">>} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(_Prefix, #{<<"type">> := <<"integer">>} = _Schema) ->
    erlang:throw(not_implemented);
is_valid(Prefix, #{<<"type">> := <<"boolean">>} = Schema) ->
    FunName = <<Prefix/binary, "any">>,
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
    {[Fun], []};
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

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
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
    );
is_valid_string(_Prefix, _Keyword, _Value) ->
    erlang:throw(not_implemented).
