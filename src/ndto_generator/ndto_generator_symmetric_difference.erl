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
-module(ndto_generator_symmetric_difference).

%%% BEHAVIOR
-behaviour(ndto_generator).

%%% EXTERNAL EXPORTS
-export([is_valid/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
is_valid(Prefix, #{one_of := Subschemas} = Schema) when is_list(Subschemas) ->
    FunName = <<Prefix/binary, ".one_of">>,
    {_Idx, IsValidFuns, ExtraFuns} = lists:foldl(
        fun(Subschema, {Idx, IsValidFunsAcc, ExtraFunsAcc}) ->
            {IsValidFun, ExtraFuns} = ndto_generator:is_valid(
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
            none,
            [
                erl_syntax:case_expr(
                    erlang:hd(ndto_generator:chain_conditions(FunName, ValidationConditions, 'xor')),
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
                                    erl_syntax:atom('none_matched')
                                ])
                            ],
                            none,
                            [
                                erl_syntax:tuple([
                                    erl_syntax:atom('false'),
                                    erl_syntax:tuple([
                                        erl_syntax:atom(binary_to_atom(FunName)),
                                        erl_syntax:binary([
                                            erl_syntax:binary_field(
                                                erl_syntax:string(
                                                    "Value is not matching exactly one condition. None matched."
                                                )
                                            )
                                        ])
                                    ])
                                ])
                            ]
                        ),
                        erl_syntax:clause(
                            [
                                erl_syntax:tuple([
                                    erl_syntax:atom('false'),
                                    erl_syntax:tuple([
                                        erl_syntax:atom('many_matched'),
                                        erl_syntax:tuple([
                                            erl_syntax:variable('First'),
                                            erl_syntax:variable('Second')
                                        ])
                                    ])
                                ])
                            ],
                            none,
                            [
                                erl_syntax:tuple([
                                    erl_syntax:atom('false'),
                                    erl_syntax:tuple([
                                        erl_syntax:atom(binary_to_atom(FunName)),
                                        erl_syntax:application(
                                            erl_syntax:atom('unicode'),
                                            erl_syntax:atom('characters_to_binary'),
                                            [
                                                erl_syntax:application(
                                                    erl_syntax:atom('io_lib'),
                                                    erl_syntax:atom('format'),
                                                    [
                                                        erl_syntax:string(
                                                            "Value is not matching exactly one condition. More than one (conditions ~p and ~p) matched."
                                                        ),
                                                        erl_syntax:list([
                                                            erl_syntax:variable('Second'),
                                                            erl_syntax:variable('First')
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
    Clauses = ndto_generator:clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, IsValidFuns ++ ExtraFuns}.
