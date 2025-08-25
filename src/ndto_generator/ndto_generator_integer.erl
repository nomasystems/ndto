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
-module(ndto_generator_integer).

%%% BEHAVIOR
-behaviour(ndto_generator).

%%% EXTERNAL EXPORTS
-export([is_valid/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
is_valid(Prefix, #{type := integer} = Schema) ->
    FunName = Prefix,
    ExtraFuns = lists:foldl(
        fun(Keyword, Acc) ->
            case maps:get(Keyword, Schema, undefined) of
                undefined ->
                    Acc;
                Value ->
                    case is_valid_(FunName, Keyword, Value, Schema) of
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
            multiple_of
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
     || Fun <- ExtraFuns
    ],
    OptionalClause = ndto_generator:optional_clause(Schema),
    NullClause = ndto_generator:null_clause(Schema),
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            ndto_generator:type_guard(integer),
            ndto_generator:chain_conditions(FunName, ValidationConditions, 'andalso')
        ),
    FalseClause = ndto_generator:false_clause(<<FunName/binary, ".type">>, "Value is not an integer"),
    Clauses = ndto_generator:clauses([OptionalClause, NullClause, TrueClause, FalseClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, ExtraFuns}.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec is_valid_(Prefix, Keyword, Value, Schema) -> Result when
    Prefix :: binary(),
    Keyword :: atom(),
    Value :: term(),
    Schema :: ndto:integer_schema(),
    Result :: undefined | erl_syntax:syntaxTree().
is_valid_(Prefix, minimum, Minimum, Schema) ->
    is_valid_minimum(Prefix, Minimum, Schema);
is_valid_(Prefix, maximum, Maximum, Schema) ->
    is_valid_maximum(Prefix, Maximum, Schema);
is_valid_(Prefix, multiple_of, MultipleOf, _Schema) ->
    is_valid_multiple_of(Prefix, MultipleOf);
is_valid_(_Prefix, _Keyword, _Value, _Schema) ->
    undefined.

is_valid_minimum(Prefix, Minimum, Schema) ->
    FunName = <<Prefix/binary, ".minimum">>,
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
    FalseClause = ndto_generator:false_clause(
        FunName,
        unicode:characters_to_list(
            io_lib:format("Value is not a number greater ~s ~p", [ComparisonTerm, Minimum])
        )
    ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    ).

is_valid_maximum(Prefix, Maximum, Schema) ->
    FunName = <<Prefix/binary, ".maximum">>,
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
    FalseClause = ndto_generator:false_clause(
        FunName,
        unicode:characters_to_list(
            io_lib:format("Number is not lower ~s ~p", [ComparisonTerm, Maximum])
        )
    ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause, FalseClause]
    ).

is_valid_multiple_of(Prefix, MultipleOf) ->
    FunName = <<Prefix/binary, ".multiple_of">>,
    TrueClause =
        erl_syntax:clause(
            [erl_syntax:variable('Val')],
            none,
            [
                erl_syntax:case_expr(
                    erl_syntax:infix_expr(
                        erl_syntax:variable('Val'),
                        erl_syntax:operator('rem'),
                        erl_syntax:integer(MultipleOf)
                    ),
                    [
                        erl_syntax:clause(
                            [erl_syntax:integer(0)],
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
                                        io_lib:format("Value is not multiple of ~p", [MultipleOf])
                                    )
                                )
                            ]
                        )
                    ]
                )
            ]
        ),
    erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        [TrueClause]
    ).
