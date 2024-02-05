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
-module(ndto_utils).

%%% EXTERNAL EXPORTS
-export([
    evaluate_conditions/4,
    mfoldl/3,
    find/2,
    find_value/2,
    format_properties/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec evaluate_conditions(FunctionName, Conditions, EvaluateMode, IsSchemaComposition) -> Resp when
    FunctionName :: atom(),
    Conditions :: {function_condition, [FArgCondition]} | {fun_condition, [FunCondition]},
    FArgCondition :: {FunctionName, Argument},
    FunctionName :: atom(),
    Argument :: term(),
    FunCondition :: function(),
    EvaluateMode :: 'orelse' | 'andalso' | 'xor',
    IsSchemaComposition :: boolean(),
    Resp :: boolean() | {false, term()}.
evaluate_conditions(FunctionName, Conditions, 'andalso', IsSchemaComposition) ->
    evaluate_andalso(FunctionName, Conditions, IsSchemaComposition);
evaluate_conditions(FunctionName, Conditions, 'orelse', IsSchemaComposition) ->
    evaluate_orelse(FunctionName, Conditions, IsSchemaComposition);
evaluate_conditions(FunctionName, Conditions, 'xor', _IsSchemaComposition) ->
    evaluate_xor(FunctionName, Conditions).

-spec mfoldl(Fun, Acc, List) -> Resp when
    Fun :: function(),
    Acc :: term(),
    List :: list(),
    Resp :: {true, term()} | {false, term(), Reason},
    Reason :: binary().
mfoldl(_Fun, Acc, []) ->
    {true, Acc};
mfoldl(Fun, Acc, [H | T]) ->
    case Fun(H, Acc) of
        {true, NewAcc} ->
            mfoldl(Fun, NewAcc, T);
        {{false, Reason}, NewAcc} ->
            {false, NewAcc, Reason}
    end.

-spec find(Fun, List) -> Resp when
    Fun :: function(),
    List :: list(),
    Resp :: {true, FoundItem} | {false, none},
    FoundItem :: term().
find(_Fun, []) ->
    {false, none};
find(Fun, [H | T]) ->
    case Fun(H) of
        false -> find(Fun, T);
        true -> {true, H}
    end.

-spec find_value(Fun, List) -> Resp when
    Fun :: function(),
    List :: list(),
    Resp :: {true, ListElement, FunResult} | {false, none},
    ListElement :: term(),
    FunResult :: term().
find_value(_Fun, []) ->
    {false, none};
find_value(Fun, [H | T]) ->
    case Fun(H) of
        false -> find_value(Fun, T);
        {true, Result} -> {true, H, Result}
    end.

-spec format_properties(List) -> Resp when
    List :: nonempty_list(Property),
    Property :: binary(),
    Resp :: binary().
format_properties([Head | List]) ->
    lists:foldl(
        fun(Term, Acc) ->
            <<Acc/binary, ", \"", Term/binary, "\"">>
        end,
        <<"\"", Head/binary, "\"">>,
        List
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
evaluate_andalso(_FunctionName, {_ConditionsType, []}, _IsSchemaComposition) ->
    true;
evaluate_andalso(FunctionName, {ConditionsType, Conditions}, true) ->
    case internal_evaluate_andalso(ConditionsType, Conditions) of
        true ->
            true;
        {false, {AllOfReasonPath, ReasonMsg}, N} ->
            ReasonPath = atom_to_list(AllOfReasonPath),
            {false, {
                FunctionName,
                unicode:characters_to_binary(
                    io_lib:format(
                        "Value is not matching all conditions. Condition ~p failed because of schema path '~ts' : ~ts",
                        [N, list_to_atom(ReasonPath), ReasonMsg]
                    )
                )
            }}
    end;
evaluate_andalso(_FunctionName, {ConditionsType, Conditions}, false) ->
    case internal_evaluate_andalso(ConditionsType, Conditions) of
        true -> true;
        {false, {ReasonPath, ReasonMsg}, _ConditionIndex} -> {false, {ReasonPath, ReasonMsg}}
    end.

evaluate_orelse(FunctionName, {_ConditionsType, []}, _IsSchemaComposition) ->
    {false, {FunctionName, <<"Value is not matching any of the (0) given conditions">>}};
evaluate_orelse(FunctionName, {ConditionsType, Conditions}, false) ->
    case internal_evaluate_orelse(ConditionsType, Conditions) of
        true -> true;
        false -> {false, {FunctionName, <<"Value is not matching any of the given conditions">>}}
    end;
evaluate_orelse(FunctionName, {ConditionsType, Conditions}, _IsSchemaComposition) ->
    case internal_evaluate_orelse(ConditionsType, Conditions) of
        true ->
            true;
        false ->
            {false, {
                FunctionName,
                <<"Value is not matching at least one condition. None matched.">>
            }}
    end.

evaluate_xor(FunctionName, {ConditionsType, Conditions}) ->
    case internal_evaluate_xor(ConditionsType, Conditions) of
        true ->
            true;
        {false, none_matched} ->
            {false, {
                FunctionName,
                <<"Value is not matching exactly one condition. None matched.">>
            }};
        {false, {many_matched, [First, Second]}} ->
            {false, {
                FunctionName,
                unicode:characters_to_binary(
                    io_lib:format(
                        "Value is not matching exactly one condition. More than one (conditions ~p and ~p) matched.",
                        [Second, First]
                    )
                )
            }}
    end.

internal_evaluate_andalso(ConditionsType, Conditions) ->
    Acc = length(Conditions) - 1,
    internal_evaluate_andalso(ConditionsType, Conditions, Acc).

internal_evaluate_andalso(fun_condition, [Fun | Rest], Acc) ->
    next_evaluate_andalso(fun_condition, Fun(), Rest, Acc);
internal_evaluate_andalso(function_condition, [{Function, Args} | Rest], Acc) ->
    next_evaluate_andalso(function_condition, Function(Args), Rest, Acc).

internal_evaluate_orelse(fun_condition, [Fun | Rest]) ->
    next_evaluate_orelse(fun_condition, Fun(), Rest);
internal_evaluate_orelse(function_condition, [{Function, Args} | Rest]) ->
    next_evaluate_orelse(function_condition, Function(Args), Rest).

internal_evaluate_xor(ConditionsType, Conditions) ->
    FirstConditionIndex = length(Conditions) - 1,
    internal_evaluate_xor(ConditionsType, Conditions, {FirstConditionIndex, []}).

internal_evaluate_xor(fun_condition, [Fun | Rest], Acc) ->
    next_evaluate_xor(fun_condition, Fun(), Rest, Acc);
internal_evaluate_xor(function_condition, [{Function, Args} | Rest], Acc) ->
    next_evaluate_xor(function_condition, Function(Args), Rest, Acc).

next_evaluate_andalso(_ConditionsType, true, [], _ConditionIndex) ->
    true;
next_evaluate_andalso(_ConditionsType, {false, Reason}, [], ConditionIndex) ->
    {false, Reason, ConditionIndex};
next_evaluate_andalso(ConditionsType, true, Rest, ConditionIndex) ->
    internal_evaluate_andalso(ConditionsType, Rest, ConditionIndex - 1);
next_evaluate_andalso(_ConditionsType, {false, Reason}, _Rest, ConditionIndex) ->
    {false, Reason, ConditionIndex}.

next_evaluate_orelse(_ConditionsType, true, _Rest) ->
    true;
next_evaluate_orelse(_ConditionsType, _Result, []) ->
    false;
next_evaluate_orelse(ConditionsType, {false, _}, Rest) ->
    internal_evaluate_orelse(ConditionsType, Rest).

next_evaluate_xor(_ConditionsType, true, [], {_ConditionIndex, []}) ->
    true;
next_evaluate_xor(_ConditionsType, _Result, [], {_ConditionIndex, []}) ->
    {false, none_matched};
next_evaluate_xor(_ConditionsType, true, _Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    {false, {many_matched, [LastMatchedCondition, ConditionIndex]}};
next_evaluate_xor(_ConditionsType, _Result, [], {_ConditionIndex, [_LastMatchedCondition]}) ->
    true;
next_evaluate_xor(ConditionsType, true, Rest, {ConditionIndex, []}) ->
    internal_evaluate_xor(ConditionsType, Rest, {ConditionIndex - 1, [ConditionIndex]});
next_evaluate_xor(ConditionsType, {false, _}, Rest, {ConditionIndex, []}) ->
    internal_evaluate_xor(ConditionsType, Rest, {ConditionIndex - 1, []});
next_evaluate_xor(ConditionsType, {false, _}, Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    internal_evaluate_xor(ConditionsType, Rest, {ConditionIndex - 1, [LastMatchedCondition]}).
