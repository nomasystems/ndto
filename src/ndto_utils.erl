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
    chain_conditions/4,
    mfoldl/3,
    find/2,
    find_value/2,
    format_properties/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec chain_conditions(FunctionName, Conditions, Operator, IsSchemaComposition) -> Resp when
    FunctionName :: atom(),
    Conditions :: {fa_condition, [FACondition]} | {fun_condition, [FunCondition]},
    FACondition :: {FunctionName, Argument},
    FunctionName :: atom(),
    Argument :: term(),
    FunCondition :: function(),
    Operator :: 'orelse' | 'andalso' | 'xor',
    IsSchemaComposition :: boolean(),
    Resp :: boolean() | {false, term()}.
chain_conditions(FunctionName, Conditions, 'andalso', IsSchemaComposition) ->
    andalso_(FunctionName, Conditions, IsSchemaComposition);
chain_conditions(FunctionName, Conditions, 'orelse', IsSchemaComposition) ->
    orelse_(FunctionName, Conditions, IsSchemaComposition);
chain_conditions(FunctionName, Conditions, 'xor', _IsSchemaComposition) ->
    xor_(FunctionName, Conditions).

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
andalso_(_FunctionName, {_ConditionsType, []}, _IsSchemaComposition) ->
    true;
andalso_(FunctionName, {ConditionsType, Conditions}, true) ->
    case evaluate_andalso(ConditionsType, Conditions) of
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
andalso_(_FunctionName, {ConditionsType, Conditions}, false) ->
    case evaluate_andalso(ConditionsType, Conditions) of
        true -> true;
        {false, {ReasonPath, ReasonMsg}, _ConditionIndex} -> {false, {ReasonPath, ReasonMsg}}
    end.

orelse_(FunctionName, {_ConditionsType, []}, _IsSchemaComposition) ->
    {false, {FunctionName, <<"Value is not matching any of the (0) given conditions">>}};
orelse_(FunctionName, {ConditionsType, Conditions}, false) ->
    case evaluate_orelse(ConditionsType, Conditions) of
        true -> true;
        false -> {false, {FunctionName, <<"Value is not matching any of the given conditions">>}}
    end;
orelse_(FunctionName, {ConditionsType, Conditions}, _IsSchemaComposition) ->
    case evaluate_orelse(ConditionsType, Conditions) of
        true ->
            true;
        false ->
            {false, {
                FunctionName,
                <<"Value is not matching at least one condition. None matched.">>
            }}
    end.

xor_(FunctionName, {ConditionsType, Conditions}) ->
    case evaluate_xor(ConditionsType, Conditions) of
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

evaluate_andalso(ConditionsType, Conditions) ->
    Acc = length(Conditions) - 1,
    evaluate_andalso(ConditionsType, Conditions, Acc).

evaluate_andalso(fun_condition, [Fun | Rest], Acc) ->
    next_andalso_(fun_condition, Fun(), Rest, Acc);
evaluate_andalso(fa_condition, [{Function, Args} | Rest], Acc) ->
    next_andalso_(fa_condition, Function(Args), Rest, Acc).

evaluate_orelse(fun_condition, [Fun | Rest]) ->
    next_orelse_(fun_condition, Fun(), Rest);
evaluate_orelse(fa_condition, [{Function, Args} | Rest]) ->
    next_orelse_(fa_condition, Function(Args), Rest).

evaluate_xor(ConditionsType, Conditions) ->
    FirstConditionIndex = length(Conditions) - 1,
    evaluate_xor(ConditionsType, Conditions, {FirstConditionIndex, []}).

evaluate_xor(fun_condition, [Fun | Rest], Acc) ->
    next_xor_(fun_condition, Fun(), Rest, Acc);
evaluate_xor(fa_condition, [{Function, Args} | Rest], Acc) ->
    next_xor_(fa_condition, Function(Args), Rest, Acc).

next_andalso_(_ConditionsType, true, [], _ConditionIndex) ->
    true;
next_andalso_(_ConditionsType, {false, Reason}, [], ConditionIndex) ->
    {false, Reason, ConditionIndex};
next_andalso_(ConditionsType, true, Rest, ConditionIndex) ->
    evaluate_andalso(ConditionsType, Rest, ConditionIndex - 1);
next_andalso_(_ConditionsType, {false, Reason}, _Rest, ConditionIndex) ->
    {false, Reason, ConditionIndex}.

next_orelse_(_ConditionsType, true, _Rest) ->
    true;
next_orelse_(_ConditionsType, _Result, []) ->
    false;
next_orelse_(ConditionsType, {false, _}, Rest) ->
    evaluate_orelse(ConditionsType, Rest).

next_xor_(_ConditionsType, true, [], {_ConditionIndex, []}) ->
    true;
next_xor_(_ConditionsType, _Result, [], {_ConditionIndex, []}) ->
    {false, none_matched};
next_xor_(_ConditionsType, true, _Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    {false, {many_matched, [LastMatchedCondition, ConditionIndex]}};
next_xor_(_ConditionsType, _Result, [], {_ConditionIndex, [_LastMatchedCondition]}) ->
    true;
next_xor_(ConditionsType, true, Rest, {ConditionIndex, []}) ->
    evaluate_xor(ConditionsType, Rest, {ConditionIndex - 1, [ConditionIndex]});
next_xor_(ConditionsType, {false, _}, Rest, {ConditionIndex, []}) ->
    evaluate_xor(ConditionsType, Rest, {ConditionIndex - 1, []});
next_xor_(ConditionsType, {false, _}, Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    evaluate_xor(ConditionsType, Rest, {ConditionIndex - 1, [LastMatchedCondition]}).
