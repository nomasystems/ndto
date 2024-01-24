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
-spec evaluate_conditions(FunctionName, Conditions, EvalueMode, IsSchemaComposition) -> Resp when
    FunctionName :: atom(),
    Conditions :: {mfa_call, [MfaCall]} | {fun_call, [FunCall]},
    MfaCall :: {function(), term()},
    FunCall :: function(),
    EvalueMode :: 'orelse' | 'andalso' | 'xor',
    IsSchemaComposition :: boolean(),
    Resp :: boolean() | {false, term()}.
evaluate_conditions(_FunctionName, {_ConditionsType, []}, 'andalso', _IsSchemaComposition) ->
    true;
evaluate_conditions(FunctionName, {_ConditionsType, []}, _EvalueMode, _IsSchemaComposition) ->
    {false, {FunctionName, <<"Value is not matching any of the (0) given conditions">>}};
evaluate_conditions(FunctionName, {ConditionsType, Conditions}, 'andalso', true) ->
    case internal_evaluate_conditions(ConditionsType, Conditions, 'andalso') of
        true ->
            true;
        {false, {AllOfReasonPath, ReasonMsg}, N} ->
            ReasonPath = atom_to_list(AllOfReasonPath),
            {false, {
                FunctionName,
                list_to_binary(
                    io_lib:format(
                        "Value is not matching all conditions. Condition ~p failed because of schema path '~ts' : ~ts",
                        [N, list_to_atom(ReasonPath), ReasonMsg]
                    )
                )
            }}
    end;
evaluate_conditions(_FunctionName, {ConditionsType, Conditions}, EvalueMode, false) ->
    case internal_evaluate_conditions(ConditionsType, Conditions, EvalueMode) of
        true -> true;
        {false, {ReasonPath, ReasonMsg}, _N} -> {false, {ReasonPath, ReasonMsg}}
    end;
evaluate_conditions(FunctionName, {ConditionsType, Conditions}, EvalueMode, _IsSchemaComposition) ->
    case internal_evaluate_conditions(ConditionsType, Conditions, EvalueMode) of
        true ->
            true;
        false ->
            {false, {
                FunctionName,
                <<"Value is not matching at least one condition. None matched.">>
            }};
        {false, none_matched} ->
            {false, {
                FunctionName,
                <<"Value is not matching exactly one condition. None matched.">>
            }};
        {false, {many_matched, [First, Second]}} ->
            {false, {
                FunctionName,
                list_to_binary(
                    io_lib:format(
                        "Value is not matching exactly one condition. More than one (conditions ~p and ~p) matched.",
                        [Second, First]
                    )
                )
            }}
    end.

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
internal_evaluate_conditions(fun_call, [Fun | Rest], EvalueMode) ->
    next_evaluate_condition(fun_call, Fun(), Rest, EvalueMode);
internal_evaluate_conditions(mfa_call, [{Function, Args} | Rest], EvalueMode) ->
    next_evaluate_condition(mfa_call, Function(Args), Rest, EvalueMode).

-spec next_evaluate_condition(ConditionsType, CurrentResult, Conditions, EvalueMode) -> Resp when
    ConditionsType :: fun_call | mfa_call,
    CurrentResult :: true | {false, term()},
    Conditions :: [Condition],
    Condition :: {fun_call, fun()} | {mfa_call, {function(), term()}},
    EvalueMode ::
        'orelse'
        | 'andalso'
        | {'andalso', ConditionIndex}
        | 'xor'
        | {'xor', ConditionIndex, MatchedIndexes},
    ConditionIndex :: non_neg_integer(),
    MatchedIndexes :: [ConditionIndex],
    Resp :: boolean() | {false, Reason} | {false, AndalsoReason, ConditionIndex},
    Reason :: none_matched | {many_matched, MatchedIndexes},
    AndalsoReason :: term().
next_evaluate_condition(_ConditionsType, true, _Rest, 'orelse') ->
    true;
next_evaluate_condition(_ConditionsType, _Result, [], 'orelse') ->
    false;
next_evaluate_condition(ConditionsType, {false, _}, Rest, 'orelse') ->
    internal_evaluate_conditions(ConditionsType, Rest, 'orelse');
next_evaluate_condition(ConditionsType, Result, Rest, 'xor') ->
    ConditionIndex = length(Rest),
    next_evaluate_condition(ConditionsType, Result, Rest, {'xor', ConditionIndex, []});
next_evaluate_condition(_ConditionsType, true, [], {'xor', _N, []}) ->
    true;
next_evaluate_condition(_ConditionsType, _Result, [], {'xor', _N, []}) ->
    {false, none_matched};
next_evaluate_condition(_ConditionsType, true, _Rest, {'xor', N, [One]}) ->
    {false, {many_matched, [One, N]}};
next_evaluate_condition(_ConditionsType, _Result, [], {'xor', _N, [_One]}) ->
    true;
next_evaluate_condition(ConditionsType, true, Rest, {'xor', N, []}) ->
    internal_evaluate_conditions(ConditionsType, Rest, {'xor', N - 1, [N]});
next_evaluate_condition(ConditionsType, {false, _}, Rest, {'xor', N, []}) ->
    internal_evaluate_conditions(ConditionsType, Rest, {'xor', N - 1, []});
next_evaluate_condition(ConditionsType, {false, _}, Rest, {'xor', N, [One]}) ->
    internal_evaluate_conditions(ConditionsType, Rest, {'xor', N - 1, [One]});
next_evaluate_condition(_ConditionsType, Result, Rest, 'andalso') ->
    ConditionIndex = length(Rest),
    next_evaluate_condition(_ConditionsType, Result, Rest, {'andalso', ConditionIndex});
next_evaluate_condition(_ConditionsType, true, [], {'andalso', _N}) ->
    true;
next_evaluate_condition(_ConditionsType, {false, Reason}, [], {'andalso', N}) ->
    {false, Reason, N};
next_evaluate_condition(ConditionsType, true, Rest, {'andalso', N}) ->
    internal_evaluate_conditions(ConditionsType, Rest, {'andalso', N - 11});
next_evaluate_condition(_ConditionsType, {false, Reason}, _Rest, {'andalso', N}) ->
    {false, Reason, N}.
