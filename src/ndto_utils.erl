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
    chain_conditions/2,
    mfoldl/3,
    find/2,
    find_value/2,
    format_properties/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec chain_conditions(Conditions, Operator) -> Result when
    Conditions :: [Condition],
    Condition :: {FunctionName, Argument} | fun(() -> Result),
    FunctionName :: atom(),
    Argument :: term(),
    Operator :: 'orelse' | 'andalso' | 'xor',
    Result :: true | {false, Reason},
    Reason :: term().
chain_conditions(Conditions, 'andalso') ->
    andalso_(Conditions);
chain_conditions(Conditions, 'orelse') ->
    orelse_(Conditions);
chain_conditions(Conditions, 'xor') ->
    xor_(Conditions).

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
andalso_([]) ->
    true;
andalso_(Conditions) ->
    evaluate_andalso(Conditions).

orelse_([]) ->
    false;
orelse_(Conditions) ->
    evaluate_orelse(Conditions).

xor_(Conditions) ->
    evaluate_xor(Conditions).

evaluate_andalso(Conditions) ->
    Acc = erlang:length(Conditions) - 1,
    evaluate_andalso(Conditions, Acc).

evaluate_andalso([{Function, Args} | Rest], Acc) ->
    next_andalso_(Function(Args), Rest, Acc);
evaluate_andalso([Fun | Rest], Acc) ->
    next_andalso_(Fun(), Rest, Acc).

evaluate_orelse([{Function, Args} | Rest]) ->
    next_orelse_(Function(Args), Rest);
evaluate_orelse([Fun | Rest]) ->
    next_orelse_(Fun(), Rest).

evaluate_xor(Conditions) ->
    FirstConditionIndex = erlang:length(Conditions) - 1,
    evaluate_xor(Conditions, {FirstConditionIndex, []}).

evaluate_xor([{Function, Args} | Rest], Acc) ->
    next_xor_(Function(Args), Rest, Acc);
evaluate_xor([Fun | Rest], Acc) ->
    next_xor_(Fun(), Rest, Acc).

next_andalso_(true, [], _ConditionIndex) ->
    true;
next_andalso_({false, Reason}, [], ConditionIndex) ->
    {false, {Reason, ConditionIndex}};
next_andalso_(true, Rest, ConditionIndex) ->
    evaluate_andalso(Rest, ConditionIndex - 1);
next_andalso_({false, Reason}, _Rest, ConditionIndex) ->
    {false, {Reason, ConditionIndex}}.

next_orelse_(true, _Rest) ->
    true;
next_orelse_(_Result, []) ->
    false;
next_orelse_({false, _}, Rest) ->
    evaluate_orelse(Rest).

next_xor_(true, [], {_ConditionIndex, []}) ->
    true;
next_xor_(_Result, [], {_ConditionIndex, []}) ->
    {false, none_matched};
next_xor_(true, _Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    {false, {many_matched, [LastMatchedCondition, ConditionIndex]}};
next_xor_(_Result, [], {_ConditionIndex, [_LastMatchedCondition]}) ->
    true;
next_xor_(true, Rest, {ConditionIndex, []}) ->
    evaluate_xor(Rest, {ConditionIndex - 1, [ConditionIndex]});
next_xor_({false, _}, Rest, {ConditionIndex, []}) ->
    evaluate_xor(Rest, {ConditionIndex - 1, []});
next_xor_({false, _}, Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    evaluate_xor(Rest, {ConditionIndex - 1, [LastMatchedCondition]}).
