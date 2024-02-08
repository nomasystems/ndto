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
    andalso_/1,
    orelse_/1,
    xor_/1,
    mfoldl/3,
    find/2,
    format_properties/1
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec andalso_(Conditions) -> Result when
    Conditions :: [Condition],
    Condition :: {FunctionName, Argument} | fun(() -> Result),
    FunctionName :: function(),
    Argument :: term(),
    Result :: true | {false, Reason},
    Reason :: {SchemaPath, Description, FailedIndex},
    SchemaPath :: atom(),
    Description :: binary(),
    FailedIndex :: non_neg_integer().
andalso_([]) ->
    true;
andalso_(Conditions) ->
    Acc = erlang:length(Conditions) - 1,
    andalso_(Conditions, Acc).

-spec orelse_(Conditions) -> Result when
    Conditions :: [Condition],
    Condition :: {FunctionName, Argument} | fun(() -> Result),
    FunctionName :: function(),
    Argument :: term(),
    Result :: true | false.
orelse_([]) ->
    false;
orelse_([{Function, Args} | Rest]) ->
    next_orelse_(Function(Args), Rest);
orelse_([Fun | Rest]) ->
    next_orelse_(Fun(), Rest).

-spec xor_(Conditions) -> Result when
    Conditions :: [Condition],
    Condition :: {FunctionName, Argument} | fun(() -> Result),
    FunctionName :: function(),
    Argument :: term(),
    Result :: true | {false, Reason},
    Reason ::
        {SchemaPath, none_matched}
        | {SchemaPath, {many_matched, {ValidationError, ValidationError}}},
    SchemaPath :: atom().
xor_(Conditions) ->
    FirstConditionIndex = erlang:length(Conditions) - 1,
    xor_(Conditions, {FirstConditionIndex, []}).

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

-spec find(Predicate, List) -> Resp when
    Predicate :: function(),
    List :: list(),
    Resp :: {true, Result} | {false, none},
    Result :: term().
find(_Predicate, []) ->
    {false, none};
find(Predicate, [H | T]) ->
    case Predicate(H) of
        false -> find(Predicate, T);
        true -> {true, H};
        {true, Result} -> {true, Result}
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
andalso_([{Function, Args} | Rest], Acc) ->
    next_andalso_(Function(Args), Rest, Acc);
andalso_([Fun | Rest], Acc) ->
    next_andalso_(Fun(), Rest, Acc).

next_andalso_(true, [], _ConditionIndex) ->
    true;
next_andalso_({false, Reason}, [], ConditionIndex) ->
    {false, {Reason, ConditionIndex}};
next_andalso_(true, Rest, ConditionIndex) ->
    andalso_(Rest, ConditionIndex - 1);
next_andalso_({false, Reason}, _Rest, ConditionIndex) ->
    {false, {Reason, ConditionIndex}}.

next_orelse_(true, _Rest) ->
    true;
next_orelse_(_Result, []) ->
    false;
next_orelse_({false, _}, Rest) ->
    orelse_(Rest).

xor_([{Function, Args} | Rest], Acc) ->
    next_xor_(Function(Args), Rest, Acc);
xor_([Fun | Rest], Acc) ->
    next_xor_(Fun(), Rest, Acc).

next_xor_(true, [], {_ConditionIndex, []}) ->
    true;
next_xor_(_Result, [], {_ConditionIndex, []}) ->
    {false, none_matched};
next_xor_(true, _Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    {false, {many_matched, {LastMatchedCondition, ConditionIndex}}};
next_xor_(_Result, [], {_ConditionIndex, [_LastMatchedCondition]}) ->
    true;
next_xor_(true, Rest, {ConditionIndex, []}) ->
    xor_(Rest, {ConditionIndex - 1, [ConditionIndex]});
next_xor_({false, _}, Rest, {ConditionIndex, []}) ->
    xor_(Rest, {ConditionIndex - 1, []});
next_xor_({false, _}, Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    xor_(Rest, {ConditionIndex - 1, [LastMatchedCondition]}).
