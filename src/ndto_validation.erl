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
-module(ndto_validation).

%%% EXTERNAL EXPORTS
-export([
    'andalso'/1,
    'orelse'/1,
    'xor'/1,
    mfoldl/3,
    find/2,
    format_properties/1
]).

%%% TYPES
-type t() :: true | {false, term()}.
-type condition() :: {condition_fun(), condition_args()}.
-type condition_fun() :: fun((term()) -> t()).
-type condition_args() :: [term()].

%%% EXPORT TYPES
-export_type([
    t/0,
    condition/0
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec 'andalso'(Conditions) -> Result when
    Conditions :: [condition()],
    Result :: true | {false, Reason},
    Reason :: {SchemaPath, Description, FailedIndex},
    SchemaPath :: atom(),
    Description :: binary(),
    FailedIndex :: non_neg_integer().
'andalso'([]) ->
    true;
'andalso'(Conditions) ->
    Acc = erlang:length(Conditions) - 1,
    'andalso'(Conditions, Acc).

-spec 'orelse'(Conditions) -> Result when
    Conditions :: [condition()],
    Result :: true | {false, Reason},
    Reason :: none_matched.
'orelse'([]) ->
    {false, none_matched};
'orelse'([{Function, Args} | Rest]) ->
    next_orelse(erlang:apply(Function, Args), Rest).

-spec 'xor'(Conditions) -> Result when
    Conditions :: [condition()],
    Result :: true | {false, Reason},
    Reason ::
        none_matched
        | {many_matched, {ValidationError, ValidationError}},
    ValidationError :: {SchemaPath, Description},
    SchemaPath :: atom(),
    Description :: binary().
'xor'(Conditions) ->
    FirstConditionIndex = erlang:length(Conditions) - 1,
    'xor'(Conditions, {FirstConditionIndex, []}).

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
'andalso'([{Function, Args} | Rest], Acc) ->
    next_andalso(erlang:apply(Function, Args), Rest, Acc).

next_andalso(true, [], _ConditionIndex) ->
    true;
next_andalso({false, Reason}, [], ConditionIndex) ->
    {false, {Reason, ConditionIndex}};
next_andalso(true, Rest, ConditionIndex) ->
    'andalso'(Rest, ConditionIndex - 1);
next_andalso({false, Reason}, _Rest, ConditionIndex) ->
    {false, {Reason, ConditionIndex}}.

next_orelse(true, _Rest) ->
    true;
next_orelse(_Result, []) ->
    {false, none_matched};
next_orelse({false, _}, Rest) ->
    'orelse'(Rest).

'xor'([{Function, Args} | Rest], Acc) ->
    next_xor(erlang:apply(Function, Args), Rest, Acc);
'xor'([Fun | Rest], Acc) ->
    next_xor(Fun(), Rest, Acc).

next_xor(true, [], {_ConditionIndex, []}) ->
    true;
next_xor(_Result, [], {_ConditionIndex, []}) ->
    {false, none_matched};
next_xor(true, _Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    {false, {many_matched, {LastMatchedCondition, ConditionIndex}}};
next_xor(_Result, [], {_ConditionIndex, [_LastMatchedCondition]}) ->
    true;
next_xor(true, Rest, {ConditionIndex, []}) ->
    'xor'(Rest, {ConditionIndex - 1, [ConditionIndex]});
next_xor({false, _}, Rest, {ConditionIndex, []}) ->
    'xor'(Rest, {ConditionIndex - 1, []});
next_xor({false, _}, Rest, {ConditionIndex, [LastMatchedCondition]}) ->
    'xor'(Rest, {ConditionIndex - 1, [LastMatchedCondition]}).
