-module(ndto_utils).

%%% EXTERNAL EXPORTS
-export([
    evalue_conditions/3,
    mfoldl/3,
    find/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec evalue_conditions(FunctionName, Conditions, EvalueMode) -> Resp when
    FunctionName :: atom(),
    Conditions :: {mfa_call, [] | [MfaCall]} | {fun_call, [] | [FunCall]},
    MfaCall :: {function(), term()},
    FunCall :: function(),
    EvalueMode :: 'orelse' | 'andalso' | 'xor',
    Resp :: boolean() | {false, term()}.
evalue_conditions(_FunctionName, {_ConditionsType, []}, 'andalso') ->
    true;
evalue_conditions(FunctionName, {_ConditionsType, []}, _EvalueMode) ->
    {false, {FunctionName, <<"Value is not matching any of the (0) given conditions">>}};
evalue_conditions(FunctionName, {ConditionsType, Conditions}, 'andalso') ->
    case internal_evalue_conditions(ConditionsType, Conditions, 'andalso') of
        true -> true;
        {false, {FailingFunction, Reason}, N} ->
            {false, {
                FunctionName, 
                list_to_binary(
                    lists:flatten(
                        io_lib:format(
                            "Value is not matching all conditions. Condition ~p failed because of field ~p : ~s", 
                            [N, FailingFunction, Reason]
                        )
                    )
                )
            }}
    end;
evalue_conditions(FunctionName, {ConditionsType, Conditions}, EvalueMode) ->
    case internal_evalue_conditions(ConditionsType, Conditions, EvalueMode) of
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
                    lists:flatten(
                        io_lib:format(
                            "Value is not matching exactly one condition. More than one (conditions ~p and ~p) matched",
                            [First, Second]
                        )
                    )
                )
            }}
    end.

internal_evalue_conditions(fun_call, [Fun | Rest], EvalueMode) ->
    next_evalue_condition(fun_call, Fun(), Rest, EvalueMode);
internal_evalue_conditions(mfa_call, [{Function, Args} | Rest], EvalueMode) ->
    next_evalue_condition(mfa_call, Function(Args), Rest, EvalueMode).

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
        {{false, {_Function, Reason}}, NewAcc} ->
            {false, NewAcc, Reason}
    end.

-spec find(Fun, List) -> Resp when
    Fun :: function(),
    List :: list(),
    Resp :: {true, term()} | {false, none}.
find(_Fun, []) -> 
    {false, none};
find(Fun, [H|T]) ->
    case Fun(H) of
        false -> find(Fun, T);
        true -> {true, H}
    end.

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
-spec next_evalue_condition(ConditionsType, CurrentResult, Conditions, EvalueMode) -> Resp when
    ConditionsType :: fun_call | mfa_call,
    CurrentResult :: true | {false, term()},
    Conditions :: [Condition],
    Condition :: {fun_call, fun()} | {mfa_call, {function(), term()}},
    EvalueMode :: 'orelse' | 'andalso' | 'xor' | {'xor', 0 | 1},
    Resp :: boolean() | {false, term()}.
next_evalue_condition(_ConditionsType, true, _Rest, 'orelse') ->
    true;
next_evalue_condition(_ConditionsType, _Result, [], 'orelse') ->
    false;
next_evalue_condition(ConditionsType, {false, _}, Rest, 'orelse') ->
    internal_evalue_conditions(ConditionsType, Rest, 'orelse');

next_evalue_condition(ConditionsType, Result, Rest, 'xor') ->
    next_evalue_condition(ConditionsType, Result, Rest, {'xor', 0, []});

next_evalue_condition(_ConditionsType, true, [], {'xor', _N, []}) ->
    true;
next_evalue_condition(_ConditionsType, _Result, [], {'xor', _N, []}) ->
    {false, none_matched};
next_evalue_condition(_ConditionsType, true, _Rest, {'xor', N, [One]}) ->
    {false, {many_matched, [One, N]}};
next_evalue_condition(_ConditionsType, _Result, [], {'xor', _N, [_One]}) ->
    true;
next_evalue_condition(ConditionsType, true, Rest, {'xor', N, []}) ->
    internal_evalue_conditions(ConditionsType, Rest, {'xor', N+1, [N]});
next_evalue_condition(ConditionsType, {false, _}, Rest, {'xor', N, []}) ->
    internal_evalue_conditions(ConditionsType, Rest, {'xor', N+1, []});
next_evalue_condition(ConditionsType, {false, _}, Rest, {'xor', N, [One]}) ->
    internal_evalue_conditions(ConditionsType, Rest, {'xor', N+1, [One]});

next_evalue_condition(_ConditionsType, Result, Conditions, 'andalso') ->
    next_evalue_condition(_ConditionsType, Result, Conditions, {'andalso', 0});
next_evalue_condition(_ConditionsType, true, [], {'andalso', _N}) ->
    true;
next_evalue_condition(_ConditionsType, {false, Reason}, [], {'andalso', N}) ->
    {false, Reason, N};
next_evalue_condition(ConditionsType, true, Rest, {'andalso', N}) ->
    internal_evalue_conditions(ConditionsType, Rest, {'andalso', N+1});
next_evalue_condition(_ConditionsType, {false, Reason}, _Rest, {'andalso', N}) ->
    {false, Reason, N}.
