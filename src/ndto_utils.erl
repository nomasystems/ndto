-module(ndto_utils).

%%% EXTERNAL EXPORTS
-export([
    evalue_conditions/2
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec evalue_conditions(Conditions, EvalueMode) -> Resp when
    Conditions :: {mfa_call, [] | [MfaCall]} | {fun_call, [] | [FunCall]},
    MfaCall :: {function(), term()},
    FunCall :: function(),
    EvalueMode :: 'orelse' | 'andalso' | 'xor',
    Resp :: boolean() | {false, term()}.
evalue_conditions({_ConditionsType, []}, 'andalso') ->
    true;
evalue_conditions({_ConditionsType, []}, _EvalueMode) ->
    false;
evalue_conditions({ConditionsType, Conditions}, EvalueMode) ->
    evalue_conditions(ConditionsType, Conditions, EvalueMode).

evalue_conditions(fun_call, [Fun | Rest], EvalueMode) ->
    next_evalue_condition(fun_call, Fun(), Rest, EvalueMode);
evalue_conditions(mfa_call, [{Function, Args} | Rest], EvalueMode) ->
    next_evalue_condition(mfa_call, Function(Args), Rest, EvalueMode).

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
    evalue_conditions(ConditionsType, Rest, 'orelse');
next_evalue_condition(ConditionsType, Result, Rest, 'xor') ->
    next_evalue_condition(ConditionsType, Result, Rest, {'xor', 0});
next_evalue_condition(_ConditionsType, true, [], {'xor', 0}) ->
    true;
next_evalue_condition(_ConditionsType, _Result, [], {'xor', 0}) ->
    {false, none_matched};
next_evalue_condition(_ConditionsType, true, _Rest, {'xor', 1}) ->
    {false, many_matched};
next_evalue_condition(_ConditionsType, _Result, [], {'xor', 1}) ->
    true;
next_evalue_condition(ConditionsType, true, Rest, {'xor', 0}) ->
    evalue_conditions(ConditionsType, Rest, {'xor', 1});
next_evalue_condition(ConditionsType, {false, _}, Rest, {'xor', 0}) ->
    evalue_conditions(ConditionsType, Rest, {'xor', 0});
next_evalue_condition(ConditionsType, {false, _}, Rest, {'xor', 1}) ->
    evalue_conditions(ConditionsType, Rest, {'xor', 1});
next_evalue_condition(_ConditionsType, Result, [], 'andalso') ->
    Result;
next_evalue_condition(ConditionsType, true, Rest, 'andalso') ->
    evalue_conditions(ConditionsType, Rest, 'andalso');
next_evalue_condition(_ConditionsType, {false, Reason}, _Rest, 'andalso') ->
    {false, Reason}.
