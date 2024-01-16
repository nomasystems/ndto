%%% Copyright 2024 Nomasystems, S.L. http://www.nomasystems.com
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License
%%% NOTE: Module generated by ndto, do not modify this file.
-module(smthing).
-feature(maybe_expr, enable).
%%% EXTERNAL EXPORTS
-export([is_valid/1]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
is_valid(Val) -> '$.'(Val).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
'$.'(Val) ->
    maybe
        true ?= '$._object'(Val),
        true ?= '$._max_properties'(Val),
        true ?= '$._min_properties'(Val),
        true ?= '$._required'(Val),
        true ?= '$.*'(Val),
        true
    end.

'$._object'(Val) when is_map(Val) -> true;
'$._object'(Val) ->
    {false,
     {'$.',
      erlang:list_to_binary(io_lib:format("~p is not a object",
                                          [Val]))}}.

'$._max_properties'(Val) ->
    erlang:length(maps:keys(Val)) =< 2.

'$._min_properties'(Val) ->
    erlang:length(maps:keys(Val)) >= 0.

'$._required'(Val) ->
    case ndto_utils:find(fun (Property) ->
                                 not maps:is_key(Property, Val)
                         end,
                         [<<"$}N+l">>])
        of
        {false, none} -> true;
        {true, MissingProperty} ->
            {false,
             {'$.',
              erlang:list_to_binary(lists:flatten(io_lib:format("$. is missing required property ~s",
                                                                [MissingProperty])))}}
    end.

'$.*'(Val) ->
    maybe
        true ?= '$.$}N+lany'(maps:get(<<"$}N+l">>,
                                      Val,
                                      undefined)),
        true
    end.

'$.$}N+lany'(_Val) -> true.