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
-module(ndto_dom).

%%% INCLUDE FILES
-include_lib("triq/include/triq.hrl").

%%% VALUE EXPORTS
-export([
    any_value/0,
    undefined_value/0,
    string_value/0,
    number_value/0,
    integer_value/0,
    boolean_value/0,
    array_value/0,
    array_value/1,
    object_value/0,
    object_value/1
]).

%%% UTIL EXPORTS
-export([types/0]).

%%% MACROS
-define(NON_RECURSIVE_TYPES, lists:subtract(types(), [<<"array">>, <<"object">>])).

%%%-----------------------------------------------------------------------------
%%% VALUE EXPORTS
%%%-----------------------------------------------------------------------------
any_value() ->
    triq_dom:oneof([
        undefined_value(),
        string_value(),
        number_value(),
        integer_value(),
        boolean_value(),
        array_value(),
        object_value()
    ]).

undefined_value() ->
    triq_dom:return(undefined).

string_value() ->
    triq_dom:unicode_binary().

number_value() ->
    triq_dom:oneof([triq_dom:int(), triq_dom:float()]).

integer_value() ->
    triq_dom:int().

boolean_value() ->
    triq_dom:bool().

array_value() ->
    ?LET(Type, triq_dom:elements(types()), array_value(Type)).

array_value(<<"array">>) ->
    triq_dom:list(
        ?LET(Type, triq_dom:elements(?NON_RECURSIVE_TYPES), array_value(Type))
    );
array_value(<<"object">>) ->
    triq_dom:list(
        ?LET(Type, triq_dom:elements(?NON_RECURSIVE_TYPES), object_value(Type))
    );
array_value(Type) ->
    Fun = erlang:binary_to_atom(<<Type/binary, "_value">>),
    triq_dom:list(erlang:apply(?MODULE, Fun, [])).

object_value() ->
    ?LET(Type, triq_dom:elements(types()), object_value(Type)).

object_value(<<"array">>) ->
    ?LET(
        Proplist,
        ?LET(
            Type,
            triq_dom:elements(?NON_RECURSIVE_TYPES),
            triq_dom:list({triq_dom:unicode_binary(5), array_value(Type)})
        ),
        maps:from_list(Proplist)
    );
object_value(<<"object">>) ->
    ?LET(
        Proplist,
        ?LET(
            Type,
            triq_dom:elements(?NON_RECURSIVE_TYPES),
            triq_dom:list({triq_dom:unicode_binary(5), object_value(Type)})
        ),
        maps:from_list(Proplist)
    );
object_value(Type) ->
    Fun = erlang:binary_to_atom(<<Type/binary, "_value">>),
    ?LET(
        Proplist,
        triq_dom:list({
            triq_dom:unicode_binary(5), erlang:apply(?MODULE, Fun, [])
        }),
        maps:from_list(Proplist)
    ).

%%%-----------------------------------------------------------------------------
%%% UTIL EXPORTS
%%%-----------------------------------------------------------------------------
types() ->
    [<<"string">>, <<"number">>, <<"integer">>, <<"boolean">>, <<"array">>, <<"object">>].
