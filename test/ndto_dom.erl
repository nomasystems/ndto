%%% Copyright 2022 Nomasystems, S.L. http://www.nomasystems.com
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

%%% SCHEMAS EXPORTS
-export([
    any_value/0,
    string_value/0,
    number_value/0,
    integer_value/0,
    boolean_value/0,
    array_value/1,
    object_value/0
]).

%%% UTIL EXPORTS
-export([
    types/0
]).

%%%-----------------------------------------------------------------------------
%%% SCHEMAS EXPORTS
%%%-----------------------------------------------------------------------------
any_value() ->
    triq_dom:oneof([
        string_value(),
        number_value(),
        integer_value(),
        boolean_value(),
        array_value(),
        object_value()
    ]).

string_value() ->
    triq_dom:unicode_binary().

number_value() ->
    triq_dom:oneof([triq_dom:int(), triq_dom:float()]).

integer_value() ->
    triq_dom:int().

boolean_value() ->
    triq_dom:bool().

array_value() ->
    triq_dom:list(
        ?LET(Type, triq_dom:elements(types()), array_value(Type))
    ).

array_value(Type) ->
    triq_dom:list(
        non_recursive_type(Type)
    ).

object_value() ->
    ?LET(
        Proplist,
        triq_dom:list({
            triq_dom:unicode_binary(),
            ?LET(Type, triq_dom:elements(types()), non_recursive_type(Type))
        }),
        maps:from_list(Proplist)
    ).

types() ->
    [<<"string">>, <<"number">>, <<"integer">>, <<"boolean">>, <<"array">>, <<"object">>].

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
non_recursive_type(<<"string">>) ->
    string_value();
non_recursive_type(<<"number">>) ->
    number_value();
non_recursive_type(<<"integer">>) ->
    integer_value();
non_recursive_type(<<"boolean">>) ->
    boolean_value();
non_recursive_type(<<"array">>) ->
    triq_dom:return([<<"string">>, 1.0, 2, #{<<"key">> => <<"value">>}]);
non_recursive_type(<<"object">>) ->
    triq_dom:return(#{<<"key">> => <<"value">>}).
