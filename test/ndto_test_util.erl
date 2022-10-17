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
-module(ndto_test_util).

%%% EXTERNAL EXPORTS
-export([
    compile/2,
    is_valid/3
]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
compile(Name, Schema) ->
    {tree, form_list, Attr, CommentedForms} = ndto:generate(Name, Schema),
    Forms = lists:filter(
        fun
            ({tree, comment, _Attr, _Comment}) -> false;
            (_) -> true
        end,
        CommentedForms
    ),
    NDto = {tree, form_list, Attr, Forms},
    {ok, _Bin} = merl:compile_and_load(NDto).

is_valid(Name, Schema, Object) ->
    compile(Name, Schema),
    (erlang:binary_to_atom(Name)):is_valid(Object).
