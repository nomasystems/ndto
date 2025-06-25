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

%% @private
-module(ndto_generator_complement).

%%% BEHAVIOR
-behaviour(ndto_generator).

%%% EXTERNAL EXPORTS
-export([is_valid/2]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
is_valid(Prefix, #{'not' := Subschema} = Schema) ->
    FunName = Prefix,
    {IsValidFun, ExtraFuns} = ndto_generator:is_valid(<<FunName/binary, ".not">>, Subschema),
    OptionalClause = ndto_generator:optional_clause(Schema),
    NullClause = ndto_generator:null_clause(Schema),
    TrueClause = erl_syntax:clause(
        [erl_syntax:variable('Val')],
        none,
        [
            erl_syntax:case_expr(
                erl_syntax:application(
                    erl_syntax:function_name(IsValidFun),
                    [erl_syntax:variable('Val')]
                ),
                [
                    erl_syntax:clause(
                        [erl_syntax:atom('true')],
                        none,
                        [erl_syntax:atom('false')]
                    ),
                    erl_syntax:clause(
                        [
                            erl_syntax:tuple([
                                erl_syntax:atom('false'),
                                erl_syntax:variable('_')
                            ])
                        ],
                        none,
                        [erl_syntax:atom('true')]
                    )
                ]
            )
        ]
    ),
    Clauses = ndto_generator:clauses([OptionalClause, NullClause, TrueClause]),
    Fun = erl_syntax:function(
        erl_syntax:atom(erlang:binary_to_atom(FunName)),
        Clauses
    ),
    {Fun, [IsValidFun | ExtraFuns]}.
