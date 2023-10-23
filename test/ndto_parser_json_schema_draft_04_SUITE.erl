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
-module(ndto_parser_json_schema_draft_04_SUITE).

%%% INCLUDE FILES
-include_lib("stdlib/include/assert.hrl").

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        oas_3_0
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    nct_util:setup_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% END SUITE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_suite(Conf) ->
    nct_util:teardown_suite(Conf).

%%%-----------------------------------------------------------------------------
%%% INIT CASE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_testcase(Case, Conf) ->
    ct:print("Starting test case ~p", [Case]),
    nct_util:init_traces(Case),
    Conf.

%%%-----------------------------------------------------------------------------
%%% END CASE EXPORTS
%%%-----------------------------------------------------------------------------
end_per_testcase(Case, Conf) ->
    nct_util:end_traces(Case),
    ct:print("Test case ~p completed", [Case]),
    Conf.

%%%-----------------------------------------------------------------------------
%%% TEST CASES
%%%-----------------------------------------------------------------------------
oas_3_0(_Conf) ->
    SpecPath = filename:join(code:lib_dir(ndto, priv), "oas/3.0/specs/oas_3_0.json"),
    {ok, Schemas} = ndto_parser:parse(ndto_parser_json_schema_draft_04, oas_3_0, SpecPath),
    #{
        type := object,
        properties := #{
            <<"openapi">> := #{
                type := string,
                pattern := Pattern
            }
        }
    } = proplists:get_value(oas_3_0, Schemas),
    ?assertEqual(<<"^3\\.0\\.\\d(-.+)?$">>, Pattern),
    #{
        type := object,
        properties := #{
            <<"not">> := #{
                one_of := SchemaOneOf
            }
        }
    } = proplists:get_value(oas_3_0_Schema, Schemas),
    Expected = lists:sort([
        #{ref => <<"oas_3_0_Reference">>}, #{ref => <<"oas_3_0_Schema">>}
    ]),
    Actual = lists:sort(SchemaOneOf),
    ?assertEqual(Expected, Actual).
