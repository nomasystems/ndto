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
%% limitations under the License.
-module(ndto_pbt_SUITE).

%%% EXTERNAL EXPORTS
-compile([export_all, nowarn_export_all]).

%%%-----------------------------------------------------------------------------
%%% SUITE EXPORTS
%%%-----------------------------------------------------------------------------
all() ->
    [
        enum,
        boolean,
        {group, string}
    ].

groups() ->
    [
        {string, [parallel], [
            string,
            string_base64,
            'string_iso8601-datetime'
        ]}
    ].

%%%-----------------------------------------------------------------------------
%%% INIT SUITE EXPORTS
%%%-----------------------------------------------------------------------------
init_per_suite(Conf) ->
    Config = nct_util:setup_suite(Conf),
    ct_property_test:init_per_suite(Config).

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
enum(Conf) ->
    ct_property_test:quickcheck(
        ndto_pbt_properties:prop_enum(),
        Conf
    ).

boolean(Conf) ->
    ct_property_test:quickcheck(
        ndto_pbt_properties:prop_boolean(),
        Conf
    ).

string(Conf) ->
    ct_property_test:quickcheck(
        ndto_pbt_properties:prop_string(),
        Conf
    ).

string_base64(Conf) ->
    ct_property_test:quickcheck(
        ndto_pbt_properties:prop_string_base64(),
        Conf
    ).

'string_iso8601-datetime'(Conf) ->
    ct_property_test:quickcheck(
        ndto_pbt_properties:'prop_string_iso8601-datetime'(),
        Conf
    ).
