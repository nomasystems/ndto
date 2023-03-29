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
-ifndef(ndto_schema).
-define(ndto_schema, true).

%%% MACROS
-define(BASIC_SCHEMAS, [
    #{<<"type">> => <<"boolean">>},
    #{<<"type">> => <<"integer">>},
    #{<<"type">> => <<"number">>},
    #{<<"type">> => <<"string">>},
    #{<<"type">> => <<"array">>},
    #{<<"type">> => <<"object">>}
]).
-define(FORMATS, [
    <<"base64">>,
    <<"iso8601-datetime">>
]).
% https://www.erlang.org/doc/efficiency_guide/advanced.html
-define(MAX_INT, 134217728).
-define(MIN_INT, -134217729).

% -ifndef(ndto_schema)
-endif.
