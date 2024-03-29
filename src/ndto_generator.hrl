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
-ifndef(ndto_generator).
-define(ndto_generator, true).

%%% MACROS
-define(CLINE, "%%-----------------------------------------------------------------------------").
-define(COPYRIGHT, [
    lists:append(["%% Copyright ", ?YEAR, " Nomasystems, S.L. http://www.nomasystems.com"]),
    "%",
    "% Licensed under the Apache License, Version 2.0 (the \"License\");",
    "% you may not use this file except in compliance with the License.",
    "% You may obtain a copy of the License at",
    "%",
    "%     http://www.apache.org/licenses/LICENSE-2.0",
    "%",
    "%  Unless required by applicable law or agreed to in writing, software",
    "%  distributed under the License is distributed on an \"AS IS\" BASIS,",
    "%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.",
    "% See the License for the specific language governing permissions and",
    "% limitations under the License"
]).
-define(EXPORTS_HEADER, "%% EXTERNAL EXPORTS").
-define(INTERNAL_HEADER, "%% INTERNAL FUNCTIONS").
-define(NOTE, "%% NOTE: Module generated by ndto, do not modify this file.").
-define(YEAR, erlang:integer_to_list(erlang:element(1, erlang:element(1, erlang:localtime())))).

% -ifndef(ndto_generator)
-endif.
