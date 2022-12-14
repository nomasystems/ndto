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
-module(ndto).

%%% INCLUDE FILES
-include("ndto.hrl").

%%% EXTERNAL EXPORTS
-export([
    generate/2,
    generate/3
]).

%%% TYPE EXPORTS
-export_type([ndto/0]).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec generate(Name, Schema) -> Result when
    Name :: binary(),
    Schema :: schema(),
    Result :: ndto().
generate(Name, Schema) ->
    generate(Name, Schema, openapi).

-spec generate(Name, Schema, Format) -> Result when
    Name :: binary(),
    Schema :: schema(),
    Format :: schema_format(),
    Result :: ndto().
generate(Name, Schema, Format) ->
    ndto_generator:generate(Name, Schema, Format).
