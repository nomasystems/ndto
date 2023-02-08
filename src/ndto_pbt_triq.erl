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
-module(ndto_pbt_triq).

%%% INCLUDE FILES
-include("ndto.hrl").
-include("ndto_pbt.hrl").

%%% EXTERNAL EXPORTS
-export([
    dto/1
]).

%%% MACROS
%% https://www.erlang.org/doc/efficiency_guide/advanced.html
-define(MAX_INT, 134217728).

%%%-----------------------------------------------------------------------------
%%% EXTERNAL EXPORTS
%%%-----------------------------------------------------------------------------
-spec dto(Schema) -> TestDataGenerator when
    Schema :: schema(),
    TestDataGenerator :: test_data_generator().
dto(#{<<"enum">> := _Enum} = Schema) ->
    enum(Schema);
dto(#{<<"type">> := <<"allOf">>} = Schema) ->
    all_of(Schema);
dto(#{<<"type">> := <<"anyOf">>} = Schema) ->
    any_of(Schema);
dto(#{<<"type">> := <<"array">>} = Schema) ->
    array(Schema);
dto(#{<<"type">> := <<"boolean">>} = Schema) ->
    boolean(Schema);
dto(#{<<"type">> := <<"integer">>} = Schema) ->
    integer(Schema);
dto(#{<<"type">> := <<"not">>} = Schema) ->
    'not'(Schema);
dto(#{<<"type">> := <<"number">>} = Schema) ->
    number(Schema);
dto(#{<<"type">> := <<"object">>} = Schema) ->
    object(Schema);
dto(#{<<"type">> := <<"oneOf">>} = Schema) ->
    one_of(Schema);
dto(#{<<"type">> := <<"string">>} = Schema) ->
    string(Schema);
dto(_Schema) ->
    any().

%%%-----------------------------------------------------------------------------
%%% GENERATORS
%%%-----------------------------------------------------------------------------
-spec all_of(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
all_of(_Schema) ->
    erlang:throw(not_implemented).

-spec any() -> Dom when
    Dom :: test_data_generator().
any() ->
    erlang:throw(not_implemented).

-spec any_of(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
any_of(_Schema) ->
    erlang:throw(not_implemented).

-spec array(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
array(_Schema) ->
    erlang:throw(not_implemented).

-spec boolean(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
boolean(_Schema) ->
    triq_dom:bool().

-spec enum(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
enum(#{<<"enum">> := Enum}) ->
    triq_dom:elements(Enum).

-spec integer(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
integer(_Schema) ->
    erlang:throw(not_implemented).

-spec 'not'(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
'not'(_Schema) ->
    erlang:throw(not_implemented).

-spec number(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
number(_Schema) ->
    erlang:throw(not_implemented).

-spec object(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
object(_Schema) ->
    erlang:throw(not_implemented).

-spec one_of(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
one_of(_Schema) ->
    erlang:throw(not_implemented).

-spec string(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
string(#{<<"pattern">> := _Pattern}) ->
    erlang:throw(not_implemented);
string(Schema) ->
    MinLength = maps:get(<<"minLength">>, Schema, 0),
    MaxLength = maps:get(<<"maxLength">>, Schema, ?MAX_INT),
    Format = maps:get(<<"format">>, Schema, undefined),
    triq_dom:bind(
        triq_dom:int(MinLength, MaxLength),
        fun(Length) ->
            string_format(Format, Length)
        end
    ).

-spec string_format(Format, Length) -> FormatGenerator when
    Format :: undefined | binary(),
    Length :: non_neg_integer(),
    FormatGenerator :: test_data_generator().
string_format(undefined, Length) ->
    triq_dom:unicode_binary(Length);
string_format(<<"base64">>, Length) ->
    triq_dom:bind(
        triq_dom:unicode_binary(Length),
        fun(Unicode) ->
            base64:encode(Unicode)
        end
    );
string_format(<<"iso8601-datetime">>, _Length) ->
    triq_dom:bind(
        {
            triq_dom:int(9999),
            triq_dom:int(1, 12),
            triq_dom:int(23),
            triq_dom:int(59),
            triq_dom:int(59),
            triq_dom:elements(timezones())
        },
        fun({Year, Month, Hour, Min, Second, Timezone}) ->
            MaxDay =
                case Month of
                    2 ->
                        case is_leap(Year) of
                            true ->
                                29;
                            _false ->
                                28
                        end;
                    Thirty when
                        Thirty =:= 4 orelse
                            Thirty =:= 6 orelse
                            Thirty =:= 9 orelse
                            Thirty =:= 11
                    ->
                        30;
                    _Otherwise ->
                        31
                end,
            triq_dom:bind(
                triq_dom:int(MaxDay),
                fun(Day) ->
                    unicode:characters_to_binary(
                        io_lib:format("~4..0B~2..0B~2..0BT~2..0B~2..0B~2..0B~s", [
                            Year, Month, Day, Hour, Min, Second, Timezone
                        ])
                    )
                end
            )
        end
    ).

%%%-----------------------------------------------------------------------------
%%% INTERNAL FUNCTIONS
%%%-----------------------------------------------------------------------------
is_leap(Year) when
    Year > 0, (Year rem 4) =:= 0, ((Year rem 100) =:= 0 orelse (Year rem 400) =/= 0)
->
    true;
is_leap(_Year) ->
    false.

timezones() ->
    [
        "Z",
        "-1200",
        "-1100",
        "-1000",
        "-0930",
        "-0900",
        "-0800",
        "-0700",
        "-0600",
        "-0500",
        "-0430",
        "-0400",
        "-0330",
        "-0300",
        "-0230",
        "-0200",
        "-0100",
        "+0000",
        "+0100",
        "+0200",
        "+0300",
        "+0330",
        "+0400",
        "+0430",
        "+0500",
        "+0530",
        "+0545",
        "+0600",
        "+0630",
        "+0700",
        "+0730",
        "+0800",
        "+0900",
        "+0930",
        "+1000",
        "+1030",
        "+1100",
        "+1130",
        "+1200",
        "+1245",
        "+1300",
        "+1345",
        "+1400"
    ].
