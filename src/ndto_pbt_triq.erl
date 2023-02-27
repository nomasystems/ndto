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
-define(MIN_INT, -?MAX_INT).

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
array(Schema) ->
    Items = maps:get(<<"items">>, Schema, #{}),
    MinItems = maps:get(<<"minItems">>, Schema, 0),
    MaxItems = maps:get(<<"maxItems">>, Schema, 250),
    UniqueItems = maps:get(<<"uniqueItems">>, Schema, false),
    triq_dom:bind(
        triq_dom:int(MinItems, MaxItems),
        fun(Length) ->
            DTO = dto(Items),
            Array = triq_dom:vector(Length, DTO),
            case UniqueItems of
                false ->
                    Array;
                true ->
                    triq_dom:suchthat(
                        triq_dom:bind(
                            Array,
                            fun lists:uniq/1
                        ),
                        fun(A) ->
                            erlang:length(A) >= MinItems
                        end
                    )
            end
        end
    ).

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
integer(Schema) ->
    RawMin = maps:get(<<"minimum">>, Schema, ?MIN_INT),
    ExclusiveMin = maps:get(<<"exclusiveMinimum">>, Schema, false),
    Min =
        case ExclusiveMin of
            true ->
                RawMin + 1;
            false ->
                RawMin
        end,
    RawMax = maps:get(<<"maximum">>, Schema, ?MAX_INT),
    ExclusiveMax = maps:get(<<"exclusiveMaximum">>, Schema, false),
    Max =
        case ExclusiveMax of
            true ->
                RawMax - 1;
            false ->
                RawMax
        end,
    MultipleOf = maps:get(<<"multipleOf">>, Schema, undefined),
    case MultipleOf of
        undefined ->
            triq_dom:int(Min, Max);
        _Otherwise ->
            Multiples = multiples(MultipleOf, Min, Max),
            triq_dom:elements(Multiples)
    end.

-spec 'not'(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
'not'(_Schema) ->
    erlang:throw(not_implemented).

-spec number(Schema) -> Dom when
    Schema :: schema(),
    Dom :: test_data_generator().
number(Schema) ->
    Min = maps:get(<<"minimum">>, Schema, ?MIN_INT),
    ExclusiveMin = maps:get(<<"exclusiveMinimum">>, Schema, false),
    Max = maps:get(<<"maximum">>, Schema, ?MAX_INT),
    ExclusiveMax = maps:get(<<"exclusiveMaximum">>, Schema, false),
    Integer = triq_dom:int(erlang:trunc(Min), erlang:trunc(Max)),
    Float = triq_dom:bind(
        triq_dom:int(?MAX_INT),
        fun(Int) ->
            Min + (Max - Min) * (Int / ?MAX_INT)
        end
    ),
    RawNumber = triq_dom:oneof([Integer, Float]),
    RawNumberMin =
        case ExclusiveMin of
            true ->
                triq_dom:suchthat(RawNumber, fun(Number) -> Number > Min end);
            false ->
                RawNumber
        end,
    case ExclusiveMax of
        true ->
            triq_dom:suchthat(RawNumber, fun(Number) -> Number < Max end);
        false ->
            RawNumberMin
    end.

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
    MaxLength = maps:get(<<"maxLength">>, Schema, 255),
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
    triq_dom:vector(
        Length,
        triq_dom:unicode_char()
    );
string_format(<<"base64">>, Length) ->
    0 = (Length rem 4),
    triq_dom:vector(
        Length,
        triq_dom:elements(base64_chars())
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
                        case calendar:is_leap_year(Year) of
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
                triq_dom:int(1, MaxDay),
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
base64_chars() ->
    lists:append(
        [
            [43, 47, 61],
            lists:seq(48, 57),
            lists:seq(65, 90),
            lists:seq(97, 122)
        ]
    ).

multiples(MultipleOf, Min, Max) when MultipleOf =< 0 ->
    multiples(MultipleOf * -1, Min, Max);
multiples(MultipleOf, Min, Max) ->
    FirstMultiple = MultipleOf * ((Min + MultipleOf - 1) div MultipleOf),
    multiples(MultipleOf, Max, FirstMultiple, []).

multiples(_MultipleOf, Max, Current, Acc) when Current > Max ->
    Acc;
multiples(MultipleOf, Max, Current, Acc) ->
    multiples(MultipleOf, Max, Current + MultipleOf, [Current | Acc]).

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
