# ndto
![ndto ci](https://github.com/nomasystems/ndto/actions/workflows/ci.yml/badge.svg)

`ndto` is an Erlang library for generating DTO (Data Transfer Object) validation modules from schemas.

## Setup

To use `ndto` in your project, just add it as a dependency in your `rebar.config` file:

```erl
{deps, [
    {ndto, {git, "https://github.com/nomasystems/ndto.git", {branch, "main"}}}
]}.
```

## Quickstart

1. Define a `ndto` schema.
```erl
Schema = #{
    <<"type">> => <<"string">>,
    <<"minLength">> => 8,
    <<"pattern">> => <<"^hello">>
}.
```

2. Generate a module using the `ndto:generate/2` function.
```erl
DTO = ndto:generate(string_schema, Schema).
```

3. Load the generated module on the fly.
```erl
ok = ndto:load(DTO).
```

4. Call the `is_valid/1` function from the generated module to validate your data.
```erl
true = string_schema:is_valid(<<"hello world">>).
false = string_schema:is_valid(<<"hello">>).
false = string_schema:is_valid(<<"hi world">>).
```
## Schema

`ndto` schemas are defined as follows:
```erl
-type schema() ::
    undefined
    | empty_schema()
    | universal_schema()
    | ref_schema()
    | boolean_schema()
    | enum_schema()
    | integer_schema()
    | number_schema()
    | string_schema()
    | array_schema()
    | object_schema()
    | union_schema()
    | intersection_schema()
    | complement_schema()
    | symmetric_difference_schema().

-type empty_schema() :: false.
-type universal_schema() :: true | #{} | union_schema().
-type ref_schema() :: #{<<"$ref">> := binary()}.
-type boolean_schema() :: #{<<"type">> := <<"boolean">>}.
-type enum_schema() :: #{<<"enum">> := [value()]}.
-type integer_schema() :: #{
    <<"type">> := <<"integer">>,
    <<"minimum">> => integer(),
    <<"exclusiveMinimum">> => boolean(),
    <<"maximum">> => integer(),
    <<"exclusiveMaximum">> => boolean(),
    <<"multipleOf">> => integer()
}.
-type number_schema() :: #{
    <<"type">> := <<"number">>,
    <<"minimum">> => number(),
    <<"exclusiveMinimum">> => boolean(),
    <<"maximum">> => number(),
    <<"exclusiveMaximum">> => boolean()
}.
-type string_schema() :: #{
    <<"type">> := <<"string">>,
    <<"minimum">> => integer(),
    <<"minLength">> => non_neg_integer(),
    <<"maxLength">> => non_neg_integer(),
    <<"format">> => format(),
    <<"pattern">> => pattern()
}.
-type array_schema() :: #{
    <<"type">> := <<"array">>,
    <<"items">> => schema(),
    <<"minItems">> => non_neg_integer(),
    <<"maxItems">> => non_neg_integer(),
    <<"uniqueItems">> => boolean()
}.
-type object_schema() :: #{
    <<"type">> := <<"object">>,
    <<"properties">> => #{binary() => schema()},
    <<"required">> => [binary()],
    <<"minProperties">> => non_neg_integer(),
    <<"maxProperties">> => non_neg_integer(),
    <<"patternProperties">> => #{pattern() => schema()},
    <<"additionalProperties">> => schema()
}.
-type union_schema() :: #{<<"anyOf">> := [schema()]}.
-type intersection_schema() :: #{<<"allOf">> := [schema()]}.
-type complement_schema() :: #{<<"not">> := schema()}.
-type symmetric_difference_schema() :: #{<<"oneOf">> := [schema()]}.

-type value() ::
    boolean()
    | integer()
    | float()
    | binary()
    | array()
    | object().
-type array() :: [value()].
-type object() :: #{binary() => value()}.
-type format() :: <<"iso8601">> | <<"base64">>.
-type pattern() :: binary().
```

## Contributing

We :heart: contributions! Please feel free to submit issues, create pull requests or just spread the word about `ndto` in the open-source community. Together, we can make this library even better! :muscle:

## Support

If you need help or have any questions, please don't hesitate to open an issue on the GitHub repository or contact the maintainers directly.

## License

`ndto` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.
