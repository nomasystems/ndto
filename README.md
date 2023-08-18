# ndto
[![ndto ci](https://github.com/nomasystems/ndto/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/ndto/actions/workflows/ci.yml)
[![ndto docs](https://github.com/nomasystems/ndto/actions/workflows/docs.yml/badge.svg)](https://nomasystems.github.io/ndto)

`ndto` is an Erlang library for generating DTO (Data Transfer Object) validation modules from schemas.

## Motivation

Validating incoming data is a critical step to ensure the integrity, consistency, and security of your application's data flow. However, writing custom validation logic for each DTO can quickly become a time-consuming and error-prone task.

With `ndto`, you can define validation schemas that describe the structure and constraints of your data. These schemas are then used to automatically generate validation modules, reducing development time and avoiding human-induced errors in the validation step.

## Quickstart

1. Add `ndto` as a dependency in your `rebar.config` file:
```erl
{deps, [
    {ndto, {git, "https://github.com/nomasystems/ndto.git", {branch, "main"}}}
]}.
```

2. Define an `ndto` schema.
```erl
Schema = #{
    <<"type">> => <<"string">>,
    <<"minLength">> => 8,
    <<"pattern">> => <<"^hello">>
}.
```

3. Generate a module using the `ndto:generate/2` function.
```erl
DTO = ndto:generate(string_schema, Schema).
```

4. Load the generated module on the fly.
```erl
ok = ndto:load(DTO).
```

5. Call the `is_valid/1` function from the generated module to validate your data.
```erl
true = string_schema:is_valid(<<"hello world">>).
false = string_schema:is_valid(<<"hello">>).
false = string_schema:is_valid(<<"hi world">>).
```

## `ndto` schema language

Schemas are built according to the `ndto:schema()` type.
```erl
%%% ndto.erl
-type schema() ::
    empty_schema()
    | universal_schema()
    | ref_schema()
    | enum_schema()
    | boolean_schema()
    | integer_schema()
    | number_schema()
    | string_schema()
    | array_schema()
    | object_schema()
    | union_schema()
    | intersection_schema()
    | complement_schema()
    | symmetric_difference_schema().
```

Check the [docs](https://nomasystems.github.io/ndto/ndto.html#types) for an up-to-date version of the type specifications.

## Contributing

We :heart: contributions! Please feel free to submit issues, create pull requests or just spread the word about `ndto` in the open-source community. Don't forget to check out our [contribution guidelines](CONTRIBUTING.md) to ensure smooth collaboration! :rocket:

## Support

If you need help or have any questions, please don't hesitate to open an issue or contact the maintainers directly.

## License

`ndto` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.
> This project uses OpenAPI specification (OAS) schemas and examples, which are licensed under the Apache 2.0 license. See the associated [LICENSE](priv/oas/LICENSE) file for more information.
