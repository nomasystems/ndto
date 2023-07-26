# ndto
[![ndto ci](https://github.com/nomasystems/ndto/actions/workflows/ci.yml/badge.svg)](https://github.com/nomasystems/ndto/actions/workflows/ci.yml)
[![ndto docs](https://github.com/nomasystems/ndto/actions/workflows/docs.yml/badge.svg)](https://nomasystems.github.io/ndto)

`ndto` is an Erlang library for generating DTO (Data Transfer Object) validation modules from schemas.

## Setup

To use `ndto` in your project, just add it as a dependency in your `rebar.config` file:

```erl
{deps, [
    {ndto, {git, "https://github.com/nomasystems/ndto.git", {branch, "main"}}}
]}.
```

## Quickstart

1. Define an `ndto` schema.
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

## Contributing

We :heart: contributions! Please feel free to submit issues, create pull requests or just spread the word about `ndto` in the open-source community. Don't forget to check out our [contribution guidelines](CONTRIBUTING) to ensure smooth collaboration! :rocket:

## Support

If you need help or have any questions, please don't hesitate to open an issue or contact the maintainers directly.

## License

`ndto` is released under the Apache 2.0 License. For more information, please see the [LICENSE](LICENSE) file.
> This project uses OpenAPI specification (OAS) schemas and examples, which are licensed under the Apache 2.0 license. See the associated [LICENSE](priv/oas/LICENSE) file for more information.
