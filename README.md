# swagger-test

This is a tool for
[Property Based Testing](https://en.wikipedia.org/wiki/Property_testing)
of [swagger](https://swagger.io) APIs.

It basically allow you to approximate the computation of the following
proposition:

> valid(response, schema), response = execute(request), ∀ request ∈ schema

Which translates to:

> For all valid requests that can be derived from my Swagger schema, the
> API response obtained from executing that request is valid according to the
> same Swagger schema.

The tool exposes several ways to configure get value from parts of it, for
example, you may be interested in getting just a random valid request from the
schema (use the `generate` command), or validating if a given response (from a
particular operation) is valid (use the `validate` command), or, run one sample
instance of the full proposition, which picks a random request and validate it's
resulting response (use the `request` command).

The generator random request values are reproducible by re-using the same _seed_
value, or one can focus on a particular operation by specifying the operation
id.

The tool also simplifies integration with other systems by allowing to configure
output formats as standard HTTP message, JSON, or curl. Additionally, there
are Haskell modules exposed as a library if one wants to build on top of it.

## Command Line Interface

*swagger-test* supports three commands:

 * `generate` - generates a new random valid request from Swagger schema.
 * `validate` - validate a response to a given operation id, according to the
   schema.
 * `request` - generate and make the request, then validates the response
   (combines generate and validate).

```console
swagger-test --help
```

```
Testing tool for Swagger APIs

Usage: swagger-test [-s|--schema FILENAME] COMMAND
  Generate Swagger requests and validates responses

Available options:
  -s,--schema FILENAME     swagger JSON schema file to read
                           from (default: "swagger.json")
  -h,--help                Show this help text

Available commands:
  generate                 Generate a random request according to Schema
  validate                 Validate a response against Schema
  request                  Generate, make the request, and validate response
```

### Sub-commands

#### Generate

```
Usage: swagger-test generate [--seed N] [-o|--operation ID]
                             [--request-format http|curl|none|json] [-i|--info]
                             [--size N]
  Generate a request

Available options:
  --seed N                 specify the seed for the random generator
  -o,--operation ID        specify a operation id to test (default pick
                           randomly)
  --request-format http|curl|none|json
                           output format of the HTTP request (default: http)
  -i,--info                render information about seed and operation id
  --size N                 control the size of the generated
                           request (default: 30)
  -h,--help                Show this help text
```

#### Validate

```
Usage: swagger-test validate [FILENAME] (-o|--operation ID)
  Validate a response

Available options:
  FILENAME                 http response file to read from (default=stdin)
  -o,--operation ID        specify a operation id to test (default pick
                           randomly)
  -h,--help                Show this help text
```

#### Request

```
Usage: swagger-test request [--seed N] [-o|--operation ID]
                            [--request-format http|curl|none|json]
                            [--response-format http|json|none] [-i|--info]
                            [--size N]
  Generate, make the request, and validate response

Available options:
  --seed N                 specify the seed for the random generator
  -o,--operation ID        specify a operation id to test (default pick
                           randomly)
  --request-format http|curl|none|json
                           output format of the HTTP request (default: none)
  --response-format http|json|none
                           output format of the HTTP request (default: none)
  -i,--info                render information about seed and operation id
  --size N                 control the size of the generated
                           request (default: 30)
  -h,--help                Show this help text
```
