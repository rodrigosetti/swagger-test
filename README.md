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
 * `report` - high-level command that run several requests concurrently, and
   write HTML reports for multiple Swagger schemas. Useful for testing systems
   with many Swagger services in a CI server.

```console
swagger-test --help
```

```
Property-based testing tool for Swagger APIs

Usage: swagger-test COMMAND
  Execute one of the commands available depending on your needs

Available options:
  -h,--help                Show this help text

Available commands:
  generate                 Generate a random request according to Schema
  validate                 Validate a response against Schema
  request                  Generate, make the request, and validate response
  report                   Run several tests and generate reports

Run `COMMAND --help` to get command specific options help
```

### Sub-commands

#### Generate

```console
swagger-test generate --help
```

```
Usage: swagger-test generate [-s|--schema FILENAME] [--seed N]
                             [-o|--operation ID]
                             [--request-format http|curl|none|json]
                             [-i|--info] [--size N]
  Generate a random request according to Schema

Available options:
  -s,--schema FILENAME     swagger JSON schema file to read
                           from (default: "swagger.json")
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

```console
swagger-test validate --help
```

```
Usage: swagger-test validate [-s|--schema FILENAME] [FILENAME]
                             (-o|--operation ID)
  Validate a response against Schema

Available options:
  -s,--schema FILENAME     swagger JSON schema file to read
                           from (default: "swagger.json")
  FILENAME                 http response file to read from (default=stdin)
  -o,--operation ID        specify a operation id to test (default pick
                           randomly)
  -h,--help                Show this help text
```

#### Request

```console
swagger-test request --help
```

```
Usage: swagger-test request [-s|--schema FILENAME] [--seed N]
                            [-o|--operation ID]
                            [--request-format http|curl|none|json]
                            [--response-format http|json|none] [-i|--info]
                            [--size N]
  Generate, make the request, and validate response

Available options:
  -s,--schema FILENAME     swagger JSON schema file to read
                           from (default: "swagger.json")
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

#### Report

```console
swagger-test report --help
```

```
Usage: swagger-test report [--schemas PATH] [--reports PATH] [--tests N]
                           [--size N]
  Run several tests and generate reports

Available options:
  --schemas PATH           path to folder with swagger
                           schemas (default: "schemas")
  --reports PATH           path to folder to write the HTML
                           reports (default: "reports")
  --tests N                number of tests to run (default: 100)
  --size N                 control the size of the generated
                           request (default: 30)
  -h,--help                Show this help text
```
