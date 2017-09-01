# swagger-test

This is a package that helps you to test your [swagger](https://swagger.io) API.

It exposes libraries and a handy executable to generate random valid request to
your API according to the specification. Sort of "monkey test" for APIs.

It can be easily integrated into other tools or testing systems by using the
command line interface.

## Command Line Tool

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
