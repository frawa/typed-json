# Typed Json

[![CircleCI](https://circleci.com/gh/frawa/typed-json/tree/develop.svg?style=svg)](https://circleci.com/gh/frawa/typed-json/tree/develop)

[![typed-json Scala version support](https://index.scala-lang.org/frawa/typed-json/typed-json/latest-by-scala-version.svg)](https://index.scala-lang.org/frawa/typed-json/typed-json)

Validating Json against Json Schema in Scala.


The purpose of this library is twofold:

- Propose a Scala solution to validate Json data against schemas.
- Assist in editing Json data with errors and suggestions, try the [sample web editor](https://frawa.github.io/typed-json).

The library is designed to integrate with any Json parser, example integrations exist for

- [typelevel/jawn](https://github.com/typelevel/jawn)
- [zio-json](https://zio.github.io/zio-json)

Validation will be compliant to specs as covered by [json-schema-org/JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite). For now it supports only `draft-2020-12` with some optional features.