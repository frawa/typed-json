# Typed Json

![ci](https://github.com/frawa/typed-json/actions/workflows/ci.yml/badge.svg)

[![typed-json Scala version support](https://index.scala-lang.org/frawa/typed-json/typed-json/latest.svg)](https://index.scala-lang.org/frawa/typed-json/typed-json)
[![typed-json Scala version support](https://index.scala-lang.org/frawa/typed-json/typed-json/latest-by-scala-version.svg?platform=sjs1)](https://index.scala-lang.org/frawa/typed-json/typed-json)
[![typed-json Scala version support](https://index.scala-lang.org/frawa/typed-json/typed-json/latest-by-scala-version.svg)](https://index.scala-lang.org/frawa/typed-json/typed-json)

[![Scala Steward badge](https://img.shields.io/badge/Scala_Steward-helping-blue.svg?style=flat&logo=data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAA4AAAAQCAMAAAARSr4IAAAAVFBMVEUAAACHjojlOy5NWlrKzcYRKjGFjIbp293YycuLa3pYY2LSqql4f3pCUFTgSjNodYRmcXUsPD/NTTbjRS+2jomhgnzNc223cGvZS0HaSD0XLjbaSjElhIr+AAAAAXRSTlMAQObYZgAAAHlJREFUCNdNyosOwyAIhWHAQS1Vt7a77/3fcxxdmv0xwmckutAR1nkm4ggbyEcg/wWmlGLDAA3oL50xi6fk5ffZ3E2E3QfZDCcCN2YtbEWZt+Drc6u6rlqv7Uk0LdKqqr5rk2UCRXOk0vmQKGfc94nOJyQjouF9H/wCc9gECEYfONoAAAAASUVORK5CYII=)](https://scala-steward.org)

Validating Json against Json Schema in Scala.


The purpose of this library is twofold:

- Propose a Scala solution to validate Json data against schemas.
- Assist in editing Json data with errors and suggestions, try the [sample web editor](https://frawa.github.io/typed-json).

The library is designed to integrate with any Json parser, example integrations exist for

- [typelevel/jawn](https://github.com/typelevel/jawn)
- [zio-json](https://zio.github.io/zio-json)

Validation will be compliant to specs as covered by [json-schema-org/JSON-Schema-Test-Suite](https://github.com/json-schema-org/JSON-Schema-Test-Suite). For now it supports only `draft-2020-12` with some optional features.