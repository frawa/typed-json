# Backlog

## Doing
- support $schema?
- support $vocabulary?

## TODO
- validating lazy loader, which validates against meta schema on the fly
- better artifact names
- review and refactor: implicits
- crystallize API with an example
- produce output schema from annotations/errors
- post process annotations?
- isolate dependency on zio-json
- latest dependencies
- publish released versions
- do not prefix results, but push value pointer into checker?
- support earlier version of spec, via JsonSchemaTestTestSuite
- support missing formats (all those RFCs ..)
- fix ignored suites in JsonSchemaTestTestSuite
- latest dependencies
- support deprecated?
- benching
- support ScalaNative? Or GraalVM?
- CI on GH Actions?

## Done
- handle ingoredKeywords with annotations
- better package name
- better names, eg. Checks -> Keywords
- streamline SchemaQuality
- support optional/ in JsonSchemaTestTestSuite
- fix: bug in refRemote.json "base URI change - change folder"
- fix: "missing schema" from lazy resolution is dropped on the way up
- support remotes in JsonSchemaTestTestSuite
- JsonSchemaTestTestSuite with ScalaJS
- latest dependencies
- support ScalaJS
- CI, circle CI
- JsonSchemaTestTestSuite data as git submodule?
- support 'unevaluatedProperties'
- support 'unevaluatedItems'
- (sort of) track keywordLocation, absoluteKeywordLocation, and instanceLocation on checks
- review and refactor: tests
- review and refactor: ctor/apply args
- review and refactor: Checks
- consolidate ignored keywords and schema errors, also across lazy
- copyright and license
- review and refactor: SchemaResolver et al
- inline spec meta schemas, via macros at compile time
- test caching of lazy loaded schemas
- validate against 'official' test suite
- record unrecognized keywords
- validate against meta schemas
- dynamicAnchor/dynamicRef
- root schema only has additional "$vocabulary" and "$schema"
- anchor/ref
- multiple files
