# Backlog

## Doing
- review and refactor: implicits

## TODO
- crystallize API with an example
- better artifact names
- produce output schema from annotations/errors
- validating lazy loader, which validates against meta schema on the fly
- improve suggestions
- integrate with an interactive tool (to improve suggestions)
  - code mirror next? 
  - vs code?
- post process annotations?
- latest dependencies
- publish released versions
- do not prefix results, but push value pointer into checker?
- support earlier version of spec, via JsonSchemaTestTestSuite
- support missing formats (all those RFCs ..)
- fix ignored suites in JsonSchemaTestTestSuite
- compile time json parsing vs runtime?
- latest dependencies
- support deprecated?
- Scala 3?!
- benching
- support ScalaNative? Or GraalVM?
- CI on GH Actions?

## Done
- review and cleanup, tests
- isolate dependency on zio-json, and use with tests and examples only
- fix some ignored suites in JsonSchemaTestTestSuite
- support $schema
- support $vocabulary
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
