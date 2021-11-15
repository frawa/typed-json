# Backlog

## Doing

## TODO
- support ScalaJS
- CI, circle CI, GH actions?
- better names, eg. Checks -> Keywords
- better package name
- cristalize API with an example
- handle ingoredKeywords with annotations?
- review and refactor: implicits
- streamline SchemaQuality
- do not prefix results, but push value pointer into checker
- fully support annotations
- support earlier version of spec, via JsonSchemaTestTestSuite
- JsonSchemaTestTestSuite data as git submodule?
- publish released versions
- support $schema?
- fix ignored suites in JsonSchemaTestTestSuite
- support $vocabulary?
- support deprecated?
- benching

## Done
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
