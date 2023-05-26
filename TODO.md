# Backlog

## Doing

- editing experience in sample editor
- suggest value in broken state 
- debug: "$anchor": null
- cleanup: remove insideKey from Pointer
- review ignored tests in JawnOffsetParserTest wrt OffsetParserTest
- suggest to find resolved $ref/$dyndref keywords

## TODO

- use free monad, aka trampoline, aka stack-less
- cache compiled function, avoid re-parsing keywords
- add hover (aka lenses?) to sample editor, leverage $comment keyword and annotations
- validate output against schema, for JsonSchemaTestTestSuite
- support missing formats (all those RFCs ..)
- move backlog into issues
- create GitHub releases
- benching
- add hot fixes, for sample editor
- DX: improve discoverability
- standalone samples
- validating lazy loader, which validates against meta schema on the fly
- latest dependencies
- update JSON-Schema-Test-Suite
- support earlier version of spec, via JsonSchemaTestTestSuite
- fix ignored suites in JsonSchemaTestTestSuite
- add scala docs

## Done

- use released version of JSON-Schema-Test-Suite
- produce output schema from annotations/errors
- monadic design for variuous output formats
- Scala 3
- fix propagation of schema changes in sample editor
- smarter suggestions, leverage keywords, formats (and annotations?)
- update JSON-Schema-Test-Suite
- review readme, add more docs
- register with JSON-Schema-Test-Suite
- publish released versions
  - https://docs.scala-lang.org/overviews/contributors/index.html
- try https://github.com/sbt/sbt-projectmatrix instead, to improve Metals in VS Code
- crystallize API with an example
- better package names
- better module names
- better artifact names
- better repo name
- recovering json parser for editing
- improve suggestions
- latest dependencies
- integrate with an interactive tool (to improve suggestions): code mirror next
- review and refactor: implicits
- review naming schema, ubiquitous language from spec
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
