package frawa.typedjson.schematestsuite

import frawa.typedjson.schematestsuite.SchemaTestSuite.Config
import frawa.typedjson.parser.Parser

abstract class Draft202012OptionalFormatTestSuite extends SchemaTestSuite:
  protected def suite(config: Config)(using Parser): Unit =
    given Config = config
    suite(data.Draft202012.optionalFormatFiles)
