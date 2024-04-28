package frawa.typedjson.schematestsuite.data

import frawa.inlinefiles.InlineFiles
import frawa.inlinefiles.InlineFiles.*

object Draft202012:

  private val all = inlineDeepTextFiles("./JSON-Schema-Test-Suite/tests/draft2020-12", ".json")

  val files               = all.files()
  val optionalFiles       = all.folder("optional").files()
  val optionalFormatFiles = all.folder("optional/format").files()
  val remotesFiles        = inlineDeepTextFiles("./JSON-Schema-Test-Suite/remotes", ".json")
