package frawa.typedjson.schema

import java.net.URI
import scala.io.Source
import frawa.typedjson.parser.Parser

object SpecMetaSchemas {

  val draft202012 = URI.create("https://json-schema.org/draft/2020-12/")

  def lazyResolver(implicit parser: Parser): LoadedSchemasResolver.LazyResolver = { uri =>
    if (uri.getSchemeSpecificPart.startsWith(draft202012.getSchemeSpecificPart)) {
      resolve202012(draft202012.relativize(uri))
    } else { None }
  }

  private def resolve202012(relative: URI)(implicit parser: Parser): Option[SchemaValue] = {
    val name = relative.getSchemeSpecificPart()
    val text = Source.fromFile(s"./schemaSpec/${name}.json").getLines.mkString("\n")
    parser
      .parse(text)
      .map(SchemaValue(_))
      .swap
      .map(message => {
        println("error loading spec meta schema", message)
        message
      })
      .swap
      .toOption
  }

}
