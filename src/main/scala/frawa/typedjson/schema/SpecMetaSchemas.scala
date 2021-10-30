/*
 * Copyright 2021 Frank Wagner
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package frawa.typedjson.schema

import java.net.URI
import scala.io.Source
import frawa.typedjson.parser.Parser
import frawa.typedjson.macros.Macros
object SpecMetaSchemas {

  val draft202012 = URI.create("https://json-schema.org/draft/2020-12/")

  def lazyResolver(implicit parser: Parser): LoadedSchemasResolver.LazyResolver = { uri =>
    if (uri.getSchemeSpecificPart.startsWith(draft202012.getSchemeSpecificPart)) {
      resolve202012(draft202012.relativize(uri))
    } else { None }
  }

  private def resolve202012(relative: URI)(implicit parser: Parser): Option[SchemaValue] = {
    val name = relative.getSchemeSpecificPart()

    val text = name match {
      case "schema"                 => Some(schemaContent)
      case "meta/applicator"        => Some(applicatorContent)
      case "meta/content"           => Some(contentContent)
      case "meta/core"              => Some(coreContent)
      case "meta/format-annotation" => Some(formatAnnotationContent)
      case "meta/meta-data"         => Some(metaDataContent)
      case "meta/unevaluated"       => Some(unevaluatedContent)
      case "meta/validation"        => Some(validationContent)
      case _                        => None
    }

    text.flatMap(
      parser
        .parse(_)
        .map(SchemaValue(_))
        .swap
        .map(message => {
          println("error loading spec meta schema", message)
          message
        })
        .swap
        .toOption
    )
  }

  import Macros._
  private val schemaContent           = fileContent("./schemaSpec/schema.json")
  private val applicatorContent       = fileContent("./schemaSpec/meta/applicator.json")
  private val contentContent          = fileContent("./schemaSpec/meta/content.json")
  private val coreContent             = fileContent("./schemaSpec/meta/core.json")
  private val formatAnnotationContent = fileContent("./schemaSpec/meta/format-annotation.json")
  private val metaDataContent         = fileContent("./schemaSpec/meta/meta-data.json")
  private val unevaluatedContent      = fileContent("./schemaSpec/meta/unevaluated.json")
  private val validationContent       = fileContent("./schemaSpec/meta/validation.json")
}
