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
import frawa.typedjson.parser._
import frawa.typedjson.macros.Macros
import UriUtil._

object SpecMetaSchemas {

  val draft202012 = uri("https://json-schema.org/draft/2020-12/")

  def lazyResolver(implicit parser: Parser): LoadedSchemasResolver.LazyResolver = { uri =>
    if (uri.getSchemeSpecificPart.startsWith(draft202012.getSchemeSpecificPart)) {
      resolve202012(draft202012.relativize(uri))
    } else { None }
  }

  private def resolve202012(relative: URI)(implicit parser: Parser): Option[SchemaValue] = {
    val name = relative.getSchemeSpecificPart()
    name match {
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
  }

  import Macros._
  private val schemaContent           = SchemaValue(jsonContent("./schemaSpec/schema.json"))
  private val applicatorContent       = SchemaValue(jsonContent("./schemaSpec/meta/applicator.json"))
  private val contentContent          = SchemaValue(jsonContent("./schemaSpec/meta/content.json"))
  private val coreContent             = SchemaValue(jsonContent("./schemaSpec/meta/core.json"))
  private val formatAnnotationContent = SchemaValue(jsonContent("./schemaSpec/meta/format-annotation.json"))
  private val metaDataContent         = SchemaValue(jsonContent("./schemaSpec/meta/meta-data.json"))
  private val unevaluatedContent      = SchemaValue(jsonContent("./schemaSpec/meta/unevaluated.json"))
  private val validationContent       = SchemaValue(jsonContent("./schemaSpec/meta/validation.json"))
}
